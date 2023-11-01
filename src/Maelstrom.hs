{-# LANGUAGE PatternSynonyms #-}

module Maelstrom where

import Control.Concurrent (forkIO, newChan, readChan, writeChan)
import Control.Lens
import Data.Aeson
import Data.Aeson.KeyMap as H
import Data.Aeson.Types (Parser)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Data (Data, dataTypeName, dataTypeOf, fromConstr, toConstr, tyconUQname)
import Data.Generics.Labels ()
import Data.Text.Display (Display)
import Deriving.Aeson
import Deriving.Aeson.Stock (Snake)
import GHC.Generics (Rep)
import Maelstrom.ErrorCode (ErrorCode)
import StateRef
import UnliftIO.Exception (throwString)

data MessageOrigin = Local | Remote

data Message o a = Message
  { src :: NodeId,
    dest :: NodeId,
    body :: Body a
  }
  deriving stock (Generic, Show, Eq)

deriving via
  Snake (Message Local a)
  instance
    (ToJSON a) => ToJSON (Message Local a)

deriving via
  Snake (Message Remote a)
  instance
    (FromJSON a) => FromJSON (Message Remote a)

type OmitSnake = CustomJSON '[FieldLabelModifier CamelToSnake, OmitNothingFields]

data Address = Address
  { msgId :: Maybe MessageId,
    inReplyTo :: Maybe MessageId
  }
  deriving stock (Generic, Show, Eq)
  deriving (ToJSON, FromJSON) via OmitSnake Address

data Body a = Body
  { address :: Address,
    payload :: a
  }
  deriving stock (Generic, Show, Eq)

instance (ToJSON a) => ToJSON (Body a) where
  toJSON (Body address payload) = case (toJSON address, toJSON payload) of
    (Object addressMap, Object payloadMap) -> Object $ H.union addressMap payloadMap
    (_, _) -> error "unreachable"

instance (FromJSON a) => FromJSON (Body a) where
  parseJSON = withObject "Body" $ \o -> do
    address <- parseJSON @Address (Object o)
    case toJSON address of
      Object adr -> do
        let o' = H.difference o adr
        payload <- parseJSON @a (Object o')
        pure $ Body address payload
      _ -> error "unreachable"

newtype MessagePayload a = MessagePayload a
  deriving stock (Generic, Data, Show)

messageType :: forall a s. (Data a, IsString s) => Proxy a -> s
messageType _ =
  dataTypeOf @a (error "unreachable")
    & dataTypeName
    & tyconUQname
    & getStringModifier @CamelToSnake
    & fromString

instance (Generic a, Data a, GToJSON Zero (Rep a), GToEncoding Zero (Rep a)) => ToJSON (MessagePayload a) where
  toJSON :: MessagePayload a -> Value
  toJSON (MessagePayload x) =
    case toJSON @(Snake a) (CustomJSON x) of
      Object keyMap -> Object $ H.union typeMap keyMap
      _ -> Object typeMap
    where
      typeMap = H.singleton "type" (messageType (Proxy @a))

instance
  ( Generic a,
    Data a,
    GFromJSON Zero (Rep a)
  ) =>
  FromJSON (MessagePayload a)
  where
  parseJSON :: Value -> Parser (MessagePayload a)
  parseJSON v =
    MessagePayload <$> do
      let expected = messageType (Proxy @a)
      v & withObject expected \o -> do
        actual <- o .: "type"
        when (expected /= actual) do
          fail $ "Expected `" <> expected <> "`, got `" <> actual <> "`"
        let v' = H.delete "type" o
        if H.null v'
          then pure . fromConstr $ toConstr (error "unreachable" :: a)
          else parseJSON @(Snake a) (Object v') <&> \case CustomJSON x -> x

newtype NodeId = NodeId Text
  deriving stock (Data, Show, Eq, Ord)
  deriving newtype (Display, ToJSON, ToJSONKey, FromJSON, FromJSONKey, Hashable)

newtype MessageId = MessageId Word
  deriving stock (Data, Show, Eq)
  deriving newtype (Display, ToJSON, FromJSON)

pattern Payload :: a -> Message o a
pattern Payload p <- Message _ _ (Body _ p)

{-# COMPLETE Payload :: Message #-}

receive :: forall a m. (HasCallStack, FromJSON a, MonadIO m) => m (Message Remote a)
receive = do
  bytes <- liftIO B.getLine
  either throwString pure $ eitherDecodeStrict bytes

send :: (ToJSON a, MonadMaelstrom m) => NodeId -> a -> m ()
send dest payload =
  sendBody
    dest
    Body
      { address = Address {msgId = Nothing, inReplyTo = Nothing},
        payload
      }

write :: (MonadMaelstrom m, ToJSON a) => Message Local a -> m ()
write = liftIO . BL.putStrLn . encode

sendBody :: (ToJSON a, MonadMaelstrom m) => NodeId -> Body a -> m ()
sendBody dest body = do
  mid <- use #messageId
  src <- use #nodeId
  write $
    Message
      { src,
        dest,
        body = body & #address . #msgId ?~ mid
      }
  #messageId %= \(MessageId i) -> MessageId (i + 1)

reply :: (ToJSON r, MonadMaelstrom m) => Message Remote a -> r -> m ()
reply req res =
  sendBody
    (req ^. #src)
    Body
      { address =
          Address
            { msgId = Nothing,
              inReplyTo = req ^. #body . #address . #msgId
            },
        payload = res
      }

data Init = Init
  { nodeId :: NodeId,
    nodeIds :: [NodeId]
  }
  deriving stock (Generic, Data, Show)
  deriving (FromJSON) via MessagePayload Init

data InitOk = InitOk
  deriving stock (Generic, Data, Show)
  deriving (ToJSON) via MessagePayload InitOk

handleInit :: IO Manager
handleInit = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  req <- receive @Init
  let manager =
        Manager
          { messageId = MessageId 0,
            nodeId = req ^. #body . #payload . #nodeId,
            nodeIds = req ^. #body . #payload . #nodeIds
          }
  execStateT (reply req InitOk) manager

data Error = Error
  { inReplyTo :: MessageId,
    code :: ErrorCode,
    text :: Maybe Text
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload Error

data Manager = Manager
  { messageId :: MessageId,
    nodeId :: NodeId,
    nodeIds :: [NodeId]
  }
  deriving stock (Generic)

type MonadMaelstrom m = (MonadState Manager m, MonadIO m)

-- TODO: Use the concurrent-state library instead
loop :: (FromJSON a) => (Manager -> IO (IORef ctx)) -> (Message Remote a -> StateRefT ctx IO ()) -> IO ()
loop newContext handle = do
  manager <- handleInit
  context <- newContext manager
  let runContext = flip runStateRefT context
  chan <- newChan
  _ <- forkIO . forever $ do
    msg <- receive
    writeChan chan msg
  runContext . forever $ liftIO (readChan chan) >>= handle