{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Maelstrom where

import Control.Concurrent (Chan, forkIO, newChan, readChan, writeChan)
import Control.Lens
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Data (Data)
import Maelstrom.Core
import Maelstrom.ErrorCode (ErrorCode)
import UnliftIO.Exception (throwString)

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
write msg = do
  chan <- use #chan
  liftIO . writeChan chan . encode $ msg

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
  chan <- newChan
  _ <- forkIO . forever $ readChan chan >>= BL.putStrLn
  let manager =
        Manager
          { messageId = MessageId 0,
            chan = chan,
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
    chan :: Chan BL.ByteString,
    nodeId :: NodeId,
    nodeIds :: [NodeId]
  }
  deriving stock (Generic)

type MonadMaelstrom m = (MonadState Manager m, MonadIO m)
