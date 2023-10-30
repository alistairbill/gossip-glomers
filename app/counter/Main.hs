{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent (Chan, forkIO, threadDelay, writeChan)
import Control.Lens hiding ((.=))
import Control.Monad (forM_, when)
import Control.Monad.State (MonadIO (liftIO), MonadState, StateT, forever, guard)
import Data.Aeson
import qualified Data.Map as M
import Data.Text (Text)
import Debug.Trace (trace)
import Lib

data Versioned a = Versioned
  { _version :: Int,
    _value :: a
  }
  deriving (Show, Eq)

instance (Eq a) => Ord (Versioned a) where
  compare (Versioned v1 _) (Versioned v2 _) = compare v1 v2

instance (FromJSON a) => FromJSON (Versioned a) where
  parseJSON = withObject "VersionedValue" $ \o ->
    Versioned <$> o .: "version" <*> o .: "value"

instance (ToJSON a) => ToJSON (Versioned a) where
  toJSON (Versioned version value) = object ["version" .= version, "value" .= value]

data InjectedPayload = IGossip

data Payload
  = Add Int
  | AddOk
  | Read
  | ReadOk Int
  | Gossip (M.Map Text (Versioned Int))
  deriving (Show)

instance FromJSON Payload where
  parseJSON = withObject "Counter" $ \o -> do
    t <- o .: "type"
    case t of
      String "add" -> Add <$> o .: "delta"
      String "read" -> pure Read
      String "gossip" -> Gossip <$> o .: "values"
      _ -> fail "Unexpected value for key `type`"

instance ToJSON Payload where
  toJSON AddOk = object ["type" .= ("add_ok" :: Text)]
  toJSON (ReadOk value) =
    object
      [ "type" .= ("read_ok" :: Text),
        "value" .= value
      ]
  toJSON (Gossip values) =
    object
      [ "type" .= ("gossip" :: Text),
        "values" .= values
      ]

newtype CounterNode = CounterNode {_state :: M.Map Text (Versioned Int)}

makeLenses ''CounterNode

gossipInterval :: Int
gossipInterval = 1000 * 1000 -- 1000 ms

fromInit :: s -> Init -> Chan (Event Payload InjectedPayload) -> IO CounterNode
fromInit _ init chan = do
  forkIO . forever $ do
    threadDelay gossipInterval
    writeChan chan $ Injected IGossip
  return . CounterNode $ M.singleton (init ^. initNodeId) (Versioned 0 0)

increment :: Int -> Versioned Int -> Versioned Int
increment delta (Versioned version value) = Versioned (version + 1) (value + delta)

step :: Event Payload InjectedPayload -> StateT (Node CounterNode) IO ()
step (Injected IGossip) = do
  nodeId' <- use nodeId
  nodeIds' <- use nodeIds
  state' <- use (other . state)
  forM_ nodeIds' $ \n -> do
    when (n /= nodeId') $
      putMessage
        Message
          { _src = nodeId',
            _dst = n,
            _body =
              Body
                { _msgId = Nothing,
                  _inReplyTo = Nothing,
                  _payload = Gossip state'
                }
          }
step (MessageEvent msg) = do
  let res = reply msg
   in case res ^. body . payload of
        Add delta -> do
          nodeId' <- use nodeId
          other . state %= M.adjust (increment delta) nodeId'
          putMessage' $ res & body . payload .~ AddOk
        Read -> do
          state' <- use (other . state)
          let val = M.foldr (\(Versioned _ value) acc -> acc + value) 0 state'
          putMessage' $ res & body . payload .~ ReadOk val
        Gossip values ->
          other . state %= M.unionWith max values

initState :: ()
initState = ()

main :: IO ()
main = loop fromInit step initState