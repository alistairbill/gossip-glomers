{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

import Control.Concurrent (Chan, forkIO, threadDelay, writeChan)
import Control.Lens hiding ((.=))
import Control.Monad.State (MonadIO (liftIO), StateT, forM_, forever, modify, replicateM)
import Data.Aeson
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import Lib
import System.Random (randomRIO)

data Payload = Broadcast Int | BroadcastOk | Read | ReadOk (S.Set Int) | Topology (M.Map Text [Text]) | TopologyOk | Gossip (S.Set Int)

data InjectedPayload = IGossip

instance FromJSON Payload where
  parseJSON = withObject "Broadcast" $ \o -> do
    t <- o .: "type"
    case t of
      String "broadcast" -> Broadcast <$> o .: "message"
      String "read" -> return Read
      String "topology" -> Topology <$> o .: "topology"
      String "gossip" -> Gossip <$> o .: "seen"
      _ -> fail "Unexpected value for key `type`"

instance ToJSON Payload where
  toJSON BroadcastOk = object ["type" .= ("broadcast_ok" :: Text)]
  toJSON (ReadOk vals) = object ["type" .= ("read_ok" :: Text), "messages" .= vals]
  toJSON TopologyOk = object ["type" .= ("topology_ok" :: Text)]
  toJSON (Gossip seen) = object ["type" .= ("gossip" :: Text), "seen" .= seen]

data BroadcastNode = BroadcastNode
  { _messages :: S.Set Int,
    _known :: M.Map Text (S.Set Int),
    _neighbourhood :: [Text]
  }

makeLenses ''BroadcastNode

gossipInterval :: Int
gossipInterval = 200 * 1000 -- 200 ms

fromInit :: () -> Init -> Chan (Event Payload InjectedPayload) -> IO BroadcastNode
fromInit _ init chan =
  let known' = M.fromList . map (,S.empty) $ init ^. initNodeIds
   in do
        forkIO . forever $ do
          threadDelay gossipInterval
          writeChan chan $ Injected IGossip
        return
          BroadcastNode
            { _messages = S.empty,
              _known = known',
              _neighbourhood = []
            }

randomSample :: (Ord a) => Int -> S.Set a -> IO (S.Set a)
randomSample k s = S.fromList <$> replicateM k ((`S.elemAt` s) <$> randomRIO (0, S.size s - 1))

gossipOverhead :: Int
gossipOverhead = 10 -- percent of values already known included in gossip message

extraNeighbours :: Int
extraNeighbours = 40 -- percent of other nodes to include as extra neighbours

step :: Event Payload InjectedPayload -> StateT (Node BroadcastNode) IO ()
step EOF = return ()
step (Injected IGossip) = do
  nodeId' <- use nodeId
  neighbours <- use (other . neighbourhood)
  known' <- use (other . known)
  messages' <- use (other . messages)
  forM_ neighbours $ \n -> do
    let knowntoN = known' M.! n
        (alreadyKnown, notifyOf) = S.partition (`S.member` knowntoN) messages'
        k = (gossipOverhead * S.size notifyOf) `div` 100
    extra <- liftIO $ randomSample k alreadyKnown
    let notifyOf' = notifyOf `S.union` extra
    putMessage
      Message
        { _src = nodeId',
          _dst = n,
          _body =
            Body
              { _msgId = Nothing,
                _inReplyTo = Nothing,
                _payload = Gossip notifyOf
              }
        }
step (MessageEvent msg) =
  let res = reply msg
   in case res ^. body . payload of
        Gossip seen -> do
          other . known %= M.adjust (S.union seen) (res ^. dst)
          other . messages %= S.union seen
        Broadcast b -> do
          other . messages %= S.insert b
          putMessage' $ res & body . payload .~ BroadcastOk
        Read -> do
          messages' <- use (other . messages)
          putMessage' $ res & body . payload .~ ReadOk messages'
        Topology topology -> do
          nodeId' <- use nodeId
          known' <- use (other . known)
          others <- liftIO $ randomSample ((extraNeighbours * M.size known') `div` 100) (M.keysSet known')
          assign (other . neighbourhood) (S.toList $ S.fromList (topology M.! nodeId') `S.union` others)
          putMessage' $ res & body . payload .~ TopologyOk
        BroadcastOk -> return ()
        ReadOk _ -> return ()
        TopologyOk -> return ()

main :: IO ()
main = loop fromInit step ()
