import Control.Concurrent (forkIO, threadDelay)
import Control.Lens hiding (Context)
import Data.Aeson
import Data.Data (Data)
import Data.Map qualified as M
import Data.Set qualified as S
import Maelstrom
import Maelstrom.Core
import Maelstrom.Union
import StateRef (StateRefT, runStateRefT)
import System.Random (randomRIO)
import Prelude hiding (Read, on)

newtype Broadcast = Broadcast
  { message :: Int
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload Broadcast

data BroadcastOk = BroadcastOk
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload BroadcastOk

data Read = Read
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload Read

newtype ReadOk = ReadOk
  { messages :: S.Set Int
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload ReadOk

newtype Topology = Topology
  { topology :: M.Map NodeId [NodeId]
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload Topology

data TopologyOk = TopologyOk
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload TopologyOk

newtype Gossip = Gossip
  { seen :: S.Set Int
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload Gossip

data Context = Context
  { messages :: S.Set Int,
    known :: M.Map NodeId (S.Set Int),
    neighbourhood :: [NodeId],
    manager :: Manager
  }
  deriving stock (Generic)

type ContextM a = StateRefT Context IO a

gossipInterval :: Int
gossipInterval = 200 * 1000 -- 200 ms

handleGossip :: Message Remote Gossip -> ContextM ()
handleGossip msg@(Payload (Gossip seen)) = do
  #known %= M.adjust (S.union seen) (msg ^. #src)
  #messages %= S.union seen

handleBroadcast :: Message Remote Broadcast -> ContextM ()
handleBroadcast msg@(Payload (Broadcast message)) = do
  modifying #messages (S.insert message)
  zoom #manager $ reply msg BroadcastOk

handleRead :: Message Remote Read -> ContextM ()
handleRead msg@(Payload Read) = do
  messages <- use #messages
  zoom #manager . reply msg $ ReadOk messages

handleTopology :: Message Remote Topology -> ContextM ()
handleTopology msg@(Payload (Topology topology)) = do
  nodeId <- use (#manager . #nodeId)
  assign #neighbourhood (topology M.! nodeId)
  zoom #manager $ reply msg TopologyOk

handler :: Message 'Remote (Union [Gossip, Topology, Read, Broadcast]) -> ContextM ()
handler =
  case_
    `on` handleBroadcast
    `on` handleRead
    `on` handleTopology
    `on` handleGossip

randomSample :: (Ord a) => Int -> S.Set a -> IO (S.Set a)
randomSample k s = S.fromList <$> replicateM k ((`S.elemAt` s) <$> randomRIO (0, S.size s - 1))

gossipOverhead :: Int
gossipOverhead = 10 -- percent of already known values to include in gossip

sendGossip :: ContextM ()
sendGossip = do
  neighbourhood <- use #neighbourhood
  forM_ neighbourhood $ \n -> do
    known <- use #known
    messages <- use #messages
    let knowntoN = known M.! n
        (alreadyKnown, notifyOf) = S.partition (`S.member` knowntoN) messages
        k = (gossipOverhead * S.size notifyOf) `div` 100
    extra <- liftIO $ randomSample k alreadyKnown
    zoom #manager . send n . Gossip $ notifyOf `S.union` extra

newContext :: Manager -> IO (IORef Context)
newContext manager = do
  let known = M.fromList . map (,S.empty) $ manager ^. #nodeIds
  let context = Context {messages = S.empty, known, neighbourhood = [], manager}
  ref <- newIORef context
  _ <- forkIO . flip runStateRefT ref . forever $ do
    liftIO $ threadDelay gossipInterval
    sendGossip
  pure ref

main :: IO ()
main = loop newContext handler
