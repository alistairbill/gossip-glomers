import Control.Concurrent (forkIO, threadDelay)
import Control.Lens hiding (Context)
import Data.Aeson
import Data.Data (Data)
import Data.Map qualified as M
import Deriving.Aeson
import Deriving.Aeson.Stock (Snake)
import Maelstrom
import Maelstrom.Core
import Maelstrom.Union
import StateRef (StateRefT, runStateRefT)
import Prelude hiding (Read, on)

data Versioned a = Versioned
  { version :: Int,
    value :: a
  }
  deriving (Generic, Data, Show, Eq)

deriving via
  Snake (Versioned a)
  instance
    (FromJSON a) => FromJSON (Versioned a)

deriving via
  Snake (Versioned a)
  instance
    (ToJSON a) => ToJSON (Versioned a)

instance (Eq a) => Ord (Versioned a) where
  compare (Versioned v1 _) (Versioned v2 _) = compare v1 v2

newtype Add = Add
  { delta :: Int
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload Add

data AddOk = AddOk
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload AddOk

data Read = Read
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload Read

newtype ReadOk = ReadOk
  { value :: Int
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload ReadOk

newtype Gossip = Gossip
  { values :: M.Map NodeId (Versioned Int)
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload Gossip

data Context = Context
  { manager :: Manager,
    values :: M.Map NodeId (Versioned Int)
  }
  deriving stock (Generic)

type ContextM a = StateRefT Context IO a

gossipInterval :: Int
gossipInterval = 1000 * 1000 -- 1000 ms

increment :: Int -> Versioned Int -> Versioned Int
increment delta (Versioned version value) = Versioned (version + 1) (value + delta)

handleAdd :: Message 'Remote Add -> ContextM ()
handleAdd msg@(Payload (Add delta)) = do
  nodeId <- use (#manager . #nodeId)
  #values %= M.adjust (increment delta) nodeId
  zoom #manager $ reply msg AddOk

handleRead :: Message 'Remote Read -> ContextM ()
handleRead msg@(Payload Read) = do
  values <- use #values
  let total = M.foldr (\(Versioned _ value) acc -> acc + value) 0 values
  zoom #manager . reply msg $ ReadOk total

handleGossip :: Message 'Remote Gossip -> ContextM ()
handleGossip (Payload (Gossip values)) = do
  #values %= M.unionWith max values

handler :: Message 'Remote (Union [Gossip, Read, Add]) -> ContextM ()
handler =
  case_
    `on` handleAdd
    `on` handleRead
    `on` handleGossip

newContext :: Manager -> IO (IORef Context)
newContext manager = do
  let context = Context {manager, values = M.singleton (manager ^. #nodeId) (Versioned 0 0)}
  ref <- newIORef context
  _ <- forkIO . flip runStateRefT ref . forever $ do
    liftIO $ threadDelay gossipInterval
    sendGossip
  pure ref

sendGossip :: ContextM ()
sendGossip = do
  nodeId <- use (#manager . #nodeId)
  nodeIds <- use (#manager . #nodeIds)
  values <- use #values
  forM_ nodeIds $ \n ->
    when (n /= nodeId) $ do
      zoom #manager $ send n (Gossip values)

main :: IO ()
main = loop newContext handler
