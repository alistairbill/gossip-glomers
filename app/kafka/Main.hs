import Control.Lens hiding (Context)
import Data.Aeson
import Data.Data (Data)
import Data.Generics.Labels ()
import Data.Map qualified as M
import Data.Sequence qualified as S
import Maelstrom
import Maelstrom.Union (Union, case_, on)
import StateRef (StateRefT)
import Prelude hiding (Read, on)

newtype LogKey = LogKey Text
  deriving stock (Data, Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype LogOffset = LogOffset Int
  deriving stock (Data, Show, Eq, Ord)
  deriving newtype (ToJSON, FromJSON)

data Send = Send
  { key :: LogKey,
    msg :: Int
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload Send

newtype SendOk = SendOk {offset :: LogOffset}
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload SendOk

newtype Poll = Poll {offsets :: M.Map LogKey LogOffset}
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload Poll

newtype PollOk = PollOk {msgs :: M.Map LogKey (S.Seq (LogOffset, Int))}
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload PollOk

newtype CommitOffsets = CommitOffsets {offsets :: M.Map LogKey LogOffset}
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload CommitOffsets

data CommitOffsetsOk = CommitOffsetsOk
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload CommitOffsetsOk

newtype ListCommittedOffsets = ListCommittedOffsets {keys :: Set LogKey}
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload ListCommittedOffsets

newtype ListCommittedOffsetsOk = ListCommittedOffsetsOk {offsets :: M.Map LogKey LogOffset}
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload ListCommittedOffsetsOk

data Context = Context
  { manager :: Manager,
    logs :: M.Map LogKey (S.Seq (LogOffset, Int)),
    commits :: M.Map LogKey LogOffset,
    latest :: M.Map LogKey LogOffset
  }
  deriving stock (Generic)

type ContextM a = StateRefT Context IO a

inc :: LogOffset -> LogOffset
inc (LogOffset i) = LogOffset (i + 1)

handleSend :: Message Remote Send -> ContextM ()
handleSend msg@(Payload (Send key log)) = do
  offset <- inc . M.findWithDefault (LogOffset 0) key <$> use #latest
  zoom #manager $ reply msg (SendOk offset)
  #latest %= M.insert key offset
  #logs %= M.insertWith (flip (<>)) key (S.singleton (offset, log))

latest :: LogOffset -> S.Seq (LogOffset, Int) -> S.Seq (LogOffset, Int)
latest offset = S.dropWhileL ((< offset) . fst)

handlePoll :: Message Remote Poll -> ContextM ()
handlePoll msg@(Payload (Poll offsets)) =
  use #logs
    >>= zoom #manager
      . reply msg
      . PollOk
      . M.mapMaybeWithKey (\key l -> latest <$> M.lookup key offsets <*> pure l)

handleCommit :: Message Remote CommitOffsets -> ContextM ()
handleCommit msg@(Payload (CommitOffsets offsets)) = do
  #commits %= M.unionWith max offsets
  zoom #manager $ reply msg CommitOffsetsOk

handleList :: Message Remote ListCommittedOffsets -> ContextM ()
handleList msg@(Payload (ListCommittedOffsets keys)) =
  M.restrictKeys <$> use #commits <*> pure keys
    >>= zoom #manager . reply msg . ListCommittedOffsetsOk

handler :: Message 'Remote (Union '[ListCommittedOffsets, CommitOffsets, Poll, Send]) -> ContextM ()
handler =
  case_
    `on` handleSend
    `on` handlePoll
    `on` handleCommit
    `on` handleList

newContext :: Manager -> IO (IORef Context)
newContext manager = newIORef Context {manager, logs = mempty, commits = mempty, latest = mempty}

main :: IO ()
main = loop newContext handler