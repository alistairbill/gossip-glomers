import Control.Lens
import Data.Aeson
import Data.Data (Data)
import Maelstrom
import Maelstrom.Core

newtype Echo = Echo
  { echo :: Text
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload Echo

newtype EchoOk = EchoOk
  { echo :: Text
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload EchoOk

handler :: (MonadState Manager m, MonadIO m) => Message 'Remote Echo -> m ()
handler msg = reply msg $ EchoOk (msg ^. #body . #payload . #echo)

main :: IO ()
main = loop newIORef handler