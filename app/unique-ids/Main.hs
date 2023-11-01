import Control.Lens
import Data.Aeson
import Data.Data (Data)
import Data.Text.Display (display)
import Maelstrom

data Generate = Generate
  deriving stock (Generic, Data, Show)
  deriving (FromJSON) via MessagePayload Generate

newtype GenerateOk = GenerateOk {id :: Text}
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload GenerateOk

guid :: (MonadMaelstrom m) => m Text
guid = do
  nodeId <- use #nodeId
  messageId <- use #messageId
  pure $ display nodeId <> "-" <> display messageId

handler :: (MonadState Manager m, MonadIO m) => Message 'Remote Generate -> m ()
handler msg = guid >>= reply msg . GenerateOk

main :: IO ()
main = loop newIORef handler
