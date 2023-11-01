import Control.Lens
import Data.Aeson
import Maelstrom
import Maelstrom.Core
import Data.Data (Data)
import Data.Text.Display (display)

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

main :: IO ()
main = do
  manager <- handleInit
  evaluatingStateT manager . forever $ do
    msg <- receive @Generate
    guid >>= reply msg . GenerateOk
