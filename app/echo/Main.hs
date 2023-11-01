import Control.Lens
import Data.Aeson
import Data.Data (Data)
import Maelstrom
import Maelstrom.Core

newtype Echo = Echo {
  echo :: Text
}
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload Echo

newtype EchoOk = EchoOk {
  echo :: Text
}
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload EchoOk

main :: IO ()
main = do
  manager <- handleInit
  evaluatingStateT manager . forever $ do
    msg <- receive @Echo
    reply msg $ EchoOk (msg ^. #body . #payload . #echo)