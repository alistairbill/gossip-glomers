{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent (Chan)
import Control.Lens hiding ((.=))
import Control.Monad.State (MonadState (get, put), StateT)
import Data.Aeson
import Data.Text (Text, pack)
import Lib

data Payload = Generate | GenerateOk {guid :: Text} deriving (Show)

instance FromJSON Payload where
  parseJSON = withObject "Generate" $ \o -> do
    t <- o .: "type"
    case t of
      String "generate" -> return Generate
      _ -> fail "Unexpected value for key `type`"

instance ToJSON Payload where
  toJSON (GenerateOk guid) =
    object
      [ "type" .= ("generate_ok" :: Text),
        "id" .= guid
      ]

data UniqueNode = UniqueNode {
  _uNodeId :: Text,
  _uMsgId :: Int
}
makeLenses ''UniqueNode

fromInit :: () -> Init -> Chan (Event Payload ()) -> UniqueNode
fromInit _ init _ = UniqueNode (init ^. nodeId) 1

step :: Event Payload () -> StateT UniqueNode IO ()
step (MessageEvent msg@(Message _ _ (Body _ _ Generate))) = do
  UniqueNode node i <- get
  let guid = node <> "-" <> pack (show i)
  putMessage' uMsgId $ reply msg & body . payload .~ GenerateOk guid

main :: IO ()
main = loop fromInit step ()