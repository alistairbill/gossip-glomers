{-# LANGUAGE OverloadedStrings #-}

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

data UniqueNode = UniqueNode

fromInit :: () -> Init -> Chan (Event Payload ()) -> IO UniqueNode
fromInit _ _ _ = pure UniqueNode

step :: Event Payload () -> StateT (Node UniqueNode) IO ()
step (MessageEvent msg@(Message _ _ (Body _ _ Generate))) = do
  nodeId' <- use nodeId
  nodeMsgId' <- use nodeMsgId
  let guid = nodeId' <> "-" <> pack (show nodeMsgId')
  putMessage' $ reply msg & body . payload .~ GenerateOk guid

main :: IO ()
main = loop fromInit step ()