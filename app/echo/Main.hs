{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Lib
import Control.Lens hiding ((.=))
import Control.Monad.State (gets, put, StateT)
import Data.Aeson
import Data.Text (Text)
import Control.Concurrent (Chan)

data Payload = Echo Text | EchoOk Text deriving Show
instance FromJSON Payload where
  parseJSON = withObject "Echo" $ \o -> do
    t <- o .: "type"
    case t of
      String "echo" -> Echo <$> o .: "echo"
      _ -> fail "Unexpected value for key `type`"

instance ToJSON Payload where
  toJSON (EchoOk echo) =
    object
      [ "type" .= ("echo_ok" :: Text),
        "echo" .= echo
      ]

newtype EchoNode = EchoNode {
  _eMsgId :: Int
}
makeLenses ''EchoNode

fromInit :: () -> Init -> Chan (Event Payload ()) -> EchoNode
fromInit _ _ _ = EchoNode 1

step :: Event Payload () -> StateT EchoNode IO ()
step (MessageEvent msg@(Message _ _ (Body _ _ (Echo e)))) = do
  putMessage' eMsgId $ reply msg & body . payload .~ EchoOk e

initState :: ()
initState = ()

main :: IO ()
main = do
  loop fromInit step initState