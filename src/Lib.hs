{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
  ( Event (..),
    Body (..),
    Init,
    loop,
    Message (..),
    putMessage,
    putMessage',
    body,
    src,
    dst,
    payload,
    reply,
    nodeId,
    nodeIds,
  )
where

import Control.Concurrent (Chan, forkIO, newChan, readChan, writeChan)
import Control.Lens hiding ((.=))
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Lens (atKey)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import System.IO (BufferMode (NoBuffering), hSetBuffering, isEOF, stdin, stdout)

data Body payload = Body
  { _msgId :: Maybe Int,
    _inReplyTo :: Maybe Int,
    _payload :: payload
  }
  deriving (Show)

makeLenses ''Body

data Message payload = Message
  { _src :: Text,
    _dst :: Text,
    _body :: Body payload
  }
  deriving (Show)

makeLenses ''Message

instance (FromJSON payload) => FromJSON (Message payload) where
  parseJSON = withObject "Message" $ \o -> do
    _src <- o .: "src"
    _dst <- o .: "dest"
    _body <- o .: "body"
    return Message {..}

instance (ToJSON payload) => ToJSON (Message payload) where
  toJSON Message {..} =
    object
      [ "src" .= _src,
        "dest" .= _dst,
        "body" .= _body
      ]

instance (ToJSON payload) => ToJSON (Body payload) where
  toJSON Body {..} =
    let setKey key val = atKey key ?~ toJSON val
     in toJSON _payload
          & maybe id (setKey "msg_id") _msgId
          & maybe id (setKey "in_reply_to") _inReplyTo

instance (FromJSON payload) => FromJSON (Body payload) where
  parseJSON = withObject "Body" $ \o -> do
    _msgId <- o .:? "msg_id"
    _inReplyTo <- o .:? "in_reply_to"
    _payload <- parseJSON (Object o)
    return Body {..}

data Init = Init
  { _nodeId :: Text,
    _nodeIds :: [Text]
  }
  deriving (Show)

makeLenses ''Init

data InitOk = InitOk
  deriving (Show)

instance FromJSON Init where
  parseJSON = withObject "Init" $ \o -> do
    _nodeId <- o .: "node_id"
    _nodeIds <- o .: "node_ids"
    return Init {..}

instance ToJSON InitOk where
  toJSON InitOk = object ["type" .= ("init_ok" :: Text)]

data Event p ip = MessageEvent (Message p) | Injected ip | EOF

reply :: Message payload -> Message payload
reply msg =
  msg
    & src .~ (msg ^. dst)
    & dst .~ (msg ^. src)
    & body . inReplyTo .~ msg ^. body . msgId

getMessage :: (MonadIO m, FromJSON payload) => m (Message payload)
getMessage = liftIO B.getLine >>= either error return . eitherDecodeStrict

putMessage :: (MonadIO m, ToJSON payload) => Message payload -> m ()
putMessage msg = do
  liftIO . B.putStrLn . toStrict . encode $ msg

putMessage' :: (MonadState a m, MonadIO m, ToJSON payload) => Lens' a Int -> Message payload -> m ()
putMessage' nodeMsgId msg = do
  i <- use nodeMsgId
  putMessage $ msg & body . msgId ?~ i
  nodeMsgId += 1

parseThread :: (FromJSON p) => Chan (Event p ip) -> IO ()
parseThread chan = do
  eof <- isEOF
  if eof
    then writeChan chan EOF
    else do
      msg <- getMessage
      writeChan chan $ MessageEvent msg
      parseThread chan

loop :: (FromJSON p) => (s -> Init -> Chan (Event p ip) -> IO a) -> (Event p ip -> StateT a IO ()) -> s -> IO ()
loop fromInit step initState = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  initMsg <- getMessage
  putMessage $ reply initMsg & body . msgId ?~ 0 & body . payload .~ InitOk
  chan <- newChan
  forkIO $ parseThread chan
  initNode <- fromInit initState (initMsg ^. body . payload) chan
  go chan initNode
  where
    go chan node = do
      input <- readChan chan
      node' <- execStateT (step input) node
      go chan node'
