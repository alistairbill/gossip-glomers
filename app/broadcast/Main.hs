{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

import Control.Concurrent (Chan)
import Control.Lens hiding ((.=))
import Control.Monad.State (StateT, modify)
import Data.Aeson
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import Lib

data Payload = Broadcast Int | BroadcastOk | Read | ReadOk (S.Set Int) | Topology (M.Map Text [Text]) | TopologyOk

instance FromJSON Payload where
  parseJSON = withObject "Broadcast" $ \o -> do
    t <- o .: "type"
    case t of
      String "broadcast" -> Broadcast <$> o .: "message"
      String "read" -> return Read
      String "topology" -> Topology <$> o .: "topology"
      _ -> fail "Unexpected value for key `type`"

instance ToJSON Payload where
  toJSON BroadcastOk = object ["type" .= ("broadcast_ok" :: Text)]
  toJSON (ReadOk vals) = object ["type" .= ("read_ok" :: Text), "messages" .= S.toList vals]
  toJSON TopologyOk = object ["type" .= ("topology_ok" :: Text)]

data BroadcastNode = BroadcastNode
  { _bNodeId :: Text,
    _bMsgId :: Int,
    _messages :: S.Set Int,
    _known :: M.Map Text (S.Set Int),
    _neighbourhood :: [Text]
  }

makeLenses ''BroadcastNode

fromInit :: () -> Init -> Chan (Event Payload ()) -> BroadcastNode
fromInit _ init _ =
  let known = M.fromList . map (,S.empty) $ init ^. nodeIds
   in BroadcastNode
        { _bNodeId = init ^. nodeId,
          _bMsgId = 1,
          _messages = S.empty,
          _known = known,
          _neighbourhood = []
        }


step :: Event Payload () -> StateT BroadcastNode IO ()
step EOF = return ()
step (MessageEvent msg) =
  let res = reply msg
   in case res ^. body . payload of
        Broadcast b -> do
          messages %= S.insert b
          putMessage' bMsgId $ res & body . payload .~ BroadcastOk
        Read -> do
          seen <- use messages
          putMessage' bMsgId $ res & body . payload .~ ReadOk seen
        Topology _ -> do
          putMessage' bMsgId $ res & body . payload .~ TopologyOk
        BroadcastOk -> return ()
        ReadOk _ -> return ()
        TopologyOk -> return ()

main :: IO ()
main = loop fromInit step ()
