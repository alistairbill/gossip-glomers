{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Maelstrom.LinKv where

import Data.Aeson hiding ((.=))
import Data.Data (Data)
import Maelstrom
import Prelude hiding (Read)
import Data.Generics.Labels ()

newtype Read = Read {key :: Text}
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload Read

newtype ReadOk v = ReadOk {value :: v}
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload (ReadOk v)

data Write v = Write {key :: Text, value :: v}
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload (Write v)

data WriteOk = WriteOk
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload WriteOk

data Cas v = Cas {key :: Text, from :: v, to :: v, createIfNotExists :: Bool}
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload (Cas v)

data CasOk = CasOk
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessagePayload CasOk

linKv :: NodeId
linKv = NodeId "lin-kv"
