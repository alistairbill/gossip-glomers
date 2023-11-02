module Maelstrom.Union
  ( Union (..),
    Member (..),
    Members,
    decompose,
    weaken,
    extract,
    case_,
    on,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import GHC.Records (HasField (..))
import Prelude hiding (on)
import Control.Lens
import Maelstrom

data Union (r :: [Type]) where
  This :: a -> Union (a : r)
  That :: Union r -> Union (any : r)

deriving instance Show (Union '[])

deriving instance (Show a, Show (Union r)) => Show (Union (a : r))

class Member a r where
  inject :: a -> Union r
  project :: Union r -> Maybe a

instance Member a (a : r) where
  inject :: a -> Union (a : r)
  inject = This

  project :: Union (a : r) -> Maybe a
  project = \case
    This x -> Just x
    That _ -> Nothing

instance {-# OVERLAPPABLE #-} (Member a r) => Member a (any : r) where
  inject :: a -> Union (any : r)
  inject = That . inject

  project :: Union (any : r) -> Maybe a
  project = \case
    This _ -> Nothing
    That u -> project u

type family Members as r :: Constraint where
  Members '[] r = ()
  Members (a : as) r = (Member a r, Members as r)

decompose :: Union (a : r) -> Either (Union r) a
decompose = \case
  This a -> Right a
  That u -> Left u

weaken :: Union r -> Union (any : r)
weaken = That

extract :: Union '[a] -> a
extract = \case
  This a -> a
  That u -> case u of {}

case_ :: Message o (Union '[]) -> a
case_ u = case u ^. #body . #payload of {}

on :: (Message o (Union r) -> b) -> (Message o a -> b) -> Message o (Union (a : r)) -> b
on f g = \case
  msg@(Payload (This x)) -> g (msg & #body . #payload .~ x)
  msg@(Payload (That u)) -> f (msg & #body . #payload .~ u)

instance
  {-# OVERLAPPABLE #-}
  (HasField x r a, HasField x (Union rs) a) =>
  HasField x (Union (r : rs)) a
  where
  getField = either (getField @x) (getField @x) . decompose

instance (HasField x r a) => HasField x (Union '[r]) a where
  getField = getField @x . extract

instance (ToJSON a) => ToJSON (Union '[a]) where
  toJSON :: Union '[a] -> Value
  toJSON = toJSON . extract

instance
  {-# OVERLAPPABLE #-}
  (ToJSON a, ToJSON (Union r)) =>
  ToJSON (Union (a : r))
  where
  toJSON :: Union (a : r) -> Value
  toJSON = either toJSON toJSON . decompose

instance (FromJSON a) => FromJSON (Union '[a]) where
  parseJSON :: Value -> Parser (Union '[a])
  parseJSON v = This <$> parseJSON v

instance
  {-# OVERLAPPABLE #-}
  (FromJSON a, FromJSON (Union r)) =>
  FromJSON (Union (a : r))
  where
  parseJSON :: Value -> Parser (Union (a : r))
  parseJSON v = This <$> parseJSON v <|> That <$> parseJSON v
