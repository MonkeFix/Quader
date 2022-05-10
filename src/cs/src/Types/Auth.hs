module Types.Auth where

import Universum
import Data.Aeson

data User
data Lobby
newtype ID (a :: Type) = ID Word
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Hashable, FromJSON, ToJSON)

data Token
data Title
newtype Textual (a :: Type) = Textual Text
  deriving stock Generic
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid, IsString)
  deriving anyclass (FromJSON, ToJSON)
