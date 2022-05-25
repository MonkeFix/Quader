module Types.Auth where

import Data.Aeson
import Data.Time.Clock (UTCTime)

data User
data Lobby
data Room
data Msg
newtype ID (a :: Type) = ID Word
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Num, Hashable, FromJSON, ToJSON)

data Token
data Title
data Email
newtype Textual (a :: Type) = Textual Text
  deriving stock Generic
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid, IsString)
  deriving anyclass (FromJSON, ToJSON)

data Registration
data Creation
newtype Date (a :: Type) = Date UTCTime
  deriving stock Generic
  deriving newtype (Eq, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

data Role = Role
  { roleID   :: ID Role
  , roleName :: Textual Role
  } deriving stock Generic
    deriving anyclass (FromJSON, ToJSON)

data UserData = UserData
  { userDataID               :: ID User
  , userDataUsername         :: Textual Title
  , userDataEmail            :: Textual Email
  , userDataRegistrationDate :: Date Registration
  , userDataRole             :: Role
  } deriving stock Generic
    deriving anyclass (FromJSON, ToJSON)
