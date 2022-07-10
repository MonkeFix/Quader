{-# LANGUAGE TemplateHaskell #-}
module Types.State where

import Data.Aeson
import Control.Lens.TH

import Network.WebSockets     qualified as WS
import StmContainers.Map      qualified as STM
import Control.Concurrent.STM qualified as STM

import Types.Auth
import Deriving.Aeson
import Data.Char

data ToLower

instance StringModifier ToLower where
  getStringModifier (x:xs) = toLower x : xs
  getStringModifier "" = ""

type SumCamel = CustomJSON '[SumUntaggedValue, ConstructorTagModifier ToLower]

data Privacy = Public | Private
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via SumCamel Privacy

data LobbyState = LobbyState
  { lobbyStateID      :: ID Lobby
  , lobbyStatePlayers :: [ID User]
  , lobbyStateTitle   :: Textual Title
  , lobbyStatePrivate :: Privacy
  }
makeFields ''LobbyState

data Environment = Environment
  { environmentIncLobbyID     :: TVar (ID Lobby)
  , environmentIncMsgID       :: TVar (ID Msg)
  , environmentLobbyStateMap  :: STM.Map (ID Lobby) LobbyState
  }
makeFields ''Environment
