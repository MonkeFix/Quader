{-# LANGUAGE TemplateHaskell #-}
module Types.State where

import Data.Aeson
import Control.Lens.TH

import Network.WebSockets     qualified as WS
import StmContainers.Map      qualified as STM
import Control.Concurrent.STM qualified as STM

import Types.Auth

data Privacy = Public | Private
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data LobbyState = LobbyState
  { lobbyStateID      :: ID Lobby
  , lobbyStatePlayers :: [ID User]
  , lobbyStateTitle   :: Textual Title
  , lobbyStatePrivate :: Privacy
  }
makeFields ''LobbyState

data Status = Online | Offline

data ClientState = ClientState
  { clientStateConnection :: WS.Connection
  , clientStateStatus     :: Status
  }
makeFields ''ClientState

data Environment = Environment
  { environmentIncLobbyID     :: TVar (ID Lobby)
  , environmentIncMsgID       :: TVar (ID Msg)
  , environmentClientStateMap :: STM.Map (ID User) ClientState
  , environmentLobbyStateMap  :: STM.Map (ID Lobby) LobbyState
  }
makeFields ''Environment
