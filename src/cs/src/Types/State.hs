{-# LANGUAGE TemplateHaskell #-}
module Types.State where

import Universum
import Data.Aeson
import Control.Lens.TH

import Network.WebSockets     qualified as WS
import StmContainers.Map      qualified as STM
import Control.Concurrent.STM qualified as STM

import Types.Auth

data Action = Introduction | Move | Invite
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

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

data Online = Yes | No

data ClientState = ClientState
  { clientStateConnection :: WS.Connection
  , clientStateTitle      :: Online
  }
makeFields ''ClientState

data Environment = Environment
  { environmentIncLobbyID     :: TVar (ID Lobby)
  , environmentClientStateMap :: STM.Map (ID User) ClientState
  , environmentLobbyStateMap  :: STM.Map (ID Lobby) LobbyState
  }
makeFields ''Environment
