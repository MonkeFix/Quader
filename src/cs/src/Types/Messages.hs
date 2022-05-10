module Types.Messages where

import Universum
import Data.Aeson

import Types.Auth
import Types.State

data TypeOfMessage = Request | Response
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Message a = Message
  { messageAction  :: Action
  , messageType    :: TypeOfMessage
  , messagePayload :: a
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype Authorize = Authorize
  { authorizeToken :: Textual Token
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype CreateLobby = CreateLobby
  { createLobbyInvite :: [ID User]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data InviteLobby = InviteLobby
  { inviteLobbyLobbyId :: ID Lobby
  , inviteLobbyPlayers :: [ID User]
  , inviteLobbyTitle   :: Textual Title
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)
