module Types.Messages where

import Data.Aeson

import Types.Auth
import Types.State

data TypeOfMessage = Request | Response
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Command = GetRoomList
             | CreateRoom
             | JoinRoom
             | LeaveRoom
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CreateRoomData = CreateRoomData
  { createRoomDataName :: Textual Title
  , createRoomDataType :: Privacy
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype CreateRoomPayload = CreateRoomPayload
  { createRoomPayloadId :: ID Room
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data RoomPayload = RoomPayload
  { roomPayloadId        :: ID Room
  , roomPayloadName      :: Textual Title
  , roomPayloadCreatedBy :: ID User
  , roomPayloadCreatedAt :: Date Creation
  , roomType             :: Privacy
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data Message a = Message
  { messageId      :: ID Msg
  , messageAction  :: Command
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
