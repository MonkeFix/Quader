module Types.Messages where

import Data.Aeson

import Types.Auth
import Data.Row
import Types.State
import Deriving.Aeson

data TypeOfMessage = Request | Response
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via SumCamel TypeOfMessage

data Command = GetRoomList
             | CreateRoom
             | JoinRoom
             | LeaveRoom
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON) via SumCamel Command

type family RequestData (cmd :: Command) where
  RequestData GetRoomList = Empty
  RequestData CreateRoom = "name" .== Textual Title
                        .+ "type" .== Privacy


type family Payload (cmd :: Command) where
  Payload CreateRoom = "id" .== ID Room
  Payload GetRoomList = "id"        .== ID Room
                     .+ "name"      .== Textual Title
                     .+ "createdBy" .== ID User
                     .+ "createdAt" .== Date Creation
                     .+ "type"      .== Privacy

type MetaData = "id"     .== Text
             .+ "action" .== Command
             .+ "type"   .== TypeOfMessage

type Data = "getRoomList" .== Rec (RequestData 'GetRoomList)
         .+ "createRoom"  .== Rec (RequestData 'CreateRoom)

type Message = MetaData .+ "payload" .== Var Data

type Authorize = "token" .== Textual Token
