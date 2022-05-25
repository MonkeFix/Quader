module App where

import Network.WebSockets     qualified as WS
import StmContainers.Map      qualified as STM
import Control.Concurrent.STM qualified as STM

import Types.State
import Auth
import Data.Aeson
import Types.Messages

serve :: Environment -> WS.Connection -> IO ()
serve env conn = do
  WS.receive conn >>= \case
    WS.ControlMessage (WS.Close _ _)   -> pass
    WS.DataMessage _ _ _ (WS.Text b _) -> case decode b of
      Just (Message mid cmd Request payload) -> case payload of
        CreateRoomData name privacy -> undefined >> serve env conn
        _ -> WS.sendClose @Text conn "Contract violation"
      _        -> WS.sendClose @Text conn "Contract violation"
    _                                  -> WS.sendClose @Text conn "Contract violation"

app :: Environment -> WS.ServerApp
app env pending = do
  conn <- WS.acceptRequest pending
  authorize conn >>= \case
    Nothing  -> WS.sendClose @Text conn "Unauthorized"
    Just uid -> do--WS.withPingThread conn 30 pass do
      atomically do STM.insert (ClientState conn Online) uid (env^.clientStateMap)
      serve env conn
      WS.sendClose @Text conn "Bye!"
