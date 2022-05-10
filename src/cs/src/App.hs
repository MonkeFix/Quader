module App where

import Universum

import Network.WebSockets     qualified as WS
import StmContainers.Map      qualified as STM
import Control.Concurrent.STM qualified as STM

import Types.State
import Auth

serve :: Environment -> WS.Connection -> IO ()
serve env conn = do
  WS.receive conn >>= \case
    WS.ControlMessage (WS.Close _ _)   -> pass
    WS.DataMessage _ _ _ (WS.Text b _) -> do
      undefined
    _                                  -> WS.sendClose @Text conn "Contract violation"

app :: Environment -> WS.ServerApp
app env pending = do
  conn <- WS.acceptRequest pending
  authorize conn >>= \case
    Nothing  -> WS.sendClose @Text conn "Unauthorized"
    Just uid -> WS.withPingThread conn 30 pass do
      atomically do STM.insert (ClientState conn Yes) uid (env^.clientStateMap)
      WS.sendTextData @Text conn "Buber"
      -- serve env conn
      WS.sendClose @Text conn "Bye!"
