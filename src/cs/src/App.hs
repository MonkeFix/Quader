module App where

import Network.WebSockets     qualified as WS
import StmContainers.Map      qualified as STM
import Control.Concurrent.STM qualified as STM

import Types.State
import Auth
import Data.Aeson
import Types.Messages
import Data.Row
import Data.Row.Variants qualified as Var
import Control.Concurrent.Async (withAsync, race)
import Control.Concurrent (threadDelay)
import Data.Aeson.Types (Parser, parseMaybe)

handleRequest :: Rec Message -> IO ()
handleRequest (Var.view #getRoomList . view #payload -> Just _) = undefined
handleRequest _ = undefined


serve :: Environment -> WS.Connection -> IO ()
serve env conn = do
  WS.receive conn >>= \case
    WS.ControlMessage (WS.Close _ _)   -> WS.sendClose @Text conn "Bye!"
    WS.DataMessage _ _ _ (WS.Text b _) -> case decode b of
      Just msg -> do
        handleRequest msg
        serve env conn
      _ -> WS.sendClose @Text conn "Contract violation"
    _ -> WS.sendClose @Text conn "Contract violation"


withPingThread :: WS.Connection -> Int -> IO () -> IO ()
withPingThread conn delay action = do
  withAsync action \_ -> do ping
  where
    waitDelay = threadDelay $ delay * 10^6

    ping = do
      WS.sendPing @Text conn ""
      race waitDelay waitPong >>= \case
        Left _  -> WS.sendClose @Text conn "No pong" -- when functions returns
        Right _ -> do                                -- async action killed
          waitDelay
          ping

    waitPong = do
      WS.receive conn >>= \case
        WS.ControlMessage (WS.Pong _) -> pass
        _                             -> waitPong


app :: Environment -> WS.ServerApp
app env pending = do
  conn <- WS.acceptRequest pending
  authorize conn >>= \case
    Nothing  -> WS.sendClose @Text conn "Unauthorized"
    Just uid -> withPingThread conn 30 do serve env conn
