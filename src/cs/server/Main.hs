module Main where

import System.Timeout (timeout)
import Data.Aeson
import Control.Lens.TH

import Network.WebSockets     qualified as WS
import Control.Concurrent.STM qualified as STM
import StmContainers.Map      qualified as STM

import Types.State
import App

initEnv :: IO Environment
initEnv = Environment <$> newTVarIO 0 <*> newTVarIO 0 <*> STM.newIO

main :: IO ()
main = initEnv >>= WS.runServer "0.0.0.0" 7892 . app
