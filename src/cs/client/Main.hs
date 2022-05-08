module Main where

import Universum
import Network.Socket      (withSocketsDo)

import qualified Network.WebSockets  as WS

main :: IO ()
main = WS.runClient "0.0.0.0" 7892 "/" \conn -> do
  putStrLn @Text "Connected!"
  WS.receive conn >>= print
