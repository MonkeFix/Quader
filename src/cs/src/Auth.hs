module Auth where

import Universum
import System.Timeout

import Network.WebSockets qualified as WS

import Types.Auth

-- type API = "user" :> ReqBody '[JSON] Text :> Get '[JSON] (Either Text Void)

-- validate = client $ Proxy @API

authorize :: WS.Connection -> IO (Maybe (ID User))
authorize conn = timeout (30 * 10^6) do return 1
