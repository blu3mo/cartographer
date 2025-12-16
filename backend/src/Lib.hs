{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp,
    API,
  )
where

import Data.Proxy
import Network.Wai.Handler.Warp qualified as Warp
import ProjectM36.Client (Connection, SessionId)
import Servant

-- | Simple API definition
type API = "health" :> Get '[JSON] String

-- | Verify that the handler matches the API
server :: Server API
server = return "OK"

-- | Boilerplate to turn the API type into a standard WAI Application
app :: SessionId -> Connection -> Application
app sessionId conn = serve (Proxy :: Proxy API) server

-- | Entry point
startApp :: SessionId -> Connection -> IO ()
startApp sessionId conn = do
  putStrLn "Starting Cartographer Backend on port 8081..."
  Warp.run 8081 (app sessionId conn)
