module Lib
  ( startApp,
  )
where

import Data.Aeson
import Data.Proxy
import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp
import Polysemy
import Servant

-- | Simple API definition
type API = "health" :> Get '[JSON] String

-- | Verify that the handler matches the API
server :: Server API
server = return "OK"

-- | Boilerplate to turn the API type into a standard WAI Application
app :: Application
app = serve (Proxy :: Proxy API) server

-- | Entry point
startApp :: IO ()
startApp = do
  putStrLn "Starting Cartographer Backend on port 8081..."
  Warp.run 8081 app
