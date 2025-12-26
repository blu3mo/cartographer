{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Effect.User
import Implementation.User
import Network.Wai.Handler.Warp qualified as Warp
import Polysemy
import Polysemy.Error
import Servant
import Servant.Server
import Web.API
import Web.Server

main :: IO ()
main = do
  putStrLn "Starting server on port 8080..."
  Warp.run 8080 app

app :: Application
app = serve (Proxy @API) (hoistServer (Proxy @API) interpretServer server)

interpretServer :: Sem '[UserEffect, Error ServerError, Embed IO] a -> Handler a
interpretServer sem = do
  res <- liftIO $ runM $ runError $ runUserPure sem
  case res of
    Left err -> throwError err
    Right val -> pure val
