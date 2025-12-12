{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql (createPostgresqlPool, runMigration)
import Database.Persist.Sql (runSqlPool)
import Network.Wai.Handler.Warp (run)
import Servant (Proxy(..), serve, hoistServer)

-- Imports from our library
import Foundation (Env(..), runApp)
import Infrastructure.DB.Model (migrateAll)
import Web.API (API)
import Web.Handler (server)

-- Import implementations to ensure instances are available
import Feature.User.Repository ()
import Feature.Blog.Repository ()

main :: IO ()
main = do
  putStrLn "Starting Cartographer Backend..."

  -- 1. Load Configuration
  -- For simplicity, hardcoding connection string. In production, use env vars.
  let connStr = "host=localhost dbname=postgres user=postgres password=your-super-secret-and-long-postgres-password port=54322"
  
  -- 2. Create DB Pool
  -- running in LoggingT IO to support persistent logging
  runStdoutLoggingT $ do
    pool <- createPostgresqlPool connStr 10
    
    -- 3. Run Migrations
    liftIO $ runStdoutLoggingT $ flip runSqlPool pool $ do
        runMigration migrateAll
    
    -- 4. Construct Env
    let env = Env pool

    -- 5. Start Server
    liftIO $ putStrLn "Listening on port 8081"
    let api = Proxy @API
    -- hoistServer allows us to transform AppM to Handler
    let app = serve api $ hoistServer api (runApp env) server
    
    liftIO $ run 8081 app
