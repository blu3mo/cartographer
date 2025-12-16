module Main (main) where

import Cartographer.Database (exportTutorialD)
import Configuration.Dotenv (defaultConfig, loadFile, onMissingFile)
import Data.ByteString.Char8 qualified as BS
import Lib
import System.Environment (getArgs, getEnv)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--export-schema"] -> exportTutorialD
    _ -> runServer

runServer :: IO ()
runServer = do
  onMissingFile (loadFile defaultConfig) (return ())
  putStrLn "Starting Cartographer Backend..."

  -- 1. Load Configuration
  dbHost <- getEnv "DB_HOST"
  dbName <- getEnv "DB_NAME"
  dbUser <- getEnv "DB_USER"
  dbPassword <- getEnv "DB_PASSWORD"
  dbPort <- getEnv "DB_PORT"

  let connStr = BS.pack $ "host=" <> dbHost <> " dbname=" <> dbName <> " user=" <> dbUser <> " password=" <> dbPassword <> " port=" <> dbPort

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
