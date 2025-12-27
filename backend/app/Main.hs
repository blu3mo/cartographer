{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Effect.Persistence (DbConfig (..), Persistence, migrateSchemaIfNeeded, runPersistence, withM36Connection)
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
  -- データベースはプロジェクトルートの .m36-data に保存
  let dbConfig = Persistent ".m36-data"

  -- 起動時にスキーママイグレーション（冪等、既存なら何もしない）
  putStrLn "Migrating schema..."
  migrationResult <- withM36Connection dbConfig migrateSchemaIfNeeded
  case migrationResult of
    Left err -> error $ "Failed to migrate schema: " ++ show err
    Right (Left err) -> error $ "Failed to migrate schema: " ++ show err
    Right (Right ()) -> putStrLn "Schema ready."

  Warp.run 8080 (app dbConfig)

app :: DbConfig -> Application
app dbConfig = serve (Proxy @API) (hoistServer (Proxy @API) (interpretServer dbConfig) server)

interpretServer :: DbConfig -> Sem '[Persistence, Error ServerError, Embed IO] a -> Handler a
interpretServer dbConfig sem = do
  res <- liftIO $ runM $ runError $ runPersistence dbConfig sem
  case res of
    Left err -> throwError err
    Right val -> pure val
