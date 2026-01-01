{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.ByteString.Char8 qualified as BS
import Effect.Persistence (DbConfig (..), migrateSchemaIfNeeded, withM36Connection)
import Effect.Projection (PgConfig (..))
import Network.Wai.Handler.Warp qualified as Warp
import System.Environment (lookupEnv)
import Web.Server (AppConfig (..), app)
import Data.Maybe (fromMaybe)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  putStrLn "Starting server on port 8080..."

  -- M36データベース設定（環境変数 M36_DATA_PATH があればそれを使用）
  m36Path <- lookupEnv "M36_DATA_PATH"
  let m36Config = Persistent (Data.Maybe.fromMaybe ".m36-data" m36Path)

  -- PostgreSQL設定（環境変数から取得、未設定なら無効）
  pgConnStr <- lookupEnv "DATABASE_URL"
  let pgConfig = PgConfig . BS.pack <$> pgConnStr

  -- 起動時にスキーママイグレーション（冪等、既存なら何もしない）
  putStrLn "Migrating schema..."
  migrationResult <- withM36Connection m36Config migrateSchemaIfNeeded
  case migrationResult of
    Left err -> error $ "Failed to migrate schema: " ++ show err
    Right (Left err) -> error $ "Failed to migrate schema: " ++ show err
    Right (Right ()) -> putStrLn "Schema ready."

  -- PostgreSQL Projection 設定状況を表示
  case pgConfig of
    Nothing -> putStrLn "PostgreSQL projection: DISABLED (set DATABASE_URL to enable)"
    Just _ -> putStrLn "PostgreSQL projection: ENABLED"

  let config = AppConfig {m36Config = m36Config, pgConfig = pgConfig}
  Warp.run 8080 (app config)
