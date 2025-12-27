{-# LANGUAGE OverloadedStrings #-}

-- | Persistence Recovery Test
--
-- 永続化とリカバリ（再接続時のデータ保持）をテストします：
-- 1. DBを作成し、データを書き込み
-- 2. 接続を閉じる
-- 3. 再接続し、データが存在することを確認
-- 4. 追加データを書き込み
-- 5. 再度閉じて再接続し、全データを確認
--
-- 使用方法: cabal run test-recovery
module Main where

import Control.Monad (when)
import Data.Text (pack, unpack)
import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Domain.Types
  ( Event (..),
    FactPayload (..),
  )
import Effect.Persistence
  ( DbConfig (..),
    migrateSchemaIfNeeded,
    withM36Connection,
  )
import ProjectM36.Base (RelationalExprBase (..))
import ProjectM36.Client.Simple (execute, query, withTransaction)
import ProjectM36.Tupleable (toInsertExpr)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)

-- 固定のDBパス
testDbPath :: FilePath
testDbPath = "/tmp/m36-recovery-test"

main :: IO ()
main = do
  putStrLn "=========================================="
  putStrLn "Persistence Recovery Test"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn $ "DB Path: " ++ testDbPath
  putStrLn ""

  -- クリーンアップ
  exists <- doesDirectoryExist testDbPath
  when exists $ do
    putStrLn "Cleaning up old DB..."
    removeDirectoryRecursive testDbPath

  sessionId <- nextRandom
  now <- getCurrentTime

  -- Session 1: 初期データの書き込み
  putStrLn "Step 1: Session 1 - Writing initial data..."

  s1Result <- withM36Connection (Persistent testDbPath) $ \conn -> do
    -- マイグレーション
    _ <- migrateSchemaIfNeeded conn

    eventId <- nextRandom
    let event =
          Event
            { eventId = eventId,
              sessionId = sessionId,
              timestamp = now,
              payload = InsightExtracted "Session 1 Data"
            }

    withTransaction conn $ do
      case toInsertExpr [event] "events" of
        Left err -> error $ "toInsertExpr failed: " ++ show err
        Right insertExpr -> execute insertExpr

  case s1Result of
    Right (Right ()) -> putStrLn "  Session 1 Write: OK"
    _ -> error "Session 1 failed"

  putStrLn "  (Connection closed)"

  -- Session 2: 再接続とデータ確認、追加書き込み
  putStrLn ""
  putStrLn "Step 2: Session 2 - Reconnecting and verifying..."

  s2Result <- withM36Connection (Persistent testDbPath) $ \conn -> do
    -- 再接続時はmigrateSchemaIfNeededでチェックのみ（マイグレーションスキップ）を行うはず
    migResult <- migrateSchemaIfNeeded conn
    case migResult of
      Right () -> pure ()
      Left err -> error $ "Migration check failed: " ++ show err

    -- データ確認
    readResult <- withTransaction conn $ query (RelationVariable "events" ())

    case readResult of
      Right rel -> do
        let relStr = show rel
        if "Session 1 Data" `isInfixOf` relStr
          then pure ()
          else error "Session 1 data NOT FOUND after reconnection"
      Left err -> error $ "Query failed: " ++ show err

    -- 追加データの書き込み
    eventId2 <- nextRandom
    let event2 =
          Event
            { eventId = eventId2,
              sessionId = sessionId,
              timestamp = now,
              payload = InsightExtracted "Session 2 Data"
            }

    withTransaction conn $ do
      case toInsertExpr [event2] "events" of
        Left err -> error $ "toInsertExpr failed: " ++ show err
        Right insertExpr -> execute insertExpr

  case s2Result of
    Right (Right ()) -> putStrLn "  Session 2 Verification & Write: OK"
    _ -> error "Session 2 failed"

  putStrLn "  (Connection closed)"

  -- Session 3: 最終確認
  putStrLn ""
  putStrLn "Step 3: Session 3 - Final verification..."

  s3Result <- withM36Connection (Persistent testDbPath) $ \conn -> do
    _ <- migrateSchemaIfNeeded conn
    withTransaction conn $ query (RelationVariable "events" ())

  case s3Result of
    Right (Right rel) -> do
      let relStr = show rel
      let hasS1 = "Session 1 Data" `isInfixOf` relStr
      let hasS2 = "Session 2 Data" `isInfixOf` relStr

      putStrLn $ "  Session 1 Data: " ++ (if hasS1 then "✓ FOUND" else "✗ NOT FOUND")
      putStrLn $ "  Session 2 Data: " ++ (if hasS2 then "✓ FOUND" else "✗ NOT FOUND")

      if hasS1 && hasS2
        then do
          putStrLn ""
          putStrLn "=========================================="
          putStrLn "SUCCESS: Persistence Recovery Verified!"
          putStrLn "=========================================="
        else do
          putStrLn "=========================================="
          putStrLn "FAILED: Data missing"
          putStrLn "=========================================="
    _ -> error "Session 3 failed"

-- Helper
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys)
  | x == y = isPrefixOf xs ys
  | otherwise = False

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_ : ys) = xs : tails ys
