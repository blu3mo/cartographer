{-# LANGUAGE OverloadedStrings #-}

-- | Schema Evolution Phase 2 Test (v2 with Conditional Migration)
--
-- このプログラムは条件付きマイグレーションを使用してスキーマ進化を検証します：
-- 1. Phase 1で保存されたデータが存在するDBに接続
-- 2. migrateSchemaIfNeeded で冪等なマイグレーションを実行
-- 3. 新しいファクト種別（ReportGenerated）を挿入
-- 4. 最後に全データを読み出してリストアップ
module Main where

import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Domain.Types
  ( Event (..),
    FactPayload (..),
    SessionBackground (..),
    SessionContext (..),
    SessionPurpose (..),
    SessionTitle (..),
  )
import Effect.Persistence
  ( DbConfig (..),
    migrateSchemaIfNeeded,
    withM36Connection,
  )
import ProjectM36.Base (RelationalExprBase (..))
import ProjectM36.Client.Simple (execute, query, withTransaction)
import ProjectM36.Tupleable (toInsertExpr)
import System.Environment (lookupEnv)

-- 固定のDBパス（Phase 1とPhase 2で共有）
testDbPath :: FilePath
testDbPath = "/tmp/m36-schema-evolution-real-test"

main :: IO ()
main = do
  envPath <- lookupEnv "TEST_DB_PATH"
  let dbPath = fromMaybe testDbPath envPath

  putStrLn "=========================================="
  putStrLn "Schema Evolution Phase 2: Conditional Migration"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn $ "DB Path: " ++ dbPath

  -- 新しいイベントを生成
  newEventId <- nextRandom
  sessionId <- nextRandom
  now <- getCurrentTime

  -- v2で追加された ReportGenerated を使用
  refId <- nextRandom
  let newEvent =
        Event
          { eventId = newEventId,
            sessionId = sessionId,
            timestamp = now,
            payload = ReportGenerated "Phase2で追加されたレポート" refId
          }

  putStrLn ""
  putStrLn "Step 1: Connecting to existing DB..."

  result <- withM36Connection (Persistent dbPath) $ \conn -> do
    -- 条件付きマイグレーション（冪等）
    putStrLn "Step 2: Running migrateSchemaIfNeeded (idempotent)..."
    migResult <- migrateSchemaIfNeeded conn
    case migResult of
      Left migErr -> do
        putStrLn $ "  Migration error: " ++ show migErr
        pure (Left $ "Migration failed: " ++ show migErr)
      Right () -> do
        putStrLn "  Migration: OK (schema exists or was created)"

        -- Phase 1のデータを読み取り
        putStrLn ""
        putStrLn "Step 3: Reading existing data from Phase 1..."
        readResult <- withTransaction conn $ do
          query (RelationVariable "events" ())

        case readResult of
          Left err -> pure (Left $ "Read failed: " ++ show err)
          Right existingData -> do
            putStrLn "  Existing data found:"
            putStrLn $ "  " ++ show existingData

            -- 新しいファクト種別（ContextDefined）を追加
            putStrLn ""
            putStrLn "Step 4: Inserting new event with ContextDefined..."
            insertResult <- withTransaction conn $ do
              case toInsertExpr [newEvent] "events" of
                Left err -> error $ "toInsertExpr failed: " ++ show err
                Right insertExpr -> do
                  execute insertExpr
                  query (RelationVariable "events" ())

            case insertResult of
              Left err -> pure (Left $ "Insert failed: " ++ show err)
              Right allData -> pure (Right allData)

  case result of
    Left err -> do
      putStrLn ""
      putStrLn $ "ERROR: " ++ show err
      putStrLn ""
      putStrLn "=========================================="
      putStrLn "Phase 2 Failed"
      putStrLn "=========================================="
    Right (Left err) -> do
      putStrLn ""
      putStrLn $ "ERROR: " ++ err
      putStrLn ""
      putStrLn "=========================================="
      putStrLn "Phase 2 Failed"
      putStrLn "=========================================="
    Right (Right finalRelation) -> do
      putStrLn "  Insert: OK"
      putStrLn ""
      putStrLn "=========================================="
      putStrLn "Step 5: Final Data Verification"
      putStrLn "=========================================="
      putStrLn ""
      putStrLn "All events in database:"
      putStrLn $ show finalRelation
      putStrLn ""

      -- データの存在確認
      let relationStr = show finalRelation
      let hasReport = "ReportGenerated" `isInfixOf` relationStr
      let hasContext = "ContextDefined" `isInfixOf` relationStr

      putStrLn "Verification:"
      putStrLn $ "  - ContextDefined (from Phase 1): " ++ (if hasContext then "✓ FOUND" else "✗ NOT FOUND")
      putStrLn $ "  - ReportGenerated (from Phase 2): " ++ (if hasReport then "✓ FOUND" else "✗ NOT FOUND")
      putStrLn ""

      if hasReport && hasContext
        then do
          putStrLn "=========================================="
          putStrLn "SUCCESS: Schema Evolution Verified!"
          putStrLn "=========================================="
          putStrLn ""
          putStrLn "Both fact types coexist in the same database:"
          putStrLn "  - Phase 1 data preserved after reconnection"
          putStrLn "  - Phase 2 data (new fact type) successfully added"
          putStrLn "  - Conditional migration works correctly"
        else do
          putStrLn "=========================================="
          putStrLn "FAILED: Missing expected data"
          putStrLn "=========================================="

-- Helperr function
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
