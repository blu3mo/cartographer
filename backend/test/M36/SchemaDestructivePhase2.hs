{-# LANGUAGE OverloadedStrings #-}

-- | Schema Destructive Change Phase 2: Read After Type Removal
--
-- このプログラムは破壊的スキーマ変更（型の削除）をテストするPhase 2です：
-- 1. Phase 1で保存されたデータ（ReportGenerated×2, ContextDefined×1）が存在するDBに接続
-- 2. 型定義からContextDefinedが削除された状態でデータを読み取り
-- 3. 何が起きるかを検証（エラー？部分的な読み取り？）
--
-- 前提:
-- - Types.hsからContextDefinedがコメントアウトされていること
-- - Phase 1が実行済み
--
-- 使用方法:
--   # Types.hsからContextDefinedをコメントアウト
--   cabal run schema-destructive-phase2
module Main where

import Data.Maybe (fromMaybe)
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
import ProjectM36.Client.Simple (query, withTransaction)
import System.Environment (lookupEnv)

-- 固定のDBパス（破壊的変更テスト用）
testDbPath :: FilePath
testDbPath = "/tmp/m36-schema-destructive-test"

main :: IO ()
main = do
  envPath <- lookupEnv "TEST_DB_PATH"
  let dbPath = fromMaybe testDbPath envPath

  putStrLn "=========================================="
  putStrLn "Destructive Change Phase 2: Read After Type Removal"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn $ "DB Path: " ++ dbPath
  putStrLn ""
  putStrLn "SCENARIO:"
  putStrLn "  Phase 1 saved: ReportGenerated x 2, ContextDefined x 1"
  putStrLn "  Phase 2 type definition: ContextDefined is REMOVED"
  putStrLn ""
  putStrLn "QUESTION: What happens to the ContextDefined data?"
  putStrLn ""

  putStrLn "Step 1: Connecting to existing DB..."

  result <- withM36Connection (Persistent dbPath) $ \conn -> do
    -- 条件付きマイグレーション
    putStrLn "Step 2: Running migrateSchemaIfNeeded..."
    migResult <- migrateSchemaIfNeeded conn
    case migResult of
      Left migErr -> do
        putStrLn $ "  Migration error: " ++ show migErr
        pure (Left $ "Migration failed: " ++ show migErr)
      Right () -> do
        putStrLn "  Migration: OK"

        -- データを読み取り
        putStrLn ""
        putStrLn "Step 3: Reading all events..."
        readResult <- withTransaction conn $ do
          query (RelationVariable "events" ())

        case readResult of
          Left err -> pure (Left $ "Read failed: " ++ show err)
          Right allData -> pure (Right allData)

  case result of
    Left err -> do
      putStrLn ""
      putStrLn $ "CONNECTION ERROR: " ++ show err
      putStrLn ""
      putStrLn "=========================================="
      putStrLn "Phase 2 Failed: Connection Error"
      putStrLn "=========================================="
    Right (Left err) -> do
      putStrLn ""
      putStrLn $ "READ ERROR: " ++ err
      putStrLn ""
      putStrLn "This may indicate that the database contains data"
      putStrLn "that cannot be deserialized with the current type definition."
      putStrLn ""
      putStrLn "=========================================="
      putStrLn "Phase 2 Result: Read Error (Expected for Destructive Change)"
      putStrLn "=========================================="
    Right (Right relation) -> do
      putStrLn ""
      putStrLn "Relation contents:"
      putStrLn $ show relation
      putStrLn ""

      -- ファクト種別の存在確認
      let relationStr = show relation
      let hasReport = "ReportGenerated" `isInfixOf` relationStr
      let hasContext = "ContextDefined" `isInfixOf` relationStr

      putStrLn "=========================================="
      putStrLn "Step 4: Data Verification"
      putStrLn "=========================================="
      putStrLn ""
      putStrLn "Verification:"
      putStrLn $ "  - ReportGenerated: " ++ (if hasReport then "✓ FOUND" else "✗ NOT FOUND")
      putStrLn $ "  - ContextDefined: " ++ (if hasContext then "✓ FOUND (unexpected!)" else "✗ NOT FOUND")
      putStrLn ""

      if hasContext
        then do
          putStrLn "RESULT: ContextDefined data is STILL PRESENT"
          putStrLn ""
          putStrLn "This indicates that M36 stores data with the type name"
          putStrLn "as part of the value, and can still read it even if"
          putStrLn "the Haskell type definition has been removed."
          putStrLn ""
          putStrLn "=========================================="
          putStrLn "Phase 2 Complete: Backward Compatibility Confirmed"
          putStrLn "=========================================="
        else
          if hasReport
            then do
              putStrLn "RESULT: Only ReportGenerated data is visible"
              putStrLn ""
              putStrLn "ContextDefined data may have been:"
              putStrLn "  a) Filtered out silently"
              putStrLn "  b) Failed to deserialize"
              putStrLn "  c) Stored differently"
              putStrLn ""
              putStrLn "=========================================="
              putStrLn "Phase 2 Complete: Partial Data Recovery"
              putStrLn "=========================================="
            else do
              putStrLn "RESULT: No events found"
              putStrLn ""
              putStrLn "=========================================="
              putStrLn "Phase 2 Complete: Unexpected State"
              putStrLn "=========================================="

-- Helper functions
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
