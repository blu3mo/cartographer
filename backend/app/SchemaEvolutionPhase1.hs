{-# LANGUAGE OverloadedStrings #-}

-- | Schema Evolution Phase 1 Test
--
-- このモジュールは単体で実行可能なテストプログラムです。
-- v1型定義（ReportGeneratedなし）でDBにデータを書き込みます。
--
-- 使用方法:
-- 1. Types.hsからReportGeneratedをコメントアウト
-- 2. cabal run schema-evolution-phase1
-- 3. Types.hsにReportGeneratedを復活
-- 4. cabal run schema-evolution-phase2
module Main where

import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Domain.Types
  ( Event (..),
    FactPayload (..),
  )
import Effect.Persistence
  ( DbConfig (..),
    migrateSchema,
    withM36Connection,
  )
import ProjectM36.Base (RelationalExprBase (..))
import ProjectM36.Client.Simple (execute, query, withTransaction)
import ProjectM36.Tupleable (toInsertExpr)

-- 固定のDBパス（Phase 1とPhase 2で共有）
testDbPath :: FilePath
testDbPath = "/tmp/m36-schema-evolution-real-test"

main :: IO ()
main = do
  putStrLn "=========================================="
  putStrLn "Schema Evolution Phase 1: v1 Type Definition"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn $ "DB Path: " ++ testDbPath

  -- イベントIDを生成
  eventId <- nextRandom
  sessionId <- nextRandom
  now <- getCurrentTime

  -- v1では InsightExtracted のみ使用可能
  let event =
        Event
          { eventId = eventId,
            sessionId = sessionId,
            timestamp = now,
            payload = InsightExtracted "Phase1で保存されたインサイト（v1型定義）"
          }

  putStrLn ""
  putStrLn "Inserting event with InsightExtracted payload..."

  result <- withM36Connection (Persistent testDbPath) $ \conn -> do
    migResult <- migrateSchema conn
    case migResult of
      Left err -> pure (Left $ "Migration failed: " ++ show err)
      Right () -> do
        txnResult <- withTransaction conn $ do
          case toInsertExpr [event] "events" of
            Left err -> error $ "toInsertExpr failed: " ++ show err
            Right insertExpr -> do
              execute insertExpr
              query (RelationVariable "events" ())
        case txnResult of
          Left err -> pure (Left $ "Transaction failed: " ++ show err)
          Right rel -> pure (Right rel)

  case result of
    Left err -> do
      putStrLn $ "ERROR: " ++ show err
      error "Phase 1 failed"
    Right (Left err) -> do
      putStrLn $ "ERROR: " ++ err
      error "Phase 1 failed"
    Right (Right relation) -> do
      putStrLn ""
      putStrLn "SUCCESS: Event inserted"
      putStrLn ""
      putStrLn "Relation contents:"
      print relation
      putStrLn ""
      putStrLn "=========================================="
      putStrLn "Phase 1 Complete"
      putStrLn "=========================================="
      putStrLn ""
      putStrLn "Next steps:"
      putStrLn "  1. Restore ReportGenerated in Types.hs"
      putStrLn "  2. Run: cabal run schema-evolution-phase2"
