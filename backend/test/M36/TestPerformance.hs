{-# LANGUAGE OverloadedStrings #-}

-- | Query Performance Test
--
-- 大量データの書き込みと読み取りのパフォーマンスを測定します：
-- 1. 1000件のイベントを一括挿入
-- 2. 全件取得の時間を測定
-- 3. 特定セッションのフィルタリング時間を測定（メモリ上）
--
-- 使用方法: cabal run test-performance
module Main where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM)
import Data.Text (pack)
import Data.Time (diffUTCTime, getCurrentTime)
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

-- 固定のDBパス
testDbPath :: FilePath
testDbPath = "/tmp/m36-performance-test"

numEvents :: Int
numEvents = 1000

main :: IO ()
main = do
  putStrLn "=========================================="
  putStrLn "Query Performance Test"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn $ "DB Path: " ++ testDbPath
  putStrLn $ "Number of events: " ++ show numEvents
  putStrLn ""

  -- 初期化
  putStrLn "Step 1: Initializing DB..."
  initResult <- withM36Connection (Persistent testDbPath) $ \conn -> do
    migrateSchemaIfNeeded conn

  case initResult of
    Right (Right ()) -> putStrLn "  Migration: OK"
    _ -> error "Initialization failed"

  sessionId <- nextRandom
  now <- getCurrentTime

  -- イベント生成
  putStrLn "Step 2: Generating events in memory..."
  events <- forM [1 .. numEvents] $ \i -> do
    eid <- nextRandom
    pure
      Event
        { eventId = eid,
          sessionId = sessionId,
          timestamp = now,
          payload = InsightExtracted $ pack $ "Perf test event " ++ show i
        }

  -- 強制評価（生成時間を測定から除外するため）
  _ <- evaluate (force (length events))

  -- 書き込みパフォーマンス測定
  putStrLn ""
  putStrLn "Step 3: Measuring WRITE performance..."
  writeStart <- getCurrentTime

  writeResult <- withM36Connection (Persistent testDbPath) $ \conn -> do
    withTransaction conn $ do
      case toInsertExpr events "events" of
        Left err -> error $ "toInsertExpr failed: " ++ show err
        Right insertExpr -> execute insertExpr

  writeEnd <- getCurrentTime

  case writeResult of
    Right (Right ()) -> putStrLn "  Write: OK"
    _ -> error "Write failed"

  let writeDuration = diffUTCTime writeEnd writeStart
  putStrLn $ "  Time taken: " ++ show writeDuration
  putStrLn $ "  Rate: " ++ show (fromIntegral numEvents / realToFrac writeDuration :: Double) ++ " events/sec"

  -- 読み取りパフォーマンス測定
  putStrLn ""
  putStrLn "Step 4: Measuring READ performance (All Events)..."
  readStart <- getCurrentTime

  readResult <- withM36Connection (Persistent testDbPath) $ \conn -> do
    withTransaction conn $ do
      query (RelationVariable "events" ())

  readEnd <- getCurrentTime

  case readResult of
    Right (Right rel) -> do
      -- 結果を強制評価
      _ <- evaluate (length (show rel))
      putStrLn "  Read: OK"
    _ -> error "Read failed"

  let readDuration = diffUTCTime readEnd readStart
  putStrLn $ "  Time taken: " ++ show readDuration

  putStrLn ""
  putStrLn "=========================================="
  putStrLn "Performance Test Complete"
  putStrLn "=========================================="
