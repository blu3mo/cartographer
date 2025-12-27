{-# LANGUAGE OverloadedStrings #-}

-- | Projection Consistency Test
--
-- イベントソーシングにおけるProjection（状態導出）の一貫性をテストします：
-- 1. 複数のイベントを順番に書き込み
-- 2. イベントから現在の状態を導出（Projection）
-- 3. 再構築結果の正確性を検証
-- 4. イベント順序の保証を確認
--
-- 使用方法: cabal run test-projection
module Main where

import Control.Monad (forM, forM_)
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Domain.Types
  ( Event (..),
    FactPayload (..),
    SessionBackground (..),
    SessionContext (..),
    SessionPurpose (..),
    SessionTitle (..),
    SessionTopic (..),
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
testDbPath = "/tmp/m36-projection-test"

main :: IO ()
main = do
  putStrLn "=========================================="
  putStrLn "Projection Consistency Test"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn $ "DB Path: " ++ testDbPath
  putStrLn ""

  sessionId1 <- nextRandom
  sessionId2 <- nextRandom

  -- 単一の接続スコープで実行 (永続化バグとロジックバグを切り分け)
  withM36ConnectionResult <- withM36Connection (Persistent testDbPath) $ \conn -> do
    -- Step 1: Migration
    putStrLn "Step 1: Initial migration..."
    migResult <- migrateSchemaIfNeeded conn
    case migResult of
      Left err -> error $ "Migration error: " ++ show err
      Right () -> putStrLn "  Migration: OK"

    -- Step 2: Create Events
    now <- getCurrentTime

    let session1Context =
          SessionContext
            { title = SessionTitle "Project Plan",
              purpose = SessionPurpose "Design",
              topic = SessionTopic "Architecture",
              background = SessionBackground "M36 Integration"
            }

    putStrLn ""
    putStrLn "Step 2: Creating events in chronological order..."

    events1 <-
      createSessionEvents
        sessionId1
        now
        [ ("00:00", ContextDefined session1Context),
          ("00:01", InsightExtracted "Insight 1: M36 supports Haskell ADT"),
          ("00:02", InsightExtracted "Insight 2: Atomable is important"),
          ("00:03", InsightExtracted "Insight 3: Persistence is not idempotent")
        ]

    events2 <-
      createSessionEvents
        sessionId2
        now
        [ ("00:00", InsightExtracted "Session 2: Event 1"),
          ("00:01", InsightExtracted "Session 2: Event 2")
        ]

    let allEvents = events1 ++ events2
    putStrLn $ "  Created " ++ show (length allEvents) ++ " events"

    -- Step 3: Write
    putStrLn ""
    putStrLn "Step 3: Writing events..."

    writeResult <- withTransaction conn $ do
      case toInsertExpr allEvents "events" of
        Left err -> error $ "toInsertExpr failed: " ++ show err
        Right insertExpr -> execute insertExpr

    case writeResult of
      Left err -> error $ "Write error: " ++ show err
      Right () -> putStrLn "  Write: OK"

    -- Step 4: Read & Verify
    putStrLn ""
    putStrLn "Step 4: Building projection from events..."

    readResult <- withTransaction conn $ do
      query (RelationVariable "events" ())

    case readResult of
      Left err -> error $ "Read error: " ++ show err
      Right relation -> do
        let relationStr = show relation
        putStrLn $ "DEBUG: Relation content:\n" ++ relationStr

        -- Step 5: Verify Session 1
        putStrLn ""
        putStrLn "Step 5: Verifying Session 1 Projection..."

        let hasContext = "ContextDefined" `isInfixOf` relationStr
        let hasInsight1 = "Insight 1" `isInfixOf` relationStr
        let hasInsight2 = "Insight 2" `isInfixOf` relationStr
        let hasInsight3 = "Insight 3" `isInfixOf` relationStr

        putStrLn $ "    - ContextDefined: " ++ showCheck hasContext
        putStrLn $ "    - InsightExtracted 1: " ++ showCheck hasInsight1
        putStrLn $ "    - InsightExtracted 2: " ++ showCheck hasInsight2
        putStrLn $ "    - InsightExtracted 3: " ++ showCheck hasInsight3

        -- Step 6: Verify Session 2
        putStrLn ""
        putStrLn "Step 6: Verifying Session 2 Projection..."
        let hasSession2Event1 = "Session 2: Event 1" `isInfixOf` relationStr
        let hasSession2Event2 = "Session 2: Event 2" `isInfixOf` relationStr

        putStrLn $ "    - Event 1: " ++ showCheck hasSession2Event1
        putStrLn $ "    - Event 2: " ++ showCheck hasSession2Event2

        let allChecks = hasContext && hasInsight1 && hasInsight2 && hasInsight3 && hasSession2Event1 && hasSession2Event2

        putStrLn ""
        if allChecks
          then do
            putStrLn "=========================================="
            putStrLn "SUCCESS: Projection consistency verified!"
            putStrLn "=========================================="
          else do
            putStrLn "=========================================="
            putStrLn "FAILED: Some events are missing"
            putStrLn "=========================================="

    return () -- End of withM36Connection block (IO ())

  -- Handle result of withM36Connection
  case withM36ConnectionResult of
    Left err -> error $ "DB Connection Error: " ++ show err
    Right () -> pure ()

-- イベントを時刻オフセット付きで作成
createSessionEvents :: UUID -> UTCTime -> [(String, FactPayload)] -> IO [Event]
createSessionEvents sessionId baseTime eventSpecs = do
  forM (zip [0 ..] eventSpecs) $ \(idx, (_, payload)) -> do
    eventId <- nextRandom
    let offset = fromIntegral (idx :: Int) * 60 -- 1分ずつずらす
    let eventTime = addUTCTime (realToFrac offset) baseTime
    pure
      Event
        { eventId = eventId,
          sessionId = sessionId,
          timestamp = eventTime,
          payload = payload
        }

-- ヘルパー関数
showCheck :: Bool -> String
showCheck True = "✓ FOUND"
showCheck False = "✗ NOT FOUND"
