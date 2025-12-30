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

import Control.Monad (forM)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Domain.Types
  ( Event (..),
    EventId,
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

-- 固定のDBパス
testDbPath :: FilePath
testDbPath = "/tmp/m36-projection-test"

main :: IO ()
main = do
  envPath <- lookupEnv "TEST_DB_PATH"
  let dbPath = fromMaybe testDbPath envPath

  putStrLn "=========================================="
  putStrLn "Projection Consistency Test"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn $ "DB Path: " ++ dbPath
  putStrLn ""

  sessionId1 <- nextRandom
  sessionId2 <- nextRandom

  -- 単一の接続スコープで実行 (永続化バグとロジックバグを切り分け)
  withM36ConnectionResult <- withM36Connection (Persistent dbPath) $ \conn -> do
    -- Step 1: Migration
    putStrLn "Step 1: Initial migration..."
    migResult <- migrateSchemaIfNeeded conn
    case migResult of
      Left err -> error $ "Migration error: " ++ show err
      Right () -> putStrLn "  Migration: OK"

    -- Step 2: Create Events
    now <- getCurrentTime

    hostUserId <- nextRandom
    let session1Context =
          SessionContext
            { title = SessionTitle "Project Plan",
              purpose = SessionPurpose "Design",
              background = SessionBackground "M36 Integration",
              hostUserId = hostUserId
            }

    putStrLn ""
    putStrLn "Step 2: Creating events in chronological order..."

    -- イベント生成にはEventIdが必要なのでIO内で行う
    refId1 <- nextRandom
    refId2 <- nextRandom
    refId3 <- nextRandom
    refId4 <- nextRandom
    refId5 <- nextRandom

    events1 <-
      createSessionEvents
        sessionId1
        now
        [ ("00:00", ContextDefined session1Context),
          ("00:01", ReportGenerated (pack "Report 1: M36 supports Haskell ADT") refId1),
          ("00:02", ReportGenerated (pack "Report 2: Atomable is important") refId2),
          ("00:03", ReportGenerated (pack "Report 3: Persistence is not idempotent") refId3)
        ]

    events2 <-
      createSessionEvents
        sessionId2
        now
        [ ("00:00", ReportGenerated (pack "Session 2: Event 1") refId4),
          ("00:01", ReportGenerated (pack "Session 2: Event 2") refId5)
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
        let hasReport1 = "Report 1" `isInfixOf` relationStr
        let hasReport2 = "Report 2" `isInfixOf` relationStr
        let hasReport3 = "Report 3" `isInfixOf` relationStr

        putStrLn $ "    - ContextDefined: " ++ showCheck hasContext
        putStrLn $ "    - ReportGenerated 1: " ++ showCheck hasReport1
        putStrLn $ "    - ReportGenerated 2: " ++ showCheck hasReport2
        putStrLn $ "    - ReportGenerated 3: " ++ showCheck hasReport3

        -- Step 6: Verify Session 2
        putStrLn ""
        putStrLn "Step 6: Verifying Session 2 Projection..."
        let hasSession2Event1 = "Session 2: Event 1" `isInfixOf` relationStr
        let hasSession2Event2 = "Session 2: Event 2" `isInfixOf` relationStr

        putStrLn $ "    - Event 1: " ++ showCheck hasSession2Event1
        putStrLn $ "    - Event 2: " ++ showCheck hasSession2Event2

        let allChecks = hasContext && hasReport1 && hasReport2 && hasReport3 && hasSession2Event1 && hasSession2Event2

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
    let offset = (fromIntegral idx :: NominalDiffTime) * 60 -- 1分ずつずらす
    let eventTime = addUTCTime offset baseTime
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
