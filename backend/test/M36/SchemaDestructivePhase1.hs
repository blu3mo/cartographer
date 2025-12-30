{-# LANGUAGE OverloadedStrings #-}

-- | Schema Destructive Change Phase 1: Write All Fact Types
--
-- このプログラムは破壊的スキーマ変更（型の削除）をテストするPhase 1です：
-- 1. 新しいDBを作成
-- 2. 複数のファクト種別（ReportGenerated, ContextDefined）でデータを保存
-- 3. Phase 2で型を削除した状態で再接続して挙動を確認
--
-- 使用方法:
--   cabal run schema-destructive-phase1
--   # Types.hsからContextDefinedをコメントアウト
--   cabal run schema-destructive-phase2
module Main where

import Data.Maybe (fromMaybe)
import Data.Text (pack)
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
    migrateSchema,
    withM36Connection,
  )
import ProjectM36.Base (RelationalExprBase (..))
import ProjectM36.Client.Simple (execute, query, withTransaction)
import ProjectM36.Tupleable (toInsertExpr)
import System.Environment (lookupEnv)

-- 固定のDBパス（破壊的変更テスト用）
testDbPath :: FilePath
testDbPath = "/tmp/m36-schema-destructive-test"

main :: IO ()
main = do
  envPath <- lookupEnv "TEST_DB_PATH"
  let dbPath = fromMaybe testDbPath envPath

  putStrLn "=========================================="
  putStrLn "Destructive Change Phase 1: Write All Fact Types"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn $ "DB Path: " ++ dbPath

  -- 複数のイベントを生成
  eventId1 <- nextRandom
  eventId2 <- nextRandom
  eventId3 <- nextRandom
  sessionId <- nextRandom
  now <- getCurrentTime

  -- 複数のファクト種別でイベントを作成
  let testContext =
        SessionContext
          { title = SessionTitle "テストセッション",
            purpose = SessionPurpose "破壊的変更テスト",
            background = SessionBackground "Phase 1でのみ存在",
            hostUserId = sessionId
          }

  let events =
        [ Event
            { eventId = eventId1,
              sessionId = sessionId,
              timestamp = now,
              payload = ReportGenerated (pack "レポート1（どちらのPhaseでも読める）") eventId2
            },
          Event
            { eventId = eventId2,
              sessionId = sessionId,
              timestamp = now,
              payload = ContextDefined testContext
            },
          Event
            { eventId = eventId3,
              sessionId = sessionId,
              timestamp = now,
              payload = ReportGenerated (pack "レポート2（どちらのPhaseでも読める）") eventId1
            }
        ]

  putStrLn ""
  putStrLn "Inserting 3 events:"
  putStrLn "  - ReportGenerated x 2"
  putStrLn "  - ContextDefined x 1"

  result <- withM36Connection (Persistent dbPath) $ \conn -> do
    migResult <- migrateSchema conn
    case migResult of
      Left err -> pure (Left $ "Migration failed: " ++ show err)
      Right () -> do
        txnResult <- withTransaction conn $ do
          case toInsertExpr events "events" of
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
      putStrLn "SUCCESS: 3 events inserted"
      putStrLn ""
      putStrLn "Relation contents:"
      putStrLn $ show relation
      putStrLn ""

      -- ファクト種別の存在確認
      let relationStr = show relation
      let reportCount = countOccurrences "ReportGenerated" relationStr
      let contextCount = countOccurrences "ContextDefined" relationStr

      putStrLn "Summary:"
      putStrLn $ "  - ReportGenerated: " ++ show reportCount ++ " events"
      putStrLn $ "  - ContextDefined: " ++ show contextCount ++ " events"
      putStrLn ""
      putStrLn "=========================================="
      putStrLn "Phase 1 Complete"
      putStrLn "=========================================="
      putStrLn ""
      putStrLn "Next steps:"
      putStrLn "  1. Comment out ContextDefined in Types.hs"
      putStrLn "  2. Run: cabal run schema-destructive-phase2"
      putStrLn "  3. Observe what happens with existing ContextDefined data"

-- Helper: 文字列内のパターン出現回数をカウント
countOccurrences :: String -> String -> Int
countOccurrences needle haystack = length $ filter (isPrefixOf needle) (tails haystack)

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys)
  | x == y = isPrefixOf xs ys
  | otherwise = False

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_ : ys) = xs : tails ys
