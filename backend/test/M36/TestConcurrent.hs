{-# LANGUAGE OverloadedStrings #-}

-- | Concurrent Access Test
--
-- M36 In-Process Persistenceの同時書き込み制限（ファイルロック・分岐競合）を回避するため、
-- Actorパターン（Single Writer Thread）を採用して同時実行性をテストします。
-- クライアントスレッドはイベント生成のみ行い、書き込みは単一のWriterスレッドが担当します。
--
-- 使用方法: cabal run test-concurrent
module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM, forM_)
import Data.Text (pack)
import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Domain.Types
  ( Event (..),
    FactPayload (..),
  )
import Effect.Persistence
  ( DbConfig (..),
    DbConn,
    migrateSchemaIfNeeded,
    withM36Connection,
  )
import ProjectM36.Base (RelationalExprBase (..))
import ProjectM36.Client.Simple (execute, query, withTransaction)
import ProjectM36.Tupleable (toInsertExpr)

-- 固定のDBパス
testDbPath :: FilePath
testDbPath = "/tmp/m36-concurrent-test"

-- スレッド数
numThreads :: Int
numThreads = 10

-- 各スレッドが書き込むイベント数
eventsPerThread :: Int
eventsPerThread = 10

-- Writerへのメッセージ
data WriterMsg
  = MsgWrite Event (MVar Bool) -- イベント書き込み要求 (結果通知用MVar付き)
  | MsgStop -- 終了シグナル

main :: IO ()
main = do
  putStrLn "=========================================="
  putStrLn "Concurrent Access Test (Actor Pattern)"
  putStrLn "=========================================="
  putStrLn ""
  putStrLn $ "DB Path: " ++ testDbPath
  putStrLn $ "Threads: " ++ show numThreads
  putStrLn $ "Events per thread: " ++ show eventsPerThread
  putStrLn $ "Total events: " ++ show (numThreads * eventsPerThread)
  putStrLn ""

  -- DB接続とWriterスレッドの初期化
  putStrLn "Step 1: Initializing DB and Writer..."

  -- メインスレッドで接続を開く
  connResult <- withM36Connection (Persistent testDbPath) $ \conn -> do
    -- マイグレーション
    migResult <- migrateSchemaIfNeeded conn
    case migResult of
      Left err -> error $ "Migration error: " ++ show err
      Right () -> putStrLn "  Migration: OK"

    -- メッセージチャネル作成
    msgChan <- newChan

    -- Writer完了待機用MVar
    writerDone <- newEmptyMVar

    -- Writerスレッド起動
    forkIO $ writerLoop conn msgChan writerDone

    -- クライアント完了通知用MVar
    clientDoneMVars <- [1 .. numThreads] `forM` \_ -> newEmptyMVar

    -- 開始時刻
    startTime <- getCurrentTime
    putStrLn ""
    putStrLn "Step 2: Starting concurrent clients..."

    -- クライアントスレッド起動
    forM_ (zip [1 .. numThreads] clientDoneMVars) $ \(threadId, doneMVar) -> do
      forkIO $ do
        result <- clientWorker msgChan threadId
        putMVar doneMVar result

    -- 全クライアント完了待機
    results <- forM clientDoneMVars takeMVar

    endTime <- getCurrentTime

    putStrLn ""
    putStrLn "Step 3: Results..."

    let successCount = length $ filter id results
    let failCount = length $ filter not results

    putStrLn $ "  Successful threads: " ++ show successCount
    putStrLn $ "  Failed threads: " ++ show failCount
    putStrLn $ "  Total time: " ++ show endTime ++ " - " ++ show startTime

    -- Writer停止
    writeChan msgChan MsgStop
    takeMVar writerDone -- Writerが完全に停止するのを待つ

    -- データ検証 (Single Writerで書き込まれたデータの整合性確認)
    putStrLn ""
    putStrLn "Step 4: Verifying data..."

    verifyResult <- withTransaction conn $ do
      query (RelationVariable "events" ())

    case verifyResult of
      Left err -> do
        putStrLn $ "Query error: " ++ show err
        pure False
      Right relation -> do
        let relationStr = show relation
        let insightCount = countOccurrences "InsightExtracted" relationStr
        putStrLn $ "  Total InsightExtracted events: " ++ show insightCount

        if insightCount >= numThreads * eventsPerThread
          then do
            putStrLn ""
            putStrLn "=========================================="
            putStrLn "SUCCESS: All concurrent writes completed!"
            putStrLn "=========================================="
            pure True
          else do
            putStrLn ""
            putStrLn "=========================================="
            putStrLn $ "WARNING: Expected " ++ show (numThreads * eventsPerThread) ++ " events"
            putStrLn "=========================================="
            pure False

  case connResult of
    Left err -> error $ "DB Connection Error: " ++ show err
    Right False -> error "Test Failed"
    Right True -> pure ()

-- Writer Loop: 逐次的に書き込み処理を行う
writerLoop :: DbConn -> Chan WriterMsg -> MVar () -> IO ()
writerLoop conn chan doneMVar = loop
  where
    loop = do
      msg <- readChan chan
      case msg of
        MsgStop -> putMVar doneMVar ()
        MsgWrite event responseMVar -> do
          -- 書き込み実行 (Single Threadなのでロック不要)
          result <- withTransaction conn $ do
            case toInsertExpr [event] "events" of
              Left err -> error $ "toInsertExpr failed: " ++ show err
              Right insertExpr -> execute insertExpr

          -- 結果をクライアントに返す
          case result of
            Right () -> putMVar responseMVar True
            Left _ -> putMVar responseMVar False

          loop

-- Client Worker: イベントを生成してWriterに送信
clientWorker :: Chan WriterMsg -> Int -> IO Bool
clientWorker msgChan threadId = do
  sessionId <- nextRandom

  results <- forM [1 .. eventsPerThread] $ \eventNum -> do
    eventId <- nextRandom
    now <- getCurrentTime

    let event =
          Event
            { eventId = eventId,
              sessionId = sessionId,
              timestamp = now,
              payload =
                InsightExtracted $
                  pack $
                    "Thread " ++ show threadId ++ " Event " ++ show eventNum
            }

    -- Writerに依頼
    responseMVar <- newEmptyMVar
    writeChan msgChan (MsgWrite event responseMVar)

    -- 結果待機
    takeMVar responseMVar

  pure (and results)

-- Helper: 文字列内のパターン出現回数をカウント
countOccurrences :: String -> String -> Int
countOccurrences needle haystack = length $ filter (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf :: String -> String -> Bool
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys
    isPrefixOf _ _ = False

    tails :: [a] -> [[a]]
    tails [] = [[]]
    tails xs@(_ : ys) = xs : tails ys
