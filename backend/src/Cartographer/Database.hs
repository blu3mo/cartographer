module Cartographer.Database where

import Cartographer.Domain as Domain
import Control.Monad (void)
import Data.Proxy (Proxy (..))
import ProjectM36.Client as M36

-- | Initialize the database connection and schema
-- Returns a session ID and connection for use in the application
initDatabase :: IO (M36.SessionId, M36.Connection)
initDatabase = do
  -- Connect in-process with persistence to "data/m36"
  -- This allows data to survive restarts.
  let connInfo = InProcessConnectionInfo (CrashSafePersistence "data/m36") emptyNotificationCallback [] basicDatabaseContext
  eConn <- connectProjectM36 connInfo
  case eConn of
    Left err -> error $ "Failed to connect to M36: " ++ show err
    Right conn -> do
      -- Create a session on the default branch "master"
      eSessionId <- createSessionAtHead conn "master"
      case eSessionId of
        Left err -> error $ "Failed to create session: " ++ show err
        Right sessionId -> do
          syncSchema sessionId conn
          return (sessionId, conn)

-- | Sync the Haskell domain schema with the database
-- This effectively performs "Code-First Migration" by registering types
-- and defining necessary relations if they don't exist.
syncSchema :: M36.SessionId -> M36.Connection -> IO ()
syncSchema sessionId conn = do
  -- 1. Register Domain Types
  -- We attempt to register all types. If they already exist, M36 returns an error,
  -- which we log as a warning but proceed (idempotent-ish).
  let types =
        [ toAddTypeExpr (Proxy @SessionStatus),
          toAddTypeExpr (Proxy @QuestionType),
          toAddTypeExpr (Proxy @Domain.SessionId),
          toAddTypeExpr (Proxy @Domain.UserId),
          toAddTypeExpr (Proxy @Domain.QuestionId),
          toAddTypeExpr (Proxy @CartographerEvent)
        ]

  mapM_ (executeExpr sessionId conn) types

  -- 2. Define "events" RelVar
  -- Schema: { session_id :: SessionId, order_index :: Int, event :: CartographerEvent }
  -- We include `order_index` to strictly order events within a session.
  let eventsAttrs =
        [ NakedAttributeExpr (Attribute "session_id" (toAtomType (Proxy @Domain.SessionId))),
          NakedAttributeExpr (Attribute "order_index" IntAtomType),
          NakedAttributeExpr (Attribute "event" (toAtomType (Proxy @CartographerEvent)))
        ]

  let defineEvents = Define "events" eventsAttrs
  executeExpr sessionId conn defineEvents

  return ()

-- | Helper to execute a Context Expression and print errors softly
executeExpr :: M36.SessionId -> M36.Connection -> DatabaseContextExpr -> IO ()
executeExpr sid conn expr = do
  res <- executeDatabaseContextExpr sid conn expr
  case res of
    Left err -> putStrLn $ "Schema Sync Info: " ++ show err
    Right _ -> return ()
