{-# LANGUAGE TemplateHaskell #-}

-- | Project M36 への永続化レイヤー
module Effect.Persistence
  ( -- * Effect
    Persistence (..),
    saveEvent,
    saveEvents,

    -- * Types
    PersistenceError (..),

    -- * Interpreter
    runPersistence,

    -- * Connection
    DbConfig (..),
    withM36Connection,

    -- * Migration
    migrateSchema,
    migrateSchemaIfNeeded,

    -- * Types (re-export)
    DbConn,
    DbError,
  )
where

import Control.Monad (join)
import Data.Proxy (Proxy (..))
import Domain.Types
  ( Event,
    FactPayload,
    SessionBackground,
    SessionContext,
    SessionPurpose,
    SessionTitle,
    Statement,
    StatementContent,
  )
import Polysemy (Embed, Member, Sem, embed, interpret, makeSem)
import ProjectM36.Atomable (toAddTypeExpr)
import ProjectM36.Base (Relation, RelationCardinality (..), RelationalExprBase (..))
import ProjectM36.Client
  ( ConnectionInfo (..),
    NotificationCallback,
    PersistenceStrategy (..),
  )
import ProjectM36.Client.Simple
  ( DbConn,
    DbError (..),
    close,
    execute,
    query,
    simpleConnectProjectM36,
    withTransaction,
  )
import ProjectM36.DatabaseContext (basicDatabaseContext)
import ProjectM36.Error (RelationalError)
import ProjectM36.Relation (cardinality)
import ProjectM36.Tupleable (toDefineExpr, toInsertExpr)

-------------------------------------------------------------------------------
-- Effect Definition
-------------------------------------------------------------------------------

data PersistenceError
  = ConnectionError DbError
  | QueryError RelationalError
  deriving (Show, Eq)

data Persistence m a where
  SaveEvent :: Event -> Persistence m (Either PersistenceError ())
  SaveEvents :: [Event] -> Persistence m (Either PersistenceError ())

makeSem ''Persistence

-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

-- | データベース接続設定
data DbConfig
  = -- | インメモリ（テスト用）
    InMemory
  | -- | ファイル永続化
    Persistent FilePath
  deriving (Eq, Show)

-- | NotificationCallback のデフォルト（何もしない）
emptyNotificationCallback :: NotificationCallback
emptyNotificationCallback _ _ = pure ()

-- | DbConfig から ConnectionInfo へ変換
toConnectionInfo :: DbConfig -> ConnectionInfo
toConnectionInfo InMemory =
  InProcessConnectionInfo NoPersistence emptyNotificationCallback [] basicDatabaseContext
toConnectionInfo (Persistent path) =
  InProcessConnectionInfo (CrashSafePersistence path) emptyNotificationCallback [] basicDatabaseContext

-------------------------------------------------------------------------------
-- Connection Management
-------------------------------------------------------------------------------

-- | M36 接続を開き、アクションを実行し、閉じる
withM36Connection :: DbConfig -> (DbConn -> IO a) -> IO (Either DbError a)
withM36Connection config action = do
  eConn <- simpleConnectProjectM36 (toConnectionInfo config)
  case eConn of
    Left err -> pure (Left err)
    Right conn -> do
      result <- action conn
      close conn
      pure (Right result)

-------------------------------------------------------------------------------
-- Interpreter
-------------------------------------------------------------------------------

-- | NOTE: 本来は Resource Effect などで接続を管理すべきだが、
-- 簡易的に毎回接続を開閉する実装とする (または呼び出し元で管理された接続を使う形が望ましいが、要件次第)
-- ここではシンプルに `withM36Connection` を都度呼ぶ形にするが、パフォーマンス上の懸念がある場合は
-- 接続プールやブラケットパターンを検討すること。
runPersistence :: (Member (Embed IO) r) => DbConfig -> Sem (Persistence : r) a -> Sem r a
runPersistence config = interpret $ \case
  SaveEvent event -> embed $ do
    case toInsertExpr [event] "events" of
      Left err -> pure (Left (QueryError err))
      Right expr -> do
        res <- withM36Connection config (\conn -> withTransaction conn (execute expr))
        -- res :: Either DbError (Either DbError ())
        -- flatten inner Either first (execute returns Right () on success inside IO?)
        -- withTransaction returns IO (Either DbError a)
        -- execute returns Db ()
        -- So result is IO (Either DbError ())
        -- withM36Connection returns IO (Either DbError (Either DbError ()))
        -- join res :: Either DbError ()
        pure $ case join res of -- join flattens Either DbError (Either DbError ()) -> Either DbError ()? No.
        -- withTransaction signature: (DbConn -> Db a -> IO (Either DbError a))
        -- execute returns Db ()
        -- So result is IO (Either DbError ())
        -- withM36Connection returns IO (Either DbError (Either DbError ()))
        -- join res :: Either DbError ()
          Left err -> Left (ConnectionError err)
          Right () -> Right ()
  SaveEvents events -> embed $ do
    case toInsertExpr events "events" of
      Left err -> pure (Left (QueryError err))
      Right expr -> do
        res <- withM36Connection config (\conn -> withTransaction conn (execute expr))
        pure $ case join res of
          Left err -> Left (ConnectionError err)
          Right () -> Right ()

-------------------------------------------------------------------------------
-- Migration
-------------------------------------------------------------------------------

-- | スキーマをマイグレーションする
-- 1. ADT 型を登録 (toAddTypeExpr)
-- 2. Event リレーション変数を定義 (toDefineExpr)
migrateSchema :: DbConn -> IO (Either DbError ())
migrateSchema conn = withTransaction conn $ do
  -- Newtype 型の登録 (SessionContext の依存型)
  -- M36はネストした型を自動登録しないため、依存順序に従って明示的に登録
  execute (toAddTypeExpr (Proxy :: Proxy SessionTitle))
  execute (toAddTypeExpr (Proxy :: Proxy SessionPurpose))

  execute (toAddTypeExpr (Proxy :: Proxy SessionBackground))
  execute (toAddTypeExpr (Proxy :: Proxy StatementContent))
  execute (toAddTypeExpr (Proxy :: Proxy Statement))

  -- ADT 型の登録 (カラム値として使う複合型)
  execute (toAddTypeExpr (Proxy :: Proxy SessionContext))
  execute (toAddTypeExpr (Proxy :: Proxy FactPayload))

  -- Event リレーション変数の定義 (行として保存する Record 型)
  execute (toDefineExpr (Proxy :: Proxy Event) "events")

  pure ()

-- | 条件付きスキーママイグレーション
-- eventsリレーション変数が存在しない場合のみマイグレーションを実行
-- 存在する場合は何もせずに成功を返す（冪等）
migrateSchemaIfNeeded :: DbConn -> IO (Either DbError ())
migrateSchemaIfNeeded conn = do
  -- まずeventsリレーション変数の存在をチェック
  checkResult <- withTransaction conn $ do
    query (RelationVariable "events" ())
  case checkResult of
    Right _ -> do
      -- eventsクエリ成功 → スキーマ既存、何もしない
      pure (Right ())
    Left _ -> do
      -- クエリエラー → スキーマ未定義の可能性、マイグレーション実行
      migrateSchema conn
