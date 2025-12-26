-- | Project M36 への永続化レイヤー
module Effect.Persistence
  ( -- * Connection
    DbConfig (..),
    withM36Connection,

    -- * Migration
    migrateSchema,

    -- * Types (re-export)
    DbConn,
    DbError,
  )
where

import Data.Proxy (Proxy (..))
import Domain.Types (Event, FactPayload, SessionContext)
import ProjectM36.Atomable (toAddTypeExpr)
import ProjectM36.Client
  ( ConnectionInfo (..),
    NotificationCallback,
    PersistenceStrategy (..),
  )
import ProjectM36.Client.Simple
  ( DbConn,
    DbError,
    close,
    execute,
    simpleConnectProjectM36,
    withTransaction,
  )
import ProjectM36.DatabaseContext (basicDatabaseContext)
import ProjectM36.Tupleable (toDefineExpr)

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
-- Migration
-------------------------------------------------------------------------------

-- | スキーマをマイグレーションする
-- 1. ADT 型を登録 (toAddTypeExpr)
-- 2. Event リレーション変数を定義 (toDefineExpr)
migrateSchema :: DbConn -> IO (Either DbError ())
migrateSchema conn = withTransaction conn $ do
  -- ADT 型の登録 (カラム値として使う Sum 型)
  execute (toAddTypeExpr (Proxy :: Proxy SessionContext))
  execute (toAddTypeExpr (Proxy :: Proxy FactPayload))

  -- Event リレーション変数の定義 (行として保存する Record 型)
  execute (toDefineExpr (Proxy :: Proxy Event) "events")

  pure ()
