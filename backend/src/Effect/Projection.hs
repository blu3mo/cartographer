{-# LANGUAGE TemplateHaskell #-}

-- | PostgreSQL への Projection レイヤー
-- M36 のイベントを PostgreSQL（Supabase）に同期する
module Effect.Projection
  ( -- * Effect
    Projection (..),
    projectEvent,
    projectEvents,

    -- * Types
    ProjectionError (..),
    PgConfig (..),

    -- * Interpreter
    runProjection,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.ByteString.Char8 qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Database.PostgreSQL.Simple
  ( Connection,
    Only (..),
    close,
    connectPostgreSQL,
    execute,
  )
import Domain.Types
  ( Event (..),
    FactPayload (..),
    SessionBackground (..),
    SessionContext (..),
    SessionId,
    SessionPurpose (..),
    SessionTitle (..),
    Statement (..),
    StatementContent (..),
  )
import Polysemy (Embed, Member, Sem, embed, interpret, makeSem)

-------------------------------------------------------------------------------
-- Effect Definition
-------------------------------------------------------------------------------

data ProjectionError
  = PgConnectionError Text
  | PgQueryError Text
  deriving (Show, Eq)

data Projection m a where
  ProjectEvent :: Event -> Projection m (Either ProjectionError ())
  ProjectEvents :: [Event] -> Projection m (Either ProjectionError ())

makeSem ''Projection

-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

-- | PostgreSQL 接続設定
newtype PgConfig = PgConfig
  { connectionString :: BS.ByteString
  }
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Interpreter
-------------------------------------------------------------------------------

-- | Projection インタープリター
-- 注意: 簡易実装として毎回接続を開閉。本番では接続プールを使用すべき。
runProjection :: (Member (Embed IO) r) => PgConfig -> Sem (Projection : r) a -> Sem r a
runProjection config = interpret $ \case
  ProjectEvent event -> embed $ projectEventIO config event
  ProjectEvents events -> embed $ projectEventsIO config events

-- | 単一イベントを Projection
projectEventIO :: PgConfig -> Event -> IO (Either ProjectionError ())
projectEventIO config event = do
  connResult <- try $ connectPostgreSQL config.connectionString
  case connResult of
    Left (err :: SomeException) ->
      pure $ Left $ PgConnectionError $ T.pack $ show err
    Right conn -> do
      result <- try $ projectEventToDb conn event
      close conn
      case result of
        Left (err :: SomeException) ->
          pure $ Left $ PgQueryError $ T.pack $ show err
        Right () -> pure $ Right ()

-- | 複数イベントを Projection
projectEventsIO :: PgConfig -> [Event] -> IO (Either ProjectionError ())
projectEventsIO config events = do
  connResult <- try $ connectPostgreSQL config.connectionString
  case connResult of
    Left (err :: SomeException) ->
      pure $ Left $ PgConnectionError $ T.pack $ show err
    Right conn -> do
      result <- try $ forM_ events (projectEventToDb conn)
      close conn
      case result of
        Left (err :: SomeException) ->
          pure $ Left $ PgQueryError $ T.pack $ show err
        Right () -> pure $ Right ()

-------------------------------------------------------------------------------
-- Database Operations
-------------------------------------------------------------------------------

-- | イベントを PostgreSQL に書き込み
projectEventToDb :: Connection -> Event -> IO ()
projectEventToDb conn event =
  case event.payload of
    ContextDefined ctx ->
      insertSession conn event.sessionId ctx
    StatementAdded stmt ->
      insertStatement conn event.sessionId stmt
    ReportGenerated _ _ ->
      -- ReportGenerated は今回スキップ
      pure ()

-- | セッションを INSERT
insertSession :: Connection -> SessionId -> SessionContext -> IO ()
insertSession conn sessionId ctx = do
  let SessionTitle titleText = ctx.title
      SessionPurpose purposeText = ctx.purpose
      SessionBackground bgText = ctx.background
      -- context = purpose + background を結合
      contextText = purposeText <> "\n\n" <> bgText
      hostId = ctx.hostUserId
  _ <-
    execute
      conn
      "INSERT INTO sessions (id, title, context, host_user_id, goal, is_public, admin_access_token) \
      \VALUES (?, ?, ?, ?, '', true, ?) \
      \ON CONFLICT (id) DO NOTHING"
      (sessionId, titleText, contextText, hostId, ctx.adminAccessToken)
  pure ()

-- | ステートメントを INSERT
insertStatement :: Connection -> SessionId -> Statement -> IO ()
insertStatement conn sessionId stmt = do
  let StatementContent textContent = stmt.content
      stmtId = stmt.id
  _ <-
    execute
      conn
      "INSERT INTO statements (id, session_id, text, order_index) \
      \VALUES (?, ?, ?, 0) \
      \ON CONFLICT (id) DO NOTHING"
      (stmtId, sessionId, textContent)
  pure ()
