{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Web.Server
  ( server,
    app,
    interpretServer,
    AppConfig (..),
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Domain.Types
  ( Event (..),
    EventId,
    FactPayload (..),
    SessionBackground (..),
    SessionContext (..),
    SessionId,
    SessionPurpose (..),
    SessionTitle (..),
    Statement (..),
    StatementContent (..),
  )
import Effect.Persistence (DbConfig, Persistence, runPersistence, saveEvents)
import Effect.Projection (PgConfig, Projection (..), projectEvents, runProjection)
import Network.Wai (Application)
import Polysemy
import Polysemy.Error
import Servant
import Web.API
import Web.Types (CreateSessionRequest (..), CreateSessionResponse (..))

-- | アプリケーション設定
data AppConfig = AppConfig
  { m36Config :: DbConfig,
    pgConfig :: Maybe PgConfig -- PostgreSQLはオプショナル
  }

server ::
  ( Member Persistence r,
    Member Projection r,
    Member (Embed IO) r,
    Member (Error ServerError) r
  ) =>
  ServerT API (Sem r)
server = healthHandler :<|> sessionsHandler

app :: AppConfig -> Application
app config = serve (Proxy @API) (hoistServer (Proxy @API) (interpretServer config) server)

interpretServer :: AppConfig -> Sem '[Persistence, Projection, Error ServerError, Embed IO] a -> Handler a
interpretServer config sem = do
  res <- liftIO $ runM $ runError $ runProjectionMaybe config.pgConfig $ runPersistence config.m36Config sem
  case res of
    Left err -> throwError err
    Right val -> pure val

-- | PostgreSQL Projection をオプショナルに実行
-- pgConfig が Nothing の場合は no-op
runProjectionMaybe :: (Member (Embed IO) r) => Maybe PgConfig -> Sem (Projection : r) a -> Sem r a
runProjectionMaybe Nothing = interpret $ \case
  ProjectEvent _ -> pure $ Right () -- PostgreSQL 未設定時は成功を返す
  ProjectEvents _ -> pure $ Right () -- PostgreSQL 未設定時は成功を返す
runProjectionMaybe (Just cfg) = runProjection cfg

healthHandler :: Sem r Text
healthHandler = pure "OK"

sessionsHandler ::
  ( Member Persistence r,
    Member Projection r,
    Member (Embed IO) r,
    Member (Error ServerError) r
  ) =>
  CreateSessionRequest ->
  Sem r CreateSessionResponse
sessionsHandler req = do
  sid <- embed nextRandom
  eid <- embed nextRandom
  tokenId <- embed nextRandom
  now <- embed getCurrentTime

  -- Convert API Request (Text) to Domain Type (SessionContext)
  let context =
        SessionContext
          { title = SessionTitle req.title,
            purpose = SessionPurpose req.purpose,
            background = SessionBackground req.background,
            hostUserId = req.hostUserId,
            adminAccessToken = tokenId
          }
      payload = ContextDefined context
      contextEvent =
        Event
          { eventId = eid,
            parentId = Nothing, -- 最初のイベントは親なし
            sessionId = sid,
            timestamp = now,
            payload = payload
          }

  -- statementEventsは前のイベントを親として参照
  (statementEvents, _lastEid) <- case req.initialQuestions of
    Nothing -> pure ([], eid)
    Just questions -> buildStatementEvents sid now eid questions

  let allEvents = contextEvent : statementEvents

  -- 1. M36に保存
  m36Result <- saveEvents allEvents
  case m36Result of
    Left _err -> throw err500
    Right () -> do
      -- 2. PostgreSQLに Projection（エラーは警告のみ）
      pgResult <- projectEvents allEvents
      case pgResult of
        Left err ->
          embed $ putStrLn $ "Projection warning: " <> show err
        Right () ->
          pure ()
      pure $ CreateSessionResponse sid tokenId

-- | Statement イベントを構築（前のイベントを parentId として参照）
buildStatementEvents ::
  (Member (Embed IO) r) =>
  SessionId ->
  UTCTime ->
  EventId ->
  [Text] ->
  Sem r ([Event], EventId)
buildStatementEvents _sid _now lastEid [] = pure ([], lastEid)
buildStatementEvents sid now lastEid (q : qs) = do
  sId <- embed nextRandom
  eId <- embed nextRandom
  let statement = Statement {id = sId, content = StatementContent q}
      event =
        Event
          { eventId = eId,
            parentId = Just lastEid, -- 前のイベントを親として参照
            sessionId = sid,
            timestamp = now,
            payload = StatementAdded statement
          }
  (rest, finalEid) <- buildStatementEvents sid now eId qs
  pure (event : rest, finalEid)
