{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Web.Server where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
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
import Effect.Persistence (DbConfig, Persistence, runPersistence, saveEvent)
import Network.Wai (Application)
import Polysemy
import Polysemy.Error
import Servant
import Web.API
import Web.Types (CreateSessionRequest (..), CreateSessionResponse (..))

server ::
  ( Member Persistence r,
    Member (Embed IO) r,
    Member (Error ServerError) r
  ) =>
  ServerT API (Sem r)
server = healthHandler :<|> sessionsHandler

app :: DbConfig -> Application
app dbConfig = serve (Proxy @API) (hoistServer (Proxy @API) (interpretServer dbConfig) server)

interpretServer :: DbConfig -> Sem '[Persistence, Error ServerError, Embed IO] a -> Handler a
interpretServer dbConfig sem = do
  res <- liftIO $ runM $ runError $ runPersistence dbConfig sem
  case res of
    Left err -> throwError err
    Right val -> pure val

healthHandler :: Sem r Text
healthHandler = pure "OK"

sessionsHandler ::
  ( Member Persistence r,
    Member (Embed IO) r,
    Member (Error ServerError) r
  ) =>
  CreateSessionRequest ->
  Sem r CreateSessionResponse
sessionsHandler req = do
  sid <- embed nextRandom
  eid <- embed nextRandom
  now <- embed getCurrentTime

  -- Convert API Request (Text) to Domain Type (SessionContext)
  let context =
        SessionContext
          { title = SessionTitle req.title,
            purpose = SessionPurpose req.purpose,
            background = SessionBackground req.background,
            hostUserId = req.hostUserId
          }
      payload = ContextDefined context
      event =
        Event
          { eventId = eid,
            sessionId = sid,
            timestamp = now,
            payload = payload
          }

  res <- saveEvent event
  case res of
    Left _err -> throw err500
    Right () -> pure $ CreateSessionResponse sid
