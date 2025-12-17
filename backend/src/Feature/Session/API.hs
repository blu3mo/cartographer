{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Feature.Session.API
  ( SessionAPI
  , sessionAPI
  ) where

import Data.Proxy (Proxy (..))
import Data.UUID (UUID)
import Servant

import Cartographer.Event (CreateSessionCommand, EventEnvelope)

-- | Session API endpoints
type SessionAPI =
  -- POST /sessions - Create a new session (emits SessionCreated event)
  "sessions" :> ReqBody '[JSON] CreateSessionCommand :> Post '[JSON] EventEnvelope
  :<|>
  -- GET /sessions/:id/events - Get all events for a session
  "sessions" :> Capture "id" UUID :> "events" :> Get '[JSON] [EventEnvelope]

sessionAPI :: Proxy SessionAPI
sessionAPI = Proxy
