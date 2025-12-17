{-# LANGUAGE OverloadedStrings #-}

module Feature.Session.Handler
  ( sessionServer
  , createSession
  , getSessionEvents
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Servant

import Cartographer.Event
import Cartographer.EventStore (appendEvent, getEventsByAggregate)
import Feature.Session.API (SessionAPI)

-- | Session server implementation
-- Note: This is a simplified version. In production, you'd inject the M36 connection.
sessionServer :: ServerT SessionAPI Handler
sessionServer = createSession :<|> getSessionEvents

-- | Create a new session
-- This generates a SessionCreated event and stores it in the event store
createSession :: CreateSessionCommand -> Handler EventEnvelope
createSession cmd = do
  -- Generate a new session ID
  newSessionId <- liftIO nextRandom
  
  -- Create the SessionCreated event
  let sessionEvent = SessionCreated
        { sessionId = newSessionId
        , sessionName = cmd.name
        , sessionDescription = cmd.description
        }
  
  -- Wrap in the global Event type
  let event = SessionEventWrapper sessionEvent
  
  -- TODO: Get M36 connection from environment
  -- For now, this is a placeholder that shows the structure
  -- In production: appendEvent sessionId conn newSessionId event
  
  -- Placeholder response (will be replaced with actual M36 integration)
  liftIO $ do
    eventId <- nextRandom
    currentTime <- Data.Time.getCurrentTime
    pure EventEnvelope
      { eventId = eventId
      , event = event
      , occurredAt = currentTime
      , aggregateId = newSessionId
      , version = 1
      }

-- | Get all events for a session
getSessionEvents :: UUID -> Handler [EventEnvelope]
getSessionEvents sessionAggregateId = do
  -- TODO: Get M36 connection from environment
  -- For now, return empty list as placeholder
  -- In production: getEventsByAggregate sessionId conn sessionAggregateId
  pure []
