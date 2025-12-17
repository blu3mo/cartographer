{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Cartographer.Event
  ( -- * Event Types
    Event (..)
  , EventId
  , EventEnvelope (..)
    -- * Session Events
  , SessionEvent (..)
  , SessionId
  , SessionName
  , SessionDescription
    -- * Commands (API Input)
  , CreateSessionCommand (..)
  ) where

import Codec.Winery (Serialise)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import ProjectM36.Atomable (Atomable)

-- | Event ID (UUID)
type EventId = UUID

-- | Session ID (UUID)
type SessionId = UUID

-- | Session name
type SessionName = Text

-- | Session description
type SessionDescription = Text

-- | Domain events for the Cartographer system
-- This is the sum type for ALL events in the system (global event store)
data Event
  = SessionEventWrapper SessionEvent
  -- | QuestionEventWrapper QuestionEvent  -- 将来の拡張用
  -- | AnswerEventWrapper AnswerEvent       -- 将来の拡張用
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, Serialise, Atomable)

-- | Session-specific events
data SessionEvent
  = SessionCreated
      { sessionId :: SessionId
      , sessionName :: SessionName
      , sessionDescription :: SessionDescription
      }
  | SessionOpened
      { sessionId :: SessionId
      }
  | SessionClosed
      { sessionId :: SessionId
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, Serialise, Atomable)

-- | Event Envelope - wraps an event with metadata for storage
data EventEnvelope = EventEnvelope
  { eventId :: EventId
  , event :: Event
  , occurredAt :: UTCTime
  , aggregateId :: UUID  -- Which aggregate this event belongs to
  , version :: Int       -- Event version for optimistic concurrency
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, Serialise, Atomable)

--------------------------------------------------------------------------------
-- Commands (API Input DTOs)
--------------------------------------------------------------------------------

-- | Command to create a new session (API input)
data CreateSessionCommand = CreateSessionCommand
  { name :: Text
  , description :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
