{-# LANGUAGE OverloadedStrings #-}

module Cartographer.EventStore
  ( -- * Event Store Operations
    appendEvent
  , getEventsByAggregate
  , getAllEvents
    -- * Schema Setup
  , createEventsRelvar
  ) where

import Cartographer.Event
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import ProjectM36.Base
import ProjectM36.Client
import ProjectM36.Relation (relationFromList)

-- | RelVar name for the global event store
eventsRelvarName :: RelVarName
eventsRelvarName = "events"

-- | Create the events relvar (run once at startup)
-- Schema: {event_id UUID, event Event, occurred_at DateTime, aggregate_id UUID, version Int}
createEventsRelvar :: SessionId -> Connection -> IO (Either RelationalError ())
createEventsRelvar sessionId conn = do
  -- Define the relvar with proper attribute types
  -- Note: In M36, we need to use Atomable types
  let attrs = attributesFromList
        [ Attribute "event_id" (TypeVariableType "UUID")
        , Attribute "event" (TypeVariableType "Event")
        , Attribute "occurred_at" (TypeVariableType "DateTime")
        , Attribute "aggregate_id" (TypeVariableType "UUID")
        , Attribute "version" IntegerAtomType
        ]
  case attrs of
    Left err -> pure $ Left err
    Right a -> do
      let expr = Define eventsRelvarName (map NakedAttributeExpr $ toList a)
      executeDatabaseContextExpr sessionId conn expr

-- | Append a new event to the event store
appendEvent
  :: SessionId
  -> Connection
  -> UUID           -- ^ Aggregate ID
  -> Event          -- ^ The event to append
  -> IO (Either RelationalError EventEnvelope)
appendEvent sessionId conn aggregateId event = do
  eventId <- nextRandom
  occurredAt <- getCurrentTime
  
  -- Get the current version for this aggregate (for optimistic concurrency)
  -- For now, we'll use a simple incrementing version
  let version = 1  -- TODO: implement proper version tracking
  
  let envelope = EventEnvelope
        { eventId = eventId
        , event = event
        , occurredAt = occurredAt
        , aggregateId = aggregateId
        , version = version
        }
  
  -- Insert into the events relvar
  -- Using M36's Atomable instances to convert Haskell types directly
  let insertExpr = Insert eventsRelvarName $ 
        MakeRelationFromExprs Nothing
          [ TupleExpr $ fromList
              [ ("event_id", NakedAtomExpr $ toAtom eventId)
              , ("event", NakedAtomExpr $ toAtom event)
              , ("occurred_at", NakedAtomExpr $ toAtom occurredAt)
              , ("aggregate_id", NakedAtomExpr $ toAtom aggregateId)
              , ("version", NakedAtomExpr $ toAtom version)
              ]
          ]
  
  result <- executeDatabaseContextExpr sessionId conn insertExpr
  pure $ envelope <$ result

-- | Get all events for a specific aggregate
getEventsByAggregate
  :: SessionId
  -> Connection
  -> UUID           -- ^ Aggregate ID
  -> IO (Either RelationalError [EventEnvelope])
getEventsByAggregate sessionId conn aggregateId = do
  let queryExpr = Restrict
        (AttributeEqualityPredicate "aggregate_id" (NakedAtomExpr $ toAtom aggregateId))
        (RelationVariable eventsRelvarName ())
  
  result <- executeRelationalExpr sessionId conn queryExpr
  case result of
    Left err -> pure $ Left err
    Right rel -> pure $ Right $ relationToEnvelopes rel

-- | Get all events in the store
getAllEvents
  :: SessionId
  -> Connection
  -> IO (Either RelationalError [EventEnvelope])
getAllEvents sessionId conn = do
  let queryExpr = RelationVariable eventsRelvarName ()
  result <- executeRelationalExpr sessionId conn queryExpr
  case result of
    Left err -> pure $ Left err
    Right rel -> pure $ Right $ relationToEnvelopes rel

-- | Helper to convert a relation to a list of EventEnvelopes
-- This leverages M36's Atomable instances for automatic conversion
relationToEnvelopes :: Relation -> [EventEnvelope]
relationToEnvelopes rel = 
  -- M36 stores Haskell ADTs directly, so we can extract them
  -- using the Atomable instances
  case relationTuples rel of
    Left _ -> []
    Right tuples -> mapMaybe tupleToEnvelope tuples
  where
    tupleToEnvelope :: RelationTuple -> Maybe EventEnvelope
    tupleToEnvelope tuple = do
      eventIdAtom <- atomForAttributeName "event_id" tuple
      eventAtom <- atomForAttributeName "event" tuple
      occurredAtAtom <- atomForAttributeName "occurred_at" tuple
      aggregateIdAtom <- atomForAttributeName "aggregate_id" tuple
      versionAtom <- atomForAttributeName "version" tuple
      
      -- Using fromAtom to convert back to Haskell types
      pure EventEnvelope
        { eventId = fromAtom eventIdAtom
        , event = fromAtom eventAtom
        , occurredAt = fromAtom occurredAtAtom
        , aggregateId = fromAtom aggregateIdAtom
        , version = fromAtom versionAtom
        }
    
    atomForAttributeName :: AttributeName -> RelationTuple -> Maybe Atom
    atomForAttributeName name tuple = 
      either (const Nothing) Just $ atomForAttributeName name tuple
