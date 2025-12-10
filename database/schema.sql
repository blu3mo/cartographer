-- Pure Event Sourcing Schema (Standard PostgreSQL 17+)
-- This schema depends on native uuidv7() function available in Postgres 17+.

--- EVENT STORE --------------------------------------------------------------
-- A pure, append-only log of domain facts.
-- No dependencies on 'auth.users' or other external tables.

create table if not exists events (
  -- Physical ID (Sorting & Idempotency)
  -- Uses native UUID v7 for Index Locality (Time-ordered insertions)
  id uuid primary key default uuidv7(),

  -- Stream Identity
  -- The "Target" of the event. Applications define what this ID means.
  stream_id uuid not null,
  stream_type text not null, -- e.g. 'session', 'user'

  -- Concurrency Control
  -- Optimistic Locking: version must be sequential.
  version bigint not null check (version > 0),

  -- Payload
  -- The "Fact" itself.
  type text not null,        -- e.g. 'SessionCreated'
  payload jsonb not null,    -- Domain Data
  meta jsonb not null default '{}'::jsonb, -- Audit Metadata (IP, UserAgent, etc)

  -- Global Ordering
  -- Strictly monotonic sequence for subscribers.
  global_seq bigint generated always as identity not null,

  -- Physical Timestamp
  -- When the server received the event.
  occurred_at timestamptz not null default now(),

  -- Constraints
  -- One version per stream.
  unique (stream_id, version)
);

-- Performance Indexes -------------------------------------------------------

-- 1. Fast Replay: "Give me all events for Stream X in order"
create index if not exists events_stream_idx on events (stream_id, version);

-- 2. Global Subscription: "Give me everything that happened since Seq N"
create index if not exists events_global_seq_idx on events (global_seq);
