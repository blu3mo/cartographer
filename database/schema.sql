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
  stream_type text not null check (char_length(stream_type) > 0), -- e.g. 'session', 'user'

  -- Concurrency Control
  -- Optimistic Locking: version must be sequential.
  version bigint not null check (version > 0),

  -- Payload
  -- The "Fact" itself.
  type text not null check (char_length(type) > 0),        -- e.g. 'SessionCreated'
  payload jsonb not null check (jsonb_typeof(payload) = 'object'),    -- Domain Data
  meta jsonb not null default '{}'::jsonb check (jsonb_typeof(meta) = 'object'), -- Audit Metadata (IP, UserAgent, etc)

  -- Global Ordering
  -- Strictly monotonic sequence for subscribers.
  global_seq bigint generated always as identity not null,

  -- Physical Timestamp
  -- When the server received the event.
  occurred_at timestamptz not null default now(),

  -- Causality
  -- The "Why" of the event. References the parent event or command.
  causation_id uuid references events(id),

  -- Constraints
  -- One version per stream.
  unique (stream_id, version)
);

-- Performance Indexes -------------------------------------------------------

-- 1. Fast Replay: "Give me all events for Stream X in order"
create index if not exists events_stream_idx on events (stream_id, version);

-- 2. Global Subscription: "Give me everything that happened since Seq N"
create index if not exists events_global_seq_idx on events (global_seq);

-- 3. Causality Lookups: "What events did this command cause?"
create index if not exists events_causation_idx on events (causation_id);
