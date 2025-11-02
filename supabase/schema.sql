-- Enable uuid generation helpers (available by default on Supabase)
create extension if not exists "pgcrypto";

-- Sessions ------------------------------------------------------------------
create table if not exists public.sessions (
  id uuid primary key default gen_random_uuid(),
  title text not null,
  context text not null,
  is_public boolean not null default true,
  host_user_id uuid not null,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);

create index if not exists sessions_host_user_idx on public.sessions (host_user_id);
create index if not exists sessions_created_at_idx on public.sessions (created_at desc);

-- Statements ----------------------------------------------------------------
create table if not exists public.statements (
  id uuid primary key default gen_random_uuid(),
  session_id uuid not null references public.sessions(id) on delete cascade,
  text text not null,
  order_index integer not null default 0,
  created_at timestamptz not null default now()
);

create index if not exists statements_session_idx on public.statements (session_id);
create index if not exists statements_session_order_idx on public.statements (session_id, order_index);

-- Participants ---------------------------------------------------------------
create table if not exists public.participants (
  user_id uuid not null,
  session_id uuid not null references public.sessions(id) on delete cascade,
  name varchar(255) not null,
  latest_individual_report_id uuid null,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  primary key (user_id, session_id)
);

create index if not exists participants_session_idx on public.participants (session_id);

-- Responses -----------------------------------------------------------------
create table if not exists public.responses (
  id uuid primary key default gen_random_uuid(),
  participant_user_id uuid not null,
  session_id uuid not null,
  statement_id uuid not null references public.statements(id) on delete cascade,
  value integer not null,
  created_at timestamptz not null default now(),
  unique (participant_user_id, session_id, statement_id),
  constraint responses_participant_fk
    foreign key (participant_user_id, session_id)
    references public.participants(user_id, session_id)
    on delete cascade
);

create index if not exists responses_statement_idx on public.responses (statement_id);
create index if not exists responses_participant_idx on public.responses (participant_user_id, session_id);

-- Situation Analysis Reports -------------------------------------------------
create table if not exists public.situation_analysis_reports (
  id uuid primary key default gen_random_uuid(),
  session_id uuid not null references public.sessions(id) on delete cascade,
  content_markdown text not null,
  created_at timestamptz not null default now()
);

create index if not exists situation_reports_session_idx
  on public.situation_analysis_reports (session_id, created_at desc);

-- Individual Reports ---------------------------------------------------------
create table if not exists public.individual_reports (
  id uuid primary key default gen_random_uuid(),
  participant_user_id uuid not null,
  session_id uuid not null,
  content_markdown text not null,
  created_at timestamptz not null default now(),
  constraint individual_reports_participant_fk
    foreign key (participant_user_id, session_id)
    references public.participants(user_id, session_id)
    on delete cascade
);

create index if not exists individual_reports_participant_idx
  on public.individual_reports (participant_user_id, session_id, created_at desc);

-- Back-reference from participants to latest report -------------------------
alter table public.participants
  add constraint participants_latest_report_fk
  foreign key (latest_individual_report_id)
  references public.individual_reports(id)
  on delete set null;

-- Event Threads -------------------------------------------------------------
create table if not exists public.event_threads (
  id uuid primary key default gen_random_uuid(),
  session_id uuid not null references public.sessions(id) on delete cascade,
  should_proceed boolean not null default true,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  unique (session_id)
);

create index if not exists event_threads_session_idx
  on public.event_threads (session_id);

-- Agent Instances -----------------------------------------------------------
create table if not exists public.agent_instances (
  id uuid primary key default gen_random_uuid(),
  thread_id uuid not null references public.event_threads(id) on delete cascade,
  agent_type text not null,
  state text not null,
  state_payload jsonb not null default '{}'::jsonb,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now(),
  unique (thread_id, agent_type)
);

create index if not exists agent_instances_thread_idx
  on public.agent_instances (thread_id);


-- Events --------------------------------------------------------------------
create table if not exists public.events (
  id uuid primary key default gen_random_uuid(),
  thread_id uuid not null references public.event_threads(id) on delete cascade,
  type text not null,
  agent_id uuid null references public.agent_instances(id) on delete set null,
  user_id uuid null,
  progress numeric(3, 2) not null default 0,
  payload jsonb not null default '{}'::jsonb,
  order_index bigint generated by default as identity,
  created_at timestamptz not null default now(),
  updated_at timestamptz not null default now()
);

create index if not exists events_thread_order_idx
  on public.events (thread_id, order_index);


-- make sure WAL includes the whole row for updates
alter table public.event_threads replica identity full;
alter table public.agent_instances replica identity full;
alter table public.events replica identity full;
alter table public.responses replica identity full; -- if not already set

-- register the tables with the realtime publication
alter publication supabase_realtime add table public.event_threads;
alter publication supabase_realtime add table public.agent_instances;
alter publication supabase_realtime add table public.events;
alter publication supabase_realtime add table public.responses;

-- Migration helpers --------------------------------------------------------
alter table if exists public.sessions
  add column if not exists goal text not null default '';