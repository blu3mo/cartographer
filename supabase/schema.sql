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
