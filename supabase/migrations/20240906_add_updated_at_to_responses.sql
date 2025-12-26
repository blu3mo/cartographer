-- Migration: ensure responses table tracks update timestamps

alter table if exists public.responses
  add column if not exists updated_at timestamptz not null default now();

-- backfill in case column was added without data
update public.responses
  set updated_at = coalesce(updated_at, created_at);
