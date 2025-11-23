-- Migration: introduce free-text responses
-- Adds response_type, allows nullable value, and stores free-form text.

alter table if exists public.responses
  add column if not exists response_type text not null default 'scale'
    check (response_type in ('scale', 'free_text'));

alter table if exists public.responses
  alter column value drop not null;

alter table if exists public.responses
  add column if not exists text_response text null;
