-- Migration: ensure responses table tracks update timestamps

do $$
begin
  if exists (select from pg_tables where schemaname = 'public' and tablename = 'responses') then
    alter table public.responses
      add column if not exists updated_at timestamptz not null default now();

    -- backfill in case column was added without data
    update public.responses
      set updated_at = coalesce(updated_at, created_at);
  end if;
end $$;
