-- Migration: introduce free-text responses
-- Adds response_type, allows nullable value, and stores free-form text.

do $$
begin
  if exists (select from pg_tables where schemaname = 'public' and tablename = 'responses') then
    -- Add response_type column
    if not exists (
      select from information_schema.columns
      where table_schema = 'public' and table_name = 'responses' and column_name = 'response_type'
    ) then
      alter table public.responses
        add column response_type text not null default 'scale'
          check (response_type in ('scale', 'free_text'));
    end if;

    -- Make value nullable
    alter table public.responses
      alter column value drop not null;

    -- Add text_response column
    if not exists (
      select from information_schema.columns
      where table_schema = 'public' and table_name = 'responses' and column_name = 'text_response'
    ) then
      alter table public.responses
        add column text_response text null;
    end if;
  end if;
end $$;
