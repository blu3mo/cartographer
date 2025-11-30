-- Disable auto-generation default for new event threads
alter table public.event_threads
  alter column should_proceed set default false;
