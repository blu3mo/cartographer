alter table "public"."responses" add column "updated_at" timestamp with time zone not null default now();

alter table "public"."session_reports" alter column "model" set default 'google/gemini-3-flash-preview'::text;


