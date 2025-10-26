import { supabase } from "@/lib/supabase";

type SessionForThread = {
  id: string;
  context: string;
  host_user_id: string;
  title?: string;
};

export type EventThreadRecord = {
  id: string;
  session_id: string;
  should_proceed: boolean;
  created_at: string;
  updated_at: string;
};

async function ensurePtolemyAgentInstance(threadId: string) {
  const { data: existingInstance, error: lookupError } = await supabase
    .from("agent_instances")
    .select("id")
    .eq("thread_id", threadId)
    .eq("agent_type", "ptolemy")
    .maybeSingle();

  if (lookupError && lookupError.code !== "PGRST116") {
    throw new Error(
      `Failed to verify Ptolemy agent instance: ${lookupError.message}`,
    );
  }

  if (existingInstance) {
    return existingInstance;
  }

  const { data: instance, error: createError } = await supabase
    .from("agent_instances")
    .insert({
      thread_id: threadId,
      agent_type: "ptolemy",
      state: "CREATING_PLAN",
      state_payload: { initializedAt: new Date().toISOString() },
    })
    .select("id")
    .single();

  if (createError || !instance) {
    throw new Error(`Failed to create Ptolemy agent: ${createError?.message}`);
  }

  return instance;
}

async function createBootstrapUserMessage(
  thread: EventThreadRecord,
  session: SessionForThread,
) {
  const { error } = await supabase.from("events").insert({
    thread_id: thread.id,
    type: "user_message",
    user_id: session.host_user_id,
    progress: 1,
    payload: {
      markdown: session.context,
      metadata: {
        kind: "session_context",
        createdBy: "system",
        title: session.title ?? "Session Context",
      },
    },
  });

  if (error) {
    throw new Error(
      `Failed to seed thread with initial UserMessage: ${error.message}`,
    );
  }
}

export async function ensureEventThreadForSession(
  session: SessionForThread,
): Promise<EventThreadRecord> {
  const { data: existingThread, error: lookupError } = await supabase
    .from("event_threads")
    .select("id, session_id, should_proceed, created_at, updated_at")
    .eq("session_id", session.id)
    .maybeSingle();

  if (lookupError && lookupError.code !== "PGRST116") {
    throw new Error(
      `Failed to verify event thread: ${lookupError.message ?? "unknown error"}`,
    );
  }

  if (existingThread) {
    await ensurePtolemyAgentInstance(existingThread.id);
    return existingThread;
  }

  const { data: createdThread, error: createError } = await supabase
    .from("event_threads")
    .insert({
      session_id: session.id,
      should_proceed: true,
    })
    .select("id, session_id, should_proceed, created_at, updated_at")
    .single();

  if (createError || !createdThread) {
    throw new Error(
      `Failed to provision event thread: ${createError?.message ?? "unknown error"}`,
    );
  }

  await createBootstrapUserMessage(createdThread, session);
  await ensurePtolemyAgentInstance(createdThread.id);

  return createdThread;
}
