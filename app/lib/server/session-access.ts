import { supabase } from "@/lib/supabase";

export class SessionAccessError extends Error {
  status: number;

  constructor(message: string, status: number) {
    super(message);
    this.status = status;
  }
}

export type SessionHostRow = {
  id: string;
  title: string;
  context: string;
  is_public: boolean;
  host_user_id: string;
};

export async function requireSessionHost(
  sessionId: string,
  userId: string,
): Promise<SessionHostRow> {
  const { data: session, error } = await supabase
    .from("sessions")
    .select("id, title, context, is_public, host_user_id")
    .eq("id", sessionId)
    .single();

  if (error || !session) {
    if (error?.code === "PGRST116") {
      throw new SessionAccessError("Session not found", 404);
    }
    console.error("Failed to fetch session:", error);
    throw new SessionAccessError("Failed to load session", 500);
  }

  if (session.host_user_id !== userId) {
    throw new SessionAccessError("Forbidden", 403);
  }

  return session;
}
