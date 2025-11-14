import { supabase } from "@/lib/supabase";

export type PublicSession = {
  id: string;
  title: string;
  context: string;
  goal: string;
  hostUserId: string;
  createdAt: string;
  _count: {
    participants: number;
    statements: number;
  };
};

type SessionRow = {
  id: string;
  title: string;
  context: string;
  goal: string;
  host_user_id: string;
  created_at: string;
  participants?: { user_id: string }[] | null;
  statements?: { id: string }[] | null;
};

export async function getPublicSessions(): Promise<PublicSession[]> {
  const { data: sessionsData, error: sessionsError } = await supabase
    .from("sessions")
    .select(
      `
        id,
        title,
        context,
        goal,
        host_user_id,
        created_at,
        participants ( user_id ),
        statements ( id )
      `,
    )
    .eq("is_public", true)
    .order("created_at", { ascending: false });

  if (sessionsError) {
    console.error("Failed to fetch public sessions:", sessionsError);
    throw new Error("Failed to fetch public sessions");
  }

  return (sessionsData ?? []).map((session: SessionRow) => ({
    id: session.id,
    title: session.title,
    context: session.context,
    goal: session.goal,
    hostUserId: session.host_user_id,
    createdAt: session.created_at,
    _count: {
      participants: (session.participants ?? []).length,
      statements: (session.statements ?? []).length,
    },
  }));
}
