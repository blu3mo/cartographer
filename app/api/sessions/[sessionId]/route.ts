import { type NextRequest, NextResponse } from "next/server";
import { getUserIdFromRequest } from "@/lib/auth";
import { supabase } from "@/lib/supabase";

type SessionRow = {
  id: string;
  title: string;
  context: string;
  is_public: boolean;
  host_user_id: string;
  created_at: string;
  updated_at: string;
  participants?: { user_id: string }[] | null;
};

function mapSession(row: SessionRow) {
  return {
    id: row.id,
    title: row.title,
    context: row.context,
    isPublic: row.is_public,
    hostUserId: row.host_user_id,
    createdAt: row.created_at,
    updatedAt: row.updated_at,
  };
}

export async function GET(
  request: NextRequest,
  { params }: { params: Promise<{ sessionId: string }> },
) {
  try {
    const { sessionId } = await params;
    const userId = getUserIdFromRequest(request);

    if (!userId) {
      return NextResponse.json(
        { error: "Unauthorized: Missing user ID" },
        { status: 401 },
      );
    }

    const { data: session, error: sessionError } = await supabase
      .from("sessions")
      .select(
        `
          id,
          title,
          context,
          is_public,
          host_user_id,
          created_at,
          updated_at,
          participants ( user_id )
        `,
      )
      .eq("id", sessionId)
      .single();

    if (sessionError || !session) {
      if (sessionError?.code === "PGRST116") {
        return NextResponse.json(
          { error: "Session not found" },
          { status: 404 },
        );
      }
      console.error("Failed to fetch session:", sessionError);
      return NextResponse.json({ error: "Session not found" }, { status: 404 });
    }

    const mappedSession = mapSession(session);
    const isHost = mappedSession.hostUserId === userId;
    const participants = session.participants ?? [];
    const isParticipant = participants.some(
      (participant) => participant.user_id === userId,
    );

    return NextResponse.json({
      session: {
        ...mappedSession,
        isHost,
        isParticipant,
      },
    });
  } catch (error) {
    console.error("Session fetch error:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}
