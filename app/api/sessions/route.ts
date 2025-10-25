import { type NextRequest, NextResponse } from "next/server";

import { getUserIdFromRequest } from "@/lib/auth";
import { generateInitialStatements } from "@/lib/llm";
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
  statements?: { id: string }[] | null;
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

export async function GET(request: NextRequest) {
  try {
    const userId = getUserIdFromRequest(request);
    if (!userId) {
      return NextResponse.json(
        { error: "Unauthorized: Missing user ID" },
        { status: 401 },
      );
    }

    const { data: participantSessions, error: participantSessionsError } =
      await supabase
        .from("participants")
        .select("session_id")
        .eq("user_id", userId);

    if (participantSessionsError) {
      console.error(
        "Failed to load participant session IDs:",
        participantSessionsError,
      );
      return NextResponse.json(
        { error: "Failed to fetch sessions" },
        { status: 500 },
      );
    }

    const participantSessionIds = (participantSessions ?? []).map(
      (participant) => participant.session_id,
    );

    const participantFilter =
      participantSessionIds.length > 0
        ? `id.in.(${participantSessionIds.map((id) => `"${id}"`).join(",")})`
        : null;

    const orFilters = [
      `host_user_id.eq.${userId}`,
      "is_public.eq.true",
      ...(participantFilter ? [participantFilter] : []),
    ].join(",");

    const { data: sessionsData, error: sessionsError } = await supabase
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
          participants ( user_id ),
          statements ( id )
        `,
      )
      .or(orFilters)
      .order("created_at", { ascending: false });

    if (sessionsError) {
      console.error("Failed to fetch sessions:", sessionsError);
      return NextResponse.json(
        { error: "Failed to fetch sessions" },
        { status: 500 },
      );
    }

    const sessionsWithRoles = (sessionsData ?? []).map((session) => {
      const mappedSession = mapSession(session);
      const participants = session.participants ?? [];
      const statements = session.statements ?? [];
      const isHost = mappedSession.hostUserId === userId;
      const isParticipant = participants.some(
        (participant) => participant.user_id === userId,
      );

      return {
        ...mappedSession,
        isHost,
        isParticipant,
        _count: {
          participants: participants.length,
          statements: statements.length,
        },
      };
    });

    return NextResponse.json({ sessions: sessionsWithRoles });
  } catch (error) {
    console.error("Sessions fetch error:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}

export async function POST(request: NextRequest) {
  try {
    const userId = getUserIdFromRequest(request);
    if (!userId) {
      return NextResponse.json(
        { error: "Unauthorized: Missing user ID" },
        { status: 401 },
      );
    }

    const body = await request.json();
    const { title, context, isPublic } = body;

    if (!title || !context) {
      return NextResponse.json(
        { error: "Missing required fields: title and context" },
        { status: 400 },
      );
    }

    const { data: createdSessions, error: createSessionError } = await supabase
      .from("sessions")
      .insert({
        title,
        context,
        is_public: typeof isPublic === "boolean" ? isPublic : true,
        host_user_id: userId,
      })
      .select(
        "id, title, context, is_public, host_user_id, created_at, updated_at",
      )
      .single();

    if (createSessionError || !createdSessions) {
      console.error("Failed to create session:", createSessionError);
      return NextResponse.json(
        { error: "Failed to create session" },
        { status: 500 },
      );
    }

    // Generate initial statements using LLM (with fallback to defaults)
    const statementTexts = await generateInitialStatements(title, context);

    // Save statements to database
    const statementsPayload = statementTexts.map((text, index) => ({
      session_id: createdSessions.id,
      text,
      order_index: index,
    }));

    const { error: insertStatementsError } = await supabase
      .from("statements")
      .insert(statementsPayload);

    if (insertStatementsError) {
      console.error(
        "Failed to insert initial statements:",
        insertStatementsError,
      );
      return NextResponse.json(
        { error: "Failed to create session" },
        { status: 500 },
      );
    }

    return NextResponse.json({ session: mapSession(createdSessions) });
  } catch (error) {
    console.error("Session creation error:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}
