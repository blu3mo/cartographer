import { type NextRequest, NextResponse } from "next/server";
import { getUserIdFromRequest } from "@/lib/auth";
import { createSession } from "@/lib/api/client";
import { CreateSessionRequest } from "@/lib/api/types";
import { supabase } from "@/lib/supabase";
import { generateSurveyStatements } from "../../../agents/llm";

type SessionRow = {
  id: string;
  title: string;
  context: string;
  goal: string;
  is_public: boolean;
  host_user_id: string;
  admin_access_token: string;
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
    goal: row.goal,
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
          goal,
          is_public,
          host_user_id,
          admin_access_token,
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
        adminAccessToken: isHost ? session.admin_access_token : undefined,
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
    const { title, context, goal } = body as {
      title?: unknown;
      context?: unknown;
      goal?: unknown;
    };

    if (typeof title !== "string" || title.trim().length === 0) {
      return NextResponse.json(
        { error: "Missing required field: title" },
        { status: 400 },
      );
    }

    if (typeof goal !== "string" || goal.trim().length === 0) {
      return NextResponse.json(
        { error: "Missing required field: goal" },
        { status: 400 },
      );
    }

    if (typeof context !== "string") {
      return NextResponse.json(
        { error: "Invalid value for context" },
        { status: 400 },
      );
    }

    const trimmedTitle = title.trim();
    const trimmedGoal = goal.trim();
    const normalizedContext = context.trim();

    // Generate initial 15 questions
    let statementTexts: string[] = [];
    try {
      const { initialQuestions } = body as { initialQuestions?: string[] };

      if (
        Array.isArray(initialQuestions) &&
        initialQuestions.every((q) => typeof q === "string") &&
        initialQuestions.length > 0
      ) {
        statementTexts = initialQuestions;
      } else {
        // Generate with minimal context (since we don't have Supabase thread history anymore)
        statementTexts = await generateSurveyStatements({
          sessionTitle: trimmedTitle,
          sessionGoal: trimmedGoal,
          initialContext: normalizedContext,
          eventThreadContext: "[]", // Empty history
          participantCount: 0,
        });
      }
    } catch (genError) {
      console.error("Failed to generate initial statements:", genError);
      // Continue without questions if generation fails
    }

    // Call Haskell Backend
    const createReq: CreateSessionRequest = {
      title: trimmedTitle,
      purpose: trimmedGoal,
      background: normalizedContext,
      hostUserId: userId,
      initialQuestions: statementTexts.length > 0 ? statementTexts : undefined,
    };

    const backendRes = await createSession(createReq);

    return NextResponse.json({
      session: {
        id: backendRes.sessionId,
        adminAccessToken: backendRes.adminAccessToken,
      },
    });
  } catch (error) {
    console.error("Session creation error:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}
