import { type NextRequest, NextResponse } from "next/server";
import { getUserIdFromRequest } from "@/lib/auth";
import { ensureEventThreadForSession } from "@/lib/server/event-threads";
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

    const { data: createdSession, error: createSessionError } = await supabase
      .from("sessions")
      .insert({
        title: trimmedTitle,
        context: normalizedContext,
        goal: trimmedGoal,
        is_public: false,
        host_user_id: userId,
      })
      .select(
        "id, title, context, goal, is_public, host_user_id, admin_access_token, created_at, updated_at",
      )
      .single();

    if (createSessionError || !createdSession) {
      console.error("Failed to create session:", createSessionError);
      return NextResponse.json(
        { error: "Failed to create session" },
        { status: 500 },
      );
    }

    let threadId: string;
    try {
      const thread = await ensureEventThreadForSession({
        id: createdSession.id,
        context: normalizedContext,
        goal: trimmedGoal,
        host_user_id: userId,
        title: trimmedTitle,
      });
      threadId = thread.id;
    } catch (threadError) {
      console.error("Failed to provision event thread:", threadError);
      return NextResponse.json(
        { error: "Failed to finalize session bootstrap" },
        { status: 500 },
      );
    }

    // Generate initial 15 questions
    try {
      // Fetch context for LLM (initial user message)
      const { data: events, error: eventsError } = await supabase
        .from("events")
        .select(
          "id, type, agent_id, user_id, progress, payload, order_index, created_at",
        )
        .eq("thread_id", threadId)
        .order("order_index", { ascending: true });

      if (eventsError) {
        console.error("Failed to fetch events for context:", eventsError);
        // Continue without generating questions, or log warning
      } else {
        const statementTexts = await generateSurveyStatements({
          sessionTitle: trimmedTitle,
          sessionGoal: trimmedGoal,
          initialContext: normalizedContext,
          eventThreadContext: JSON.stringify(events),
          participantCount: 0, // Initial creation, no participants yet
        });

        if (statementTexts.length > 0) {
          const statementsPayload = statementTexts.map(
            (text: string, index: number) => ({
              session_id: createdSession.id,
              text,
              order_index: index + 1,
            }),
          );

          const { error: insertError } = await supabase
            .from("statements")
            .insert(statementsPayload);

          if (insertError) {
            console.error(
              "Failed to insert generated statements:",
              insertError,
            );
          }
        }
      }
    } catch (genError) {
      console.error("Failed to generate initial statements:", genError);
      // We don't fail the request here, just log the error.
      // The user will see the session created but without questions.
    }

    return NextResponse.json({
      session: {
        ...mapSession(createdSession),
        adminAccessToken: createdSession.admin_access_token,
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
