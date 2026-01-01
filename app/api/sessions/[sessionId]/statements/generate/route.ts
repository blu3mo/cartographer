export const dynamic = "force-dynamic";

import { type NextRequest, NextResponse } from "next/server";

import { getUserIdFromRequest } from "@/lib/auth";
import { ensureEventThreadForSession } from "@/lib/server/event-threads";
import {
  requireSessionHost,
  SessionAccessError,
} from "@/lib/server/session-access";
import { supabase } from "@/lib/supabase";
import { generateSurveyStatements } from "../../../../../../agents/llm";

export async function POST(
  request: NextRequest,
  { params }: { params: Promise<{ sessionId: string }> },
) {
  const userId = getUserIdFromRequest(request);

  if (!userId) {
    return NextResponse.json(
      { error: "Unauthorized: Missing user ID" },
      { status: 401 },
    );
  }

  try {
    const { sessionId } = await params;
    const session = await requireSessionHost(sessionId, userId);
    const thread = await ensureEventThreadForSession(session);

    // Fetch context for LLM
    const { data: events, error: eventsError } = await supabase
      .from("events")
      .select(
        "id, type, agent_id, user_id, progress, payload, order_index, created_at",
      )
      .eq("thread_id", thread.id)
      .order("order_index", { ascending: true });

    if (eventsError) {
      console.error("Failed to fetch events for context:", eventsError);
      return NextResponse.json(
        { error: "Failed to load event thread" },
        { status: 500 },
      );
    }

    const { count: participantCount } = await supabase
      .from("participants")
      .select("user_id", { count: "exact", head: true })
      .eq("session_id", sessionId);

    // Generate statements (15 questions)
    const statementTexts = await generateSurveyStatements({
      sessionTitle: session.title,
      sessionGoal: session.goal,
      initialContext: session.context,
      eventThreadContext: JSON.stringify(events), // Simplified context
      participantCount: participantCount ?? 0,
    });

    // Get last order index
    const { data: lastStatement } = await supabase
      .from("statements")
      .select("order_index")
      .eq("session_id", sessionId)
      .order("order_index", { ascending: false })
      .limit(1)
      .maybeSingle();

    const baseIndex = lastStatement?.order_index ?? 0;

    // Prepare bulk insert
    const statementsPayload = statementTexts.map((text, index) => ({
      session_id: sessionId,
      text,
      order_index: baseIndex + index + 1,
    }));

    if (statementsPayload.length === 0) {
      return NextResponse.json(
        { error: "No statements generated" },
        { status: 500 },
      );
    }

    // Insert statements
    const { data: insertedStatements, error: insertError } = await supabase
      .from("statements")
      .insert(statementsPayload)
      .select("id, text, order_index");

    if (insertError || !insertedStatements) {
      console.error("Failed to insert generated statements:", insertError);
      return NextResponse.json(
        { error: "Failed to save generated statements" },
        { status: 500 },
      );
    }

    return NextResponse.json({
      statements: insertedStatements.map((s) => ({
        id: s.id,
        text: s.text,
        orderIndex: s.order_index,
      })),
    });
  } catch (error) {
    if (error instanceof SessionAccessError) {
      return NextResponse.json(
        { error: error.message },
        { status: error.status },
      );
    }

    console.error("Failed to generate statements:", error);
    return NextResponse.json(
      { error: "Failed to generate statements" },
      { status: 500 },
    );
  }
}
