import { type NextRequest, NextResponse } from "next/server";

import { getUserIdFromRequest } from "@/lib/auth";
import { ensureEventThreadForSession } from "@/lib/server/event-threads";
import {
  requireSessionHost,
  SessionAccessError,
} from "@/lib/server/session-access";
import { supabase } from "@/lib/supabase";

type SupabaseEventRow = {
  id: string;
  type: string;
  agent_id: string | null;
  user_id: string | null;
  progress: number | string | null;
  payload: Record<string, unknown> | null;
  order_index: number | string | null;
  created_at: string;
  updated_at: string;
};

export async function GET(
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

    const { data: rawEvents, error: eventsError } = await supabase
      .from("events")
      .select(
        "id, type, agent_id, user_id, progress, payload, order_index, created_at, updated_at",
      )
      .eq("thread_id", thread.id)
      .order("order_index", { ascending: true });

    if (eventsError) {
      console.error("Failed to fetch events:", eventsError);
      return NextResponse.json(
        { error: "Failed to load event thread" },
        { status: 500 },
      );
    }

    const statementIds = new Set<string>();
    (rawEvents ?? []).forEach((event) => {
      const payload = (event.payload ?? {}) as {
        statementIds?: string[];
      };
      if (Array.isArray(payload.statementIds)) {
        for (const id of payload.statementIds) {
          statementIds.add(id);
        }
      }
    });

    let statementsById: Record<
      string,
      { id: string; text: string; orderIndex: number }
    > = {};

    if (statementIds.size > 0) {
      const { data: statements, error: statementsError } = await supabase
        .from("statements")
        .select("id, text, order_index")
        .in("id", Array.from(statementIds));

      if (statementsError) {
        console.error(
          "Failed to fetch statements for events:",
          statementsError,
        );
        return NextResponse.json(
          { error: "Failed to load event thread" },
          { status: 500 },
        );
      }

      statementsById = Object.fromEntries(
        (statements ?? []).map((statement) => [
          statement.id,
          {
            id: statement.id,
            text: statement.text,
            orderIndex: statement.order_index ?? 0,
          },
        ]),
      );
    }

    const events = (rawEvents ?? []).map((event: SupabaseEventRow) => {
      const payload = (event.payload ?? {}) as Record<string, unknown>;
      const statementIdsFromPayload = (
        payload as {
          statementIds?: string[];
        }
      ).statementIds;
      const statementList = Array.isArray(statementIdsFromPayload)
        ? statementIdsFromPayload
            .map((statementId) => statementsById[statementId])
            .filter(Boolean)
        : [];

      return {
        id: event.id,
        type: event.type,
        agentId: event.agent_id,
        userId: event.user_id,
        progress: Number(event.progress ?? 0),
        payload,
        orderIndex: Number(event.order_index ?? 0),
        createdAt: event.created_at,
        updatedAt: event.updated_at,
        statements: statementList,
      };
    });

    const { data: agents, error: agentsError } = await supabase
      .from("agent_instances")
      .select("id, agent_type, state, state_payload, created_at, updated_at")
      .eq("thread_id", thread.id)
      .order("created_at", { ascending: true });

    if (agentsError) {
      console.error("Failed to fetch agents:", agentsError);
      return NextResponse.json(
        { error: "Failed to load event thread" },
        { status: 500 },
      );
    }

    return NextResponse.json({
      session: {
        id: session.id,
        title: session.title,
        context: session.context,
        goal: session.goal,
        isPublic: session.is_public,
      },
      thread: {
        id: thread.id,
        shouldProceed: thread.should_proceed,
        createdAt: thread.created_at,
        updatedAt: thread.updated_at,
      },
      events,
      agents: (agents ?? []).map((agent) => ({
        id: agent.id,
        agentType: agent.agent_type,
        state: agent.state,
        statePayload: agent.state_payload ?? {},
        createdAt: agent.created_at,
        updatedAt: agent.updated_at,
      })),
    });
  } catch (error) {
    if (error instanceof SessionAccessError) {
      return NextResponse.json(
        { error: error.message },
        { status: error.status },
      );
    }

    console.error("Failed to load event thread:", error);
    return NextResponse.json(
      { error: "Failed to load event thread" },
      { status: 500 },
    );
  }
}
