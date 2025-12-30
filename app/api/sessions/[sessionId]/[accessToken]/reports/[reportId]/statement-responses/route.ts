export const dynamic = "force-dynamic";

import { type NextRequest, NextResponse } from "next/server";

import { getUserIdFromRequest } from "@/lib/auth";
import {
  requireSessionAdminToken,
  SessionAccessError,
} from "@/lib/server/session-access";
import { supabase } from "@/lib/supabase";

type ResponseValue = -2 | -1 | 0 | 1 | 2;

function getResponseLabel(value: ResponseValue): string {
  switch (value) {
    case 2:
      return "強く同意";
    case 1:
      return "同意";
    case 0:
      return "わからない";
    case -1:
      return "反対";
    case -2:
      return "強く反対";
    default:
      return "Unknown";
  }
}

function handleAccessError(error: unknown) {
  if (error instanceof SessionAccessError) {
    return NextResponse.json(
      { error: error.message },
      { status: error.status },
    );
  }
  console.error(error);
  return NextResponse.json({ error: "Internal Server Error" }, { status: 500 });
}

export async function GET(
  request: NextRequest,
  {
    params,
  }: {
    params: Promise<{
      sessionId: string;
      accessToken: string;
      reportId: string;
    }>;
  },
) {
  const { sessionId, accessToken } = await params;
  const userId = getUserIdFromRequest(request);

  if (!userId) {
    return NextResponse.json({ error: "Unauthorized" }, { status: 401 });
  }

  const url = new URL(request.url);
  const statementNumberParam = url.searchParams.get("statementNumber");

  if (!statementNumberParam) {
    return NextResponse.json(
      { error: "Missing required query parameter: statementNumber" },
      { status: 400 },
    );
  }

  const statementNumber = parseInt(statementNumberParam, 10);
  if (isNaN(statementNumber) || statementNumber < 1) {
    return NextResponse.json(
      { error: "Invalid statementNumber" },
      { status: 400 },
    );
  }

  try {
    await requireSessionAdminToken(sessionId, accessToken);

    // order_index is 0-based, statementNumber is 1-based
    const orderIndex = statementNumber - 1;

    const { data: statement, error: statementError } = await supabase
      .from("statements")
      .select("id, text, order_index")
      .eq("session_id", sessionId)
      .eq("order_index", orderIndex)
      .maybeSingle();

    if (statementError) {
      console.error("Failed to fetch statement:", statementError);
      return NextResponse.json(
        { error: "Failed to fetch statement" },
        { status: 500 },
      );
    }

    if (!statement) {
      return NextResponse.json(
        { error: "Statement not found" },
        { status: 404 },
      );
    }

    // Fetch all responses for this statement
    const { data: responses, error: responsesError } = await supabase
      .from("responses")
      .select("participant_user_id, response_type, value, text_response")
      .eq("session_id", sessionId)
      .eq("statement_id", statement.id);

    if (responsesError) {
      console.error("Failed to fetch responses:", responsesError);
      return NextResponse.json(
        { error: "Failed to fetch responses" },
        { status: 500 },
      );
    }

    // Fetch participants for this session
    const { data: participants, error: participantsError } = await supabase
      .from("participants")
      .select("user_id, name")
      .eq("session_id", sessionId);

    if (participantsError) {
      console.error("Failed to fetch participants:", participantsError);
      return NextResponse.json(
        { error: "Failed to fetch participants" },
        { status: 500 },
      );
    }

    // Create a map of user_id -> name
    const participantMap = new Map(
      (participants ?? []).map((p) => [p.user_id, p.name]),
    );

    const formattedResponses = (responses ?? []).map((response) => {
      const value = response.value as ResponseValue | null;

      return {
        participantName:
          participantMap.get(response.participant_user_id) ?? "Unknown",
        responseType: response.response_type as "scale" | "free_text",
        value,
        valueLabel: value !== null ? getResponseLabel(value) : null,
        textResponse: response.text_response,
      };
    });

    return NextResponse.json({
      statement: {
        id: statement.id,
        number: statementNumber,
        text: statement.text,
      },
      responses: formattedResponses,
    });
  } catch (error) {
    return handleAccessError(error);
  }
}
