import { type NextRequest, NextResponse } from "next/server";

import { getUserIdFromRequest } from "@/lib/auth";
import { supabase } from "@/lib/supabase";

type StatementRow = {
  id: string;
  session_id: string;
  text: string;
  order_index: number;
  created_at: string;
};

const mapStatement = (row: StatementRow) => ({
  id: row.id,
  sessionId: row.session_id,
  text: row.text,
  orderIndex: row.order_index,
  createdAt: row.created_at,
});

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

    const { data: participant, error: participantError } = await supabase
      .from("participants")
      .select("user_id")
      .eq("user_id", userId)
      .eq("session_id", sessionId)
      .maybeSingle();

    if (participantError) {
      console.error("Failed to verify participant:", participantError);
      return NextResponse.json(
        { error: "Internal server error" },
        { status: 500 },
      );
    }

    if (!participant) {
      return NextResponse.json(
        { error: "Unauthorized: Not a participant in this session" },
        { status: 401 },
      );
    }

    const { data: latestSurveyEvent } = await supabase
      .from("events")
      .select("payload")
      .eq("thread_id", sessionId)
      .eq("type", "survey")
      .order("order_index", { ascending: false })
      .limit(1)
      .maybeSingle();

    const openStatementId =
      (latestSurveyEvent?.payload as { openStatementId?: string })
        ?.openStatementId ?? null;

    if (!openStatementId) {
      return NextResponse.json({ statement: null });
    }

    const { data: existingResponse } = await supabase
      .from("responses")
      .select("statement_id")
      .eq("participant_user_id", userId)
      .eq("session_id", sessionId)
      .eq("statement_id", openStatementId)
      .maybeSingle();

    if (existingResponse) {
      return NextResponse.json({ statement: null });
    }

    const { data: openStatement, error: statementError } = await supabase
      .from("statements")
      .select("id, session_id, text, order_index, created_at")
      .eq("id", openStatementId)
      .eq("kind", "OPEN")
      .maybeSingle();

    if (statementError) {
      console.error("Failed to load open statement:", statementError);
      return NextResponse.json(
        { error: "Internal server error" },
        { status: 500 },
      );
    }

    if (!openStatement) {
      return NextResponse.json({ statement: null });
    }

    return NextResponse.json({ statement: mapStatement(openStatement) });
  } catch (error) {
    console.error("Get open question error:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}
