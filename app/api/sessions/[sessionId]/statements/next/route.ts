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

    // Get excludeStatementId(s) from query params if provided
    const { searchParams } = new URL(request.url);
    const excludeStatementIds = new Set(
      searchParams.getAll("excludeStatementId").filter(Boolean),
    );

    const { data: allStatements, error: statementsError } = await supabase
      .from("statements")
      .select("id, session_id, text, order_index, created_at")
      .eq("session_id", sessionId)
      .order("order_index", { ascending: true });

    if (statementsError) {
      console.error("Failed to load statements:", statementsError);
      return NextResponse.json(
        { error: "Internal server error" },
        { status: 500 },
      );
    }

    // Get all responses by this participant
    const { data: existingResponses, error: responsesError } = await supabase
      .from("responses")
      .select("statement_id")
      .eq("participant_user_id", userId)
      .eq("session_id", sessionId);

    if (responsesError) {
      console.error("Failed to load responses:", responsesError);
      return NextResponse.json(
        { error: "Internal server error" },
        { status: 500 },
      );
    }

    const answeredStatementIds = new Set(
      (existingResponses ?? []).map((r) => r.statement_id),
    );

    // Filter unanswered statements
    let unansweredStatements = (allStatements ?? []).filter(
      (s) => !answeredStatementIds.has(s.id),
    );

    // Exclude the currently displayed statement(s) if provided
    if (excludeStatementIds.size > 0) {
      unansweredStatements = unansweredStatements.filter(
        (s) => !excludeStatementIds.has(s.id),
      );
    }

    if (unansweredStatements.length === 0) {
      return NextResponse.json({ statement: null });
    }

    // Return a random unanswered statement
    const randomIndex = Math.floor(Math.random() * unansweredStatements.length);
    const statement = unansweredStatements[randomIndex] as StatementRow;

    return NextResponse.json({ statement: mapStatement(statement) });
  } catch (error) {
    console.error("Get next statement error:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}
