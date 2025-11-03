import { type NextRequest, NextResponse } from "next/server";
import { getUserIdFromRequest } from "@/lib/auth";
import { supabase } from "@/lib/supabase";

type StatementRow = {
  id: string;
  text: string;
  order_index: number;
};

type ResponseRow = {
  id: string;
  participant_user_id: string;
  session_id: string;
  statement_id: string;
  value: number | null;
  text_answer: string | null;
  created_at: string;
  statement?: StatementRow | StatementRow[] | null;
};

function mapResponse(row: ResponseRow) {
  const statement = Array.isArray(row.statement)
    ? (row.statement[0] ?? null)
    : (row.statement ?? null);

  return {
    id: row.id,
    statementId: row.statement_id,
    statementText: statement?.text ?? "",
    orderIndex: statement?.order_index ?? 0,
    value: row.value,
    textAnswer: row.text_answer,
    createdAt: row.created_at,
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

    const { data: participant, error: participantError } = await supabase
      .from("participants")
      .select("user_id")
      .eq("user_id", userId)
      .eq("session_id", sessionId)
      .maybeSingle();

    if (participantError) {
      console.error("Failed to verify participant:", participantError);
      return NextResponse.json(
        { error: "Failed to fetch responses" },
        { status: 500 },
      );
    }

    if (!participant) {
      return NextResponse.json(
        { error: "Participant not found in this session" },
        { status: 404 },
      );
    }

    const { data: responses, error: responsesError } = await supabase
      .from("responses")
      .select(
        `
          id,
          participant_user_id,
          session_id,
          statement_id,
          value,
          text_answer,
          created_at,
          statement:statements (
            id,
            text,
            order_index
          )
        `,
      )
      .eq("participant_user_id", userId)
      .eq("session_id", sessionId);

    if (responsesError) {
      console.error("Failed to fetch participant responses:", responsesError);
      return NextResponse.json(
        { error: "Failed to fetch responses" },
        { status: 500 },
      );
    }

    return NextResponse.json({
      responses: (responses ?? [])
        .map((response) => mapResponse(response as ResponseRow))
        .sort((a, b) => {
          if (a.createdAt === b.createdAt) {
            return a.orderIndex - b.orderIndex;
          }
          return (
            new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime()
          );
        }),
    });
  } catch (error) {
    console.error("Failed to fetch participant responses:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}

export async function POST(
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

    const body = await request.json();
    const { statementId, value, textAnswer } = body;

    if (!statementId) {
      return NextResponse.json(
        { error: "Missing required field: statementId" },
        { status: 400 },
      );
    }

    const hasValue = value !== undefined && value !== null;
    const hasTextAnswer =
      textAnswer !== undefined && textAnswer !== null && textAnswer !== "";

    if (!hasValue && !hasTextAnswer) {
      return NextResponse.json(
        { error: "Either value or textAnswer must be provided" },
        { status: 400 },
      );
    }

    if (hasValue && hasTextAnswer) {
      return NextResponse.json(
        { error: "Cannot provide both value and textAnswer" },
        { status: 400 },
      );
    }

    // Validate value is in range if provided
    if (hasValue && ![-2, -1, 0, 1, 2].includes(value)) {
      return NextResponse.json(
        { error: "Invalid value: must be -2, -1, 0, 1, or 2" },
        { status: 400 },
      );
    }

    // Validate textAnswer length if provided
    if (hasTextAnswer && textAnswer.length > 5000) {
      return NextResponse.json(
        { error: "Text answer must be 5000 characters or less" },
        { status: 400 },
      );
    }

    const { data: statement, error: statementError } = await supabase
      .from("statements")
      .select("id")
      .eq("id", statementId)
      .eq("session_id", sessionId)
      .single();

    if (statementError || !statement) {
      if (statementError?.code === "PGRST116") {
        return NextResponse.json(
          { error: "Statement not found in this session" },
          { status: 404 },
        );
      }
      console.error("Failed to verify statement:", statementError);
      return NextResponse.json(
        { error: "Statement not found in this session" },
        { status: 404 },
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
        { error: "Failed to submit response" },
        { status: 500 },
      );
    }

    if (!participant) {
      return NextResponse.json(
        { error: "Participant not found in this session" },
        { status: 404 },
      );
    }

    const responseData: {
      participant_user_id: string;
      session_id: string;
      statement_id: string;
      value?: number;
      text_answer?: string;
    } = {
      participant_user_id: userId,
      session_id: sessionId,
      statement_id: statementId,
    };

    if (hasValue) {
      responseData.value = value;
    } else if (hasTextAnswer) {
      responseData.text_answer = textAnswer;
    }

    const { data: response, error: upsertError } = await supabase
      .from("responses")
      .upsert(responseData, {
        onConflict: "participant_user_id,session_id,statement_id",
      })
      .select(
        `
          id,
          participant_user_id,
          session_id,
          statement_id,
          value,
          text_answer,
          created_at,
          statement:statements (
            id,
            text,
            order_index
          )
        `,
      )
      .single();

    if (upsertError || !response) {
      console.error("Failed to submit response:", upsertError);
      return NextResponse.json(
        { error: "Failed to submit response" },
        { status: 500 },
      );
    }

    return NextResponse.json({
      success: true,
      response: mapResponse(response as ResponseRow),
    });
  } catch (error) {
    console.error("Response submission error:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}
