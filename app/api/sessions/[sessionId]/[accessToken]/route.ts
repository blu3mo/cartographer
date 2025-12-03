import { type NextRequest, NextResponse } from "next/server";

import { getUserIdFromRequest } from "@/lib/auth";
import { supabase } from "@/lib/supabase";

type ResponseValue = -2 | -1 | 0 | 1 | 2;

interface ResponseStats {
  strongYes: number; // percentage
  yes: number;
  dontKnow: number;
  no: number;
  strongNo: number;
  totalCount: number;
  freeTextCount: number;
  freeTextSamples: Array<{ participantUserId: string | null; text: string }>;
}

interface StatementWithStats {
  id: string;
  sessionId: string;
  text: string;
  orderIndex: number;
  responses: ResponseStats;
  agreementScore: number;
}

interface ParticipantProgress {
  userId: string;
  name: string;
  answeredCount: number;
  completionRate: number;
  totalStatements: number;
  updatedAt: string;
}

export async function GET(
  request: NextRequest,
  { params }: { params: Promise<{ sessionId: string; accessToken: string }> },
) {
  try {
    const { sessionId, accessToken } = await params;
    const userId = getUserIdFromRequest(request);

    if (!userId) {
      return NextResponse.json(
        { error: "Unauthorized: User ID not found" },
        { status: 401 },
      );
    }

    const { data: session, error: sessionError } = await supabase
      .from("sessions")
      .select(
        "id, title, context, goal, is_public, created_at, host_user_id, admin_access_token",
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
      console.error("Failed to load session for admin view:", sessionError);
      return NextResponse.json(
        { error: "Internal server error" },
        { status: 500 },
      );
    }

    if (session.admin_access_token !== accessToken) {
      return NextResponse.json(
        { error: "Forbidden: Invalid access token" },
        { status: 403 },
      );
    }

    const isHost = session.host_user_id === userId;

    const { data: statements, error: statementsError } = await supabase
      .from("statements")
      .select("id, session_id, text, order_index")
      .eq("session_id", sessionId)
      .order("order_index", { ascending: true });

    if (statementsError) {
      console.error(
        "Failed to fetch statements for admin view:",
        statementsError,
      );
      return NextResponse.json(
        { error: "Internal server error" },
        { status: 500 },
      );
    }

    const { data: responseRows, error: responsesError } = await supabase
      .from("responses")
      .select(
        "statement_id, value, participant_user_id, response_type, text_response",
      )
      .eq("session_id", sessionId);

    if (responsesError) {
      console.error(
        "Failed to fetch responses for admin view:",
        responsesError,
      );
      return NextResponse.json(
        { error: "Internal server error" },
        { status: 500 },
      );
    }

    const { data: participantRows, error: participantsError } = await supabase
      .from("participants")
      .select("user_id, name, updated_at")
      .eq("session_id", sessionId)
      .order("created_at", { ascending: true });

    if (participantsError) {
      console.error(
        "Failed to fetch participants for admin view:",
        participantsError,
      );
      return NextResponse.json(
        { error: "Internal server error" },
        { status: 500 },
      );
    }

    const responseMap = new Map<
      string,
      {
        value: number | null;
        responseType: string;
        text: string | null;
        participantId: string | null;
      }[]
    >();
    const participantResponseCount = new Map<string, number>();

    (responseRows ?? []).forEach((response) => {
      const list = responseMap.get(response.statement_id) ?? [];
      list.push({
        value: response.value,
        responseType: response.response_type,
        text: response.text_response,
        participantId: response.participant_user_id ?? null,
      });
      responseMap.set(response.statement_id, list);

      if (response.participant_user_id) {
        const current = participantResponseCount.get(
          response.participant_user_id,
        );
        participantResponseCount.set(
          response.participant_user_id,
          (current ?? 0) + 1,
        );
      }
    });

    const totalStatements = statements?.length ?? 0;

    const participants: ParticipantProgress[] = (participantRows ?? []).map(
      (participant) => {
        const answeredCount =
          participantResponseCount.get(participant.user_id) ?? 0;
        const completionRate =
          totalStatements > 0
            ? Math.round((answeredCount / totalStatements) * 1000) / 10
            : 0;

        return {
          userId: participant.user_id,
          name: participant.name,
          answeredCount,
          completionRate,
          totalStatements,
          updatedAt: participant.updated_at,
        };
      },
    );

    const statementsWithStats: StatementWithStats[] = (statements ?? []).map(
      (statement) => {
        const statementResponses = responseMap.get(statement.id) ?? [];
        const scaleResponses = statementResponses.filter(
          (response) => response.responseType === "scale",
        );
        const freeTextResponses = statementResponses.filter(
          (response) => response.responseType === "free_text",
        );
        const totalCount = scaleResponses.length;

        let strongYesCount = 0;
        let yesCount = 0;
        let dontKnowCount = 0;
        let noCount = 0;
        let strongNoCount = 0;

        scaleResponses.forEach((response) => {
          const value = response.value as ResponseValue;
          switch (value) {
            case 2:
              strongYesCount++;
              break;
            case 1:
              yesCount++;
              break;
            case 0:
              dontKnowCount++;
              break;
            case -1:
              noCount++;
              break;
            case -2:
              strongNoCount++;
              break;
          }
        });

        const strongYesPercent =
          totalCount > 0 ? (strongYesCount / totalCount) * 100 : 0;
        const yesPercent = totalCount > 0 ? (yesCount / totalCount) * 100 : 0;
        const dontKnowPercent =
          totalCount > 0 ? (dontKnowCount / totalCount) * 100 : 0;
        const noPercent = totalCount > 0 ? (noCount / totalCount) * 100 : 0;
        const strongNoPercent =
          totalCount > 0 ? (strongNoCount / totalCount) * 100 : 0;

        const positiveCount = strongYesCount + yesCount;
        const negativeCount = strongNoCount + noCount;
        const agreementScore = Math.abs(positiveCount - negativeCount);

        return {
          id: statement.id,
          sessionId: statement.session_id,
          text: statement.text,
          orderIndex: statement.order_index,
          responses: {
            strongYes: Math.round(strongYesPercent * 100) / 100,
            yes: Math.round(yesPercent * 100) / 100,
            dontKnow: Math.round(dontKnowPercent * 100) / 100,
            no: Math.round(noPercent * 100) / 100,
            strongNo: Math.round(strongNoPercent * 100) / 100,
            totalCount,
            freeTextCount: freeTextResponses.length,
            freeTextSamples: freeTextResponses.map((response) => ({
              participantUserId: response.participantId,
              text: response.text ?? "",
            })),
          },
          agreementScore,
        };
      },
    );

    return NextResponse.json({
      data: {
        id: session.id,
        title: session.title,
        context: session.context,
        goal: session.goal,
        isPublic: session.is_public,
        createdAt: session.created_at,
        statements: statementsWithStats,
        participants,
        totalStatements,
        totalParticipants: participants.length,
        canEdit: isHost,
      },
    });
  } catch (error) {
    console.error("Error fetching admin data:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}

export async function PATCH(
  request: NextRequest,
  { params }: { params: Promise<{ sessionId: string; accessToken: string }> },
) {
  try {
    const { sessionId, accessToken } = await params;
    const userId = getUserIdFromRequest(request);

    if (!userId) {
      return NextResponse.json(
        { error: "Unauthorized: User ID not found" },
        { status: 401 },
      );
    }

    const { data: session, error: sessionError } = await supabase
      .from("sessions")
      .select("id, host_user_id, admin_access_token")
      .eq("id", sessionId)
      .single();

    if (sessionError || !session) {
      if (sessionError?.code === "PGRST116") {
        return NextResponse.json(
          { error: "Session not found" },
          { status: 404 },
        );
      }
      console.error("Failed to load session for update:", sessionError);
      return NextResponse.json(
        { error: "Internal server error" },
        { status: 500 },
      );
    }

    if (session.admin_access_token !== accessToken) {
      return NextResponse.json(
        { error: "Forbidden: Invalid access token" },
        { status: 403 },
      );
    }

    if (session.host_user_id !== userId) {
      return NextResponse.json(
        { error: "Forbidden: Only the host can edit this session" },
        { status: 403 },
      );
    }

    const body = await request.json();
    const { title, context, goal } = body as {
      title?: unknown;
      context?: unknown;
      goal?: unknown;
    };

    if (typeof title !== "string" || title.trim().length === 0) {
      return NextResponse.json({ error: "Invalid title" }, { status: 400 });
    }

    if (typeof context !== "string") {
      return NextResponse.json({ error: "Invalid context" }, { status: 400 });
    }

    if (typeof goal !== "string" || goal.trim().length === 0) {
      return NextResponse.json({ error: "Invalid goal" }, { status: 400 });
    }

    const { data: updatedSession, error: updateError } = await supabase
      .from("sessions")
      .update({
        title: title.trim(),
        context: context.trim(),
        goal: goal.trim(),
        is_public: false,
      })
      .eq("id", sessionId)
      .select("id, title, context, goal, is_public, created_at")
      .single();

    if (updateError || !updatedSession) {
      console.error("Failed to update session:", updateError);
      return NextResponse.json(
        { error: "Failed to update session" },
        { status: 500 },
      );
    }

    return NextResponse.json({
      data: {
        id: updatedSession.id,
        title: updatedSession.title,
        context: updatedSession.context,
        goal: updatedSession.goal,
        isPublic: updatedSession.is_public,
        createdAt: updatedSession.created_at,
      },
    });
  } catch (error) {
    console.error("Error updating session:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}

export async function DELETE(
  request: NextRequest,
  { params }: { params: Promise<{ sessionId: string; accessToken: string }> },
) {
  try {
    const { sessionId, accessToken } = await params;
    const userId = getUserIdFromRequest(request);

    if (!userId) {
      return NextResponse.json(
        { error: "Unauthorized: User ID not found" },
        { status: 401 },
      );
    }

    const { data: session, error: sessionError } = await supabase
      .from("sessions")
      .select("id, host_user_id, admin_access_token")
      .eq("id", sessionId)
      .single();

    if (sessionError || !session) {
      if (sessionError?.code === "PGRST116") {
        return NextResponse.json(
          { error: "Session not found" },
          { status: 404 },
        );
      }
      console.error("Failed to load session for delete:", sessionError);
      return NextResponse.json(
        { error: "Internal server error" },
        { status: 500 },
      );
    }

    if (session.admin_access_token !== accessToken) {
      return NextResponse.json(
        { error: "Forbidden: Invalid access token" },
        { status: 403 },
      );
    }

    if (session.host_user_id !== userId) {
      return NextResponse.json(
        { error: "Forbidden: Only the host can delete this session" },
        { status: 403 },
      );
    }

    const { error: deleteError } = await supabase
      .from("sessions")
      .delete()
      .eq("id", sessionId);

    if (deleteError) {
      console.error("Failed to delete session:", deleteError);
      return NextResponse.json(
        { error: "Failed to delete session" },
        { status: 500 },
      );
    }

    return NextResponse.json({
      success: true,
      message: "Session deleted successfully",
    });
  } catch (error) {
    console.error("Error deleting session:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}
