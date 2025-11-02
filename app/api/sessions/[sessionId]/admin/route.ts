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
  { params }: { params: Promise<{ sessionId: string }> },
) {
  try {
    const { sessionId } = await params;
    const userId = getUserIdFromRequest(request);

    if (!userId) {
      return NextResponse.json(
        { error: "Unauthorized: User ID not found" },
        { status: 401 },
      );
    }

    // Verify that the user is the host of this session
    const { data: session, error: sessionError } = await supabase
      .from("sessions")
      .select("id, title, context, goal, is_public, created_at, host_user_id")
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

    if (session.host_user_id !== userId) {
      return NextResponse.json(
        { error: "Forbidden: You are not the host of this session" },
        { status: 403 },
      );
    }

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
      .select("statement_id, value, participant_user_id")
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

    // Calculate statistics for each statement
    const responseMap = new Map<string, { value: number }[]>();
    const participantResponseCount = new Map<string, number>();

    (responseRows ?? []).forEach((response) => {
      const list = responseMap.get(response.statement_id) ?? [];
      list.push({ value: response.value });
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
        const totalCount = statementResponses.length;

        let strongYesCount = 0;
        let yesCount = 0;
        let dontKnowCount = 0;
        let noCount = 0;
        let strongNoCount = 0;

        statementResponses.forEach((response) => {
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

        // Agreement score: absolute value of (yes+strongYes) - (no+strongNo)
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
          },
          agreementScore,
        };
      },
    );

    // Fetch the latest situation analysis report
    const { data: latestReport, error: latestReportError } = await supabase
      .from("situation_analysis_reports")
      .select("id, session_id, content_markdown, created_at")
      .eq("session_id", sessionId)
      .order("created_at", { ascending: false })
      .limit(1)
      .maybeSingle();

    if (latestReportError && latestReportError.code !== "PGRST116") {
      console.error(
        "Failed to fetch latest report for admin view:",
        latestReportError,
      );
      return NextResponse.json(
        { error: "Internal server error" },
        { status: 500 },
      );
    }

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
        latestSituationAnalysisReport: latestReport
          ? {
              id: latestReport.id,
              sessionId: latestReport.session_id,
              contentMarkdown: latestReport.content_markdown,
              createdAt: latestReport.created_at,
            }
          : undefined,
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
  { params }: { params: Promise<{ sessionId: string }> },
) {
  try {
    const { sessionId } = await params;
    const userId = getUserIdFromRequest(request);

    if (!userId) {
      return NextResponse.json(
        { error: "Unauthorized: User ID not found" },
        { status: 401 },
      );
    }

    const { data: session, error: sessionError } = await supabase
      .from("sessions")
      .select("id, host_user_id")
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

    if (session.host_user_id !== userId) {
      return NextResponse.json(
        { error: "Forbidden: You are not the host of this session" },
        { status: 403 },
      );
    }

    const body = await request.json();
    const { title, context, goal, isPublic } = body as {
      title?: unknown;
      context?: unknown;
      goal?: unknown;
      isPublic?: unknown;
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

    if (typeof isPublic !== "boolean") {
      return NextResponse.json(
        { error: "Invalid visibility" },
        { status: 400 },
      );
    }

    const { data: updatedSession, error: updateError } = await supabase
      .from("sessions")
      .update({
        title: title.trim(),
        context: context.trim(),
        goal: goal.trim(),
        is_public: isPublic,
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
  { params }: { params: Promise<{ sessionId: string }> },
) {
  try {
    const { sessionId } = await params;
    const userId = getUserIdFromRequest(request);

    if (!userId) {
      return NextResponse.json(
        { error: "Unauthorized: User ID not found" },
        { status: 401 },
      );
    }

    const { data: session, error: sessionError } = await supabase
      .from("sessions")
      .select("id, host_user_id")
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

    if (session.host_user_id !== userId) {
      return NextResponse.json(
        { error: "Forbidden: You are not the host of this session" },
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
