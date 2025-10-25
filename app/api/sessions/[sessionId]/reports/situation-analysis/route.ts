import { type NextRequest, NextResponse } from "next/server";

import { getUserIdFromRequest } from "@/lib/auth";
import { generateSituationAnalysisReport } from "@/lib/llm";
import { supabase } from "@/lib/supabase";

type ResponseValue = -2 | -1 | 0 | 1 | 2;

export async function POST(
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
      .select("id, title, context, host_user_id")
      .eq("id", sessionId)
      .single();

    if (sessionError || !session) {
      if (sessionError?.code === "PGRST116") {
        return NextResponse.json(
          { error: "Session not found" },
          { status: 404 },
        );
      }
      console.error(
        "Failed to load session for situation analysis:",
        sessionError,
      );
      return NextResponse.json(
        { error: "Failed to generate report" },
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
      .select("id, text, order_index")
      .eq("session_id", sessionId)
      .order("order_index", { ascending: true });

    if (statementsError) {
      console.error(
        "Failed to load statements for situation analysis:",
        statementsError,
      );
      return NextResponse.json(
        { error: "Failed to generate report" },
        { status: 500 },
      );
    }

    const { data: responses, error: responsesError } = await supabase
      .from("responses")
      .select("participant_user_id, statement_id, value")
      .eq("session_id", sessionId);

    if (responsesError) {
      console.error(
        "Failed to load responses for situation analysis:",
        responsesError,
      );
      return NextResponse.json(
        { error: "Failed to generate report" },
        { status: 500 },
      );
    }

    const { data: participants, error: participantsError } = await supabase
      .from("participants")
      .select("user_id, name")
      .eq("session_id", sessionId);

    if (participantsError) {
      console.error(
        "Failed to load participants for situation analysis:",
        participantsError,
      );
      return NextResponse.json(
        { error: "Failed to generate report" },
        { status: 500 },
      );
    }

    const participantNameMap = new Map(
      (participants ?? []).map((participant) => [
        participant.user_id,
        participant.name,
      ]),
    );

    const responsesByStatement = new Map<
      string,
      { participantUserId: string; value: number }[]
    >();

    (responses ?? []).forEach((response) => {
      const list = responsesByStatement.get(response.statement_id) ?? [];
      list.push({
        participantUserId: response.participant_user_id,
        value: response.value,
      });
      responsesByStatement.set(response.statement_id, list);
    });

    const statementsWithStats = (statements ?? []).map((statement) => {
      const statementResponses = responsesByStatement.get(statement.id) ?? [];
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

      const agreementScore =
        (strongYesCount * 2 + yesCount - noCount - strongNoCount * 2) /
        (totalCount > 0 ? totalCount : 1);

      return {
        text: statement.text,
        responses: {
          strongYes: Math.round(strongYesPercent),
          yes: Math.round(yesPercent),
          dontKnow: Math.round(dontKnowPercent),
          no: Math.round(noPercent),
          strongNo: Math.round(strongNoPercent),
          totalCount,
        },
        agreementScore,
      };
    });

    const sortedStatements = [...statementsWithStats].sort(
      (a, b) => b.agreementScore - a.agreementScore,
    );

    const uniqueParticipants = new Set(
      (responses ?? []).map((response) => response.participant_user_id),
    );
    const totalParticipants = uniqueParticipants.size;

    let individualResponses: Array<{
      name: string;
      responses: Array<{ statementText: string; value: number }>;
    }> | null = null;

    if (totalParticipants <= 10) {
      const participantResponsesMap = new Map<
        string,
        Array<{ statementText: string; value: number }>
      >();

      (statements ?? []).forEach((statement) => {
        const statementResponses = responsesByStatement.get(statement.id) ?? [];

        statementResponses.forEach((response) => {
          const participantUserId = response.participantUserId;
          const participantName =
            participantNameMap.get(participantUserId) ?? "Unknown";
          const key = `${participantUserId}:${participantName}`;

          let responsesForParticipant = participantResponsesMap.get(key);
          if (!responsesForParticipant) {
            responsesForParticipant = [];
            participantResponsesMap.set(key, responsesForParticipant);
          }

          responsesForParticipant.push({
            statementText: statement.text,
            value: response.value,
          });
        });
      });

      individualResponses = Array.from(participantResponsesMap.entries()).map(
        ([key, responsesForParticipant]) => {
          const [, name] = key.split(":");
          return { name, responses: responsesForParticipant };
        },
      );
    }

    const reportContent = await generateSituationAnalysisReport(
      session.title,
      session.context,
      sortedStatements,
      totalParticipants,
      individualResponses,
    );

    const { data: report, error: reportError } = await supabase
      .from("situation_analysis_reports")
      .insert({
        session_id: sessionId,
        content_markdown: reportContent,
      })
      .select("id, session_id, content_markdown, created_at")
      .single();

    if (reportError || !report) {
      console.error("Failed to save situation analysis report:", reportError);
      return NextResponse.json(
        { error: "Failed to generate report" },
        { status: 500 },
      );
    }

    return NextResponse.json({
      report: {
        id: report.id,
        sessionId: report.session_id,
        contentMarkdown: report.content_markdown,
        createdAt: report.created_at,
      },
    });
  } catch (error) {
    console.error("Error generating situation analysis report:", error);
    return NextResponse.json(
      { error: "Failed to generate report" },
      { status: 500 },
    );
  }
}
