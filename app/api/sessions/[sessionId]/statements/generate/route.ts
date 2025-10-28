import { type NextRequest, NextResponse } from "next/server";

import { getUserIdFromRequest } from "@/lib/auth";
import { buildSessionBrief, generateNewStatements } from "@/lib/llm";
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
      .select("id, title, context, goal, host_user_id")
      .eq("id", sessionId)
      .single();

    if (sessionError || !session) {
      if (sessionError?.code === "PGRST116") {
        return NextResponse.json(
          { error: "Session not found" },
          { status: 404 },
        );
      }
      console.error("Failed to load session:", sessionError);
      return NextResponse.json({ error: "Session not found" }, { status: 404 });
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
      console.error("Failed to fetch statements:", statementsError);
      return NextResponse.json(
        { error: "Failed to generate new statements" },
        { status: 500 },
      );
    }

    const { data: responses, error: responsesError } = await supabase
      .from("responses")
      .select("statement_id, value")
      .eq("session_id", sessionId);

    if (responsesError) {
      console.error("Failed to fetch responses:", responsesError);
      return NextResponse.json(
        { error: "Failed to generate new statements" },
        { status: 500 },
      );
    }

    const responseMap = new Map<string, { value: number }[]>();
    (responses ?? []).forEach((response) => {
      const list = responseMap.get(response.statement_id) ?? [];
      list.push({ value: response.value });
      responseMap.set(response.statement_id, list);
    });

    const statementsWithStats = (statements ?? []).map((statement) => {
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

      return {
        text: statement.text,
        responses: {
          strongYes: Math.round(strongYesPercent * 100) / 100,
          yes: Math.round(yesPercent * 100) / 100,
          dontKnow: Math.round(dontKnowPercent * 100) / 100,
          no: Math.round(noPercent * 100) / 100,
          strongNo: Math.round(strongNoPercent * 100) / 100,
          totalCount,
        },
      };
    });

    const { data: latestReport, error: latestReportError } = await supabase
      .from("situation_analysis_reports")
      .select("id, session_id, content_markdown, created_at")
      .eq("session_id", sessionId)
      .order("created_at", { ascending: false })
      .limit(1)
      .maybeSingle();

    if (latestReportError && latestReportError.code !== "PGRST116") {
      console.error("Failed to fetch latest report:", latestReportError);
      return NextResponse.json(
        { error: "Failed to generate new statements" },
        { status: 500 },
      );
    }

    // Generate new statements using LLM
    const sessionBrief = buildSessionBrief(session.goal, session.context);
    const newStatementTexts = await generateNewStatements(
      session.title,
      sessionBrief,
      statementsWithStats,
      latestReport?.content_markdown,
    );

    // Get the maximum order index
    const maxOrderIndex =
      (statements ?? []).reduce(
        (max, s) => Math.max(max, s.order_index ?? 0),
        -1,
      ) ?? -1;

    // Save new statements to database
    if (newStatementTexts.length === 0) {
      return NextResponse.json({ newStatements: [] });
    }

    const newStatementsPayload = newStatementTexts.map((text, index) => ({
      session_id: sessionId,
      text,
      order_index: maxOrderIndex + 1 + index,
    }));

    const { data: insertedStatements, error: insertError } = await supabase
      .from("statements")
      .insert(newStatementsPayload)
      .select("id, session_id, text, order_index, created_at");

    if (insertError || !insertedStatements) {
      console.error("Failed to insert new statements:", insertError);
      return NextResponse.json(
        { error: "Failed to generate new statements" },
        { status: 500 },
      );
    }

    return NextResponse.json({
      newStatements: insertedStatements.map((s) => ({
        id: s.id,
        sessionId: s.session_id,
        text: s.text,
        orderIndex: s.order_index,
        createdAt: s.created_at,
      })),
    });
  } catch (error) {
    console.error("Error generating new statements:", error);
    return NextResponse.json(
      { error: "Failed to generate new statements" },
      { status: 500 },
    );
  }
}
