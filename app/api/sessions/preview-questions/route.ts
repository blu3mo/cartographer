import { type NextRequest, NextResponse } from "next/server";
import { getUserIdFromRequest } from "@/lib/auth";
import { generateSurveyStatements } from "../../../../agents/llm";

export async function POST(request: NextRequest) {
  try {
    const userId = getUserIdFromRequest(request);
    if (!userId) {
      return NextResponse.json(
        { error: "Unauthorized: Missing user ID" },
        { status: 401 },
      );
    }

    const body = await request.json();
    const { title, context, goal } = body as {
      title?: unknown;
      context?: unknown;
      goal?: unknown;
    };

    if (typeof title !== "string" || title.trim().length === 0) {
      return NextResponse.json(
        { error: "Missing required field: title" },
        { status: 400 },
      );
    }

    if (typeof goal !== "string" || goal.trim().length === 0) {
      return NextResponse.json(
        { error: "Missing required field: goal" },
        { status: 400 },
      );
    }

    if (typeof context !== "string") {
      return NextResponse.json(
        { error: "Invalid value for context" },
        { status: 400 },
      );
    }

    const trimmedTitle = title.trim();
    const trimmedGoal = goal.trim();
    const normalizedContext = context.trim();

    // Generate 15 questions
    try {
      const statementTexts = await generateSurveyStatements({
        sessionTitle: trimmedTitle,
        sessionGoal: trimmedGoal,
        initialContext: normalizedContext,
        eventThreadContext: "[]", // No event thread context for preview
        participantCount: 0,
      });

      return NextResponse.json({ questions: statementTexts });
    } catch (genError) {
      console.error("Failed to generate preview statements:", genError);
      return NextResponse.json(
        { error: "Failed to generate questions" },
        { status: 500 },
      );
    }
  } catch (error) {
    console.error("Preview generation error:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}
