import { NextRequest, NextResponse } from "next/server";
import { getUserIdFromRequest } from "@/lib/auth";
import { prisma } from "@/lib/prisma";
import { generateNewStatements } from "@/lib/llm";

type ResponseValue = -2 | -1 | 0 | 1 | 2;

export async function POST(
  request: NextRequest,
  { params }: { params: Promise<{ sessionId: string }> }
) {
  try {
    const { sessionId } = await params;
    const userId = getUserIdFromRequest(request);

    if (!userId) {
      return NextResponse.json(
        { error: "Unauthorized: User ID not found" },
        { status: 401 }
      );
    }

    // Verify that the user is the host of this session
    const session = await prisma.session.findUnique({
      where: { id: sessionId },
    });

    if (!session) {
      return NextResponse.json(
        { error: "Session not found" },
        { status: 404 }
      );
    }

    if (session.hostUserId !== userId) {
      return NextResponse.json(
        { error: "Forbidden: You are not the host of this session" },
        { status: 403 }
      );
    }

    // Fetch all statements with their responses
    const statements = await prisma.statement.findMany({
      where: { sessionId },
      include: {
        responses: true,
      },
      orderBy: { orderIndex: "asc" },
    });

    // Calculate statistics for each statement
    const statementsWithStats = statements.map((statement) => {
      const responses = statement.responses;
      const totalCount = responses.length;

      let strongYesCount = 0;
      let yesCount = 0;
      let dontKnowCount = 0;
      let noCount = 0;
      let strongNoCount = 0;

      responses.forEach((response) => {
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

    // Get the latest situation analysis report
    const latestReport = await prisma.situationAnalysisReport.findFirst({
      where: { sessionId },
      orderBy: { createdAt: "desc" },
    });

    // Generate new statements using LLM
    const newStatementTexts = await generateNewStatements(
      session.title,
      session.context,
      statementsWithStats,
      latestReport?.contentMarkdown
    );

    // Get the maximum order index
    const maxOrderIndex = statements.reduce(
      (max, s) => Math.max(max, s.orderIndex),
      -1
    );

    // Save new statements to database
    const newStatements = await Promise.all(
      newStatementTexts.map((text, index) =>
        prisma.statement.create({
          data: {
            sessionId,
            text,
            orderIndex: maxOrderIndex + 1 + index,
          },
        })
      )
    );

    return NextResponse.json({
      newStatements: newStatements.map((s) => ({
        id: s.id,
        sessionId: s.sessionId,
        text: s.text,
        orderIndex: s.orderIndex,
        createdAt: s.createdAt,
      })),
    });
  } catch (error) {
    console.error("Error generating new statements:", error);
    return NextResponse.json(
      { error: "Failed to generate new statements" },
      { status: 500 }
    );
  }
}
