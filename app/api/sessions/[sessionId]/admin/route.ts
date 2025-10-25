import { type NextRequest, NextResponse } from "next/server";
import { getUserIdFromRequest } from "@/lib/auth";
import { prisma } from "@/lib/prisma";

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
    const session = await prisma.session.findUnique({
      where: { id: sessionId },
    });

    if (!session) {
      return NextResponse.json({ error: "Session not found" }, { status: 404 });
    }

    if (session.hostUserId !== userId) {
      return NextResponse.json(
        { error: "Forbidden: You are not the host of this session" },
        { status: 403 },
      );
    }

    // Fetch all statements for this session
    const statements = await prisma.statement.findMany({
      where: { sessionId },
      include: {
        responses: true,
      },
      orderBy: { orderIndex: "asc" },
    });

    type StatementWithResponses = (typeof statements)[number];
    type ResponseWithValue = StatementWithResponses["responses"][number];

    // Calculate statistics for each statement
    const statementsWithStats: StatementWithStats[] = statements.map(
      (statement: StatementWithResponses) => {
        const responses = statement.responses;
        const totalCount = responses.length;

        let strongYesCount = 0;
        let yesCount = 0;
        let dontKnowCount = 0;
        let noCount = 0;
        let strongNoCount = 0;

        responses.forEach((response: ResponseWithValue) => {
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
          sessionId: statement.sessionId,
          text: statement.text,
          orderIndex: statement.orderIndex,
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
    const latestReport = await prisma.situationAnalysisReport.findFirst({
      where: { sessionId },
      orderBy: { createdAt: "desc" },
    });

    return NextResponse.json({
      data: {
        id: session.id,
        title: session.title,
        context: session.context,
        isPublic: session.isPublic,
        createdAt: session.createdAt,
        statements: statementsWithStats,
        latestSituationAnalysisReport: latestReport
          ? {
              id: latestReport.id,
              sessionId: latestReport.sessionId,
              contentMarkdown: latestReport.contentMarkdown,
              createdAt: latestReport.createdAt,
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

    const session = await prisma.session.findUnique({
      where: { id: sessionId },
    });

    if (!session) {
      return NextResponse.json({ error: "Session not found" }, { status: 404 });
    }

    if (session.hostUserId !== userId) {
      return NextResponse.json(
        { error: "Forbidden: You are not the host of this session" },
        { status: 403 },
      );
    }

    const body = await request.json();
    const { title, context, isPublic } = body as {
      title?: unknown;
      context?: unknown;
      isPublic?: unknown;
    };

    if (typeof title !== "string" || title.trim().length === 0) {
      return NextResponse.json({ error: "Invalid title" }, { status: 400 });
    }

    if (typeof context !== "string" || context.trim().length === 0) {
      return NextResponse.json({ error: "Invalid context" }, { status: 400 });
    }

    if (typeof isPublic !== "boolean") {
      return NextResponse.json(
        { error: "Invalid visibility" },
        { status: 400 },
      );
    }

    const updatedSession = await prisma.session.update({
      where: { id: sessionId },
      data: {
        title: title.trim(),
        context: context.trim(),
        isPublic,
      },
    });

    return NextResponse.json({
      data: {
        id: updatedSession.id,
        title: updatedSession.title,
        context: updatedSession.context,
        isPublic: updatedSession.isPublic,
        createdAt: updatedSession.createdAt,
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

    const session = await prisma.session.findUnique({
      where: { id: sessionId },
    });

    if (!session) {
      return NextResponse.json({ error: "Session not found" }, { status: 404 });
    }

    if (session.hostUserId !== userId) {
      return NextResponse.json(
        { error: "Forbidden: You are not the host of this session" },
        { status: 403 },
      );
    }

    await prisma.session.delete({
      where: { id: sessionId },
    });

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
