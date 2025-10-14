import { NextRequest, NextResponse } from "next/server";
import { getUserIdFromRequest } from "@/lib/auth";
import { prisma } from "@/lib/prisma";
import { generateIndividualReport } from "@/lib/llm";

export async function GET(
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

    const participant = await prisma.participant.findUnique({
      where: {
        userId_sessionId: {
          userId,
          sessionId,
        },
      },
      include: {
        latestIndividualReport: true,
      },
    });

    if (!participant) {
      return NextResponse.json(
        { error: "Forbidden: You are not a participant in this session" },
        { status: 403 }
      );
    }

    const report = participant.latestIndividualReport;

    if (!report) {
      return NextResponse.json({ report: null });
    }

    return NextResponse.json({
      report: {
        id: report.id,
        participantUserId: report.participantUserId,
        sessionId: report.sessionId,
        contentMarkdown: report.contentMarkdown,
        createdAt: report.createdAt,
      },
    });
  } catch (error) {
    console.error("Error fetching individual report:", error);
    return NextResponse.json(
      { error: "Failed to fetch individual report" },
      { status: 500 }
    );
  }
}

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

    // Verify that the user is a participant in this session
    const participant = await prisma.participant.findUnique({
      where: {
        userId_sessionId: {
          userId,
          sessionId,
        },
      },
    });

    if (!participant) {
      return NextResponse.json(
        { error: "Forbidden: You are not a participant in this session" },
        { status: 403 }
      );
    }

    // Get session context
    const session = await prisma.session.findUnique({
      where: { id: sessionId },
    });

    if (!session) {
      return NextResponse.json(
        { error: "Session not found" },
        { status: 404 }
      );
    }

    // Fetch all responses for this participant with statement text
    const responses = await prisma.response.findMany({
      where: {
        participantUserId: userId,
        sessionId,
      },
      include: {
        statement: true,
      },
    });

    if (responses.length === 0) {
      return NextResponse.json(
        { error: "No responses found. Please answer some questions first." },
        { status: 400 }
      );
    }

    // Format responses for LLM
    const responsesWithStatement = responses.map((r) => ({
      statementText: r.statement.text,
      value: r.value,
    }));

    // Generate individual report using LLM
    const reportContent = await generateIndividualReport(
      session.context,
      responsesWithStatement,
      participant.name
    );

    // Save report to database
    const report = await prisma.individualReport.create({
      data: {
        participantUserId: userId,
        sessionId,
        contentMarkdown: reportContent,
      },
    });

    // Update participant's latest report reference
    await prisma.participant.update({
      where: {
        userId_sessionId: {
          userId,
          sessionId,
        },
      },
      data: {
        latestIndividualReportId: report.id,
      },
    });

    return NextResponse.json({
      report: {
        id: report.id,
        participantUserId: report.participantUserId,
        sessionId: report.sessionId,
        contentMarkdown: report.contentMarkdown,
        createdAt: report.createdAt,
      },
    });
  } catch (error) {
    console.error("Error generating individual report:", error);
    return NextResponse.json(
      { error: "Failed to generate individual report" },
      { status: 500 }
    );
  }
}
