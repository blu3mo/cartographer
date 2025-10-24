import { type NextRequest, NextResponse } from "next/server";
import { getUserIdFromRequest } from "@/lib/auth";
import { prisma } from "@/lib/prisma";

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

    const reports = await prisma.situationAnalysisReport.findMany({
      where: { sessionId },
      orderBy: { createdAt: "desc" },
    });

    return NextResponse.json({
      reports: reports.map((report) => ({
        id: report.id,
        sessionId: report.sessionId,
        contentMarkdown: report.contentMarkdown,
        createdAt: report.createdAt,
      })),
    });
  } catch (error) {
    console.error("Error fetching reports:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}
