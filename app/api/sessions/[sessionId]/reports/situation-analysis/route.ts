import { type NextRequest, NextResponse } from "next/server";

import { getUserIdFromRequest } from "@/lib/auth";
import { generateSituationAnalysisReport } from "@/lib/llm";
import { prisma } from "@/lib/prisma";

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

    // Fetch all statements with their responses (including participant info)
    const statements = await prisma.statement.findMany({
      where: { sessionId },
      include: {
        responses: {
          include: {
            participant: {
              select: {
                userId: true,
                name: true,
              },
            },
          },
        },
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

      // Calculate agreement score for sorting
      // Strong Yes (2) + Yes (1) - No (-1) - Strong No (-2), weighted by count
      const agreementScore =
        (strongYesCount * 2 + yesCount * 1 - noCount * 1 - strongNoCount * 2) /
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

    // Sort statements by agreement score (descending)
    const sortedStatements = [...statementsWithStats].sort(
      (a, b) => b.agreementScore - a.agreementScore,
    );

    // Get total number of unique participants
    const uniqueParticipants = new Set(
      statements.flatMap((s) => s.responses.map((r) => r.participantUserId)),
    );
    const totalParticipants = uniqueParticipants.size;

    // If 10 or fewer participants, prepare individual responses
    let individualResponses = null;
    if (totalParticipants <= 10) {
      const participantResponsesMap = new Map<
        string,
        Array<{ statementText: string; value: number }>
      >();

      statements.forEach((statement) => {
        statement.responses.forEach((response) => {
          const participantUserId = response.participantUserId;
          const participantName = response.participant?.name || "Unknown";
          const key = `${participantUserId}:${participantName}`;

          let responsesForParticipant = participantResponsesMap.get(key);
          if (!responsesForParticipant) {
            responsesForParticipant = [];
            participantResponsesMap.set(key, responsesForParticipant);
          }

          responsesForParticipant.push({
            statementText: statement.text,
            value: response.value as number,
          });
        });
      });

      individualResponses = Array.from(participantResponsesMap.entries()).map(
        ([key, responses]) => {
          const [, name] = key.split(":");
          return { name, responses };
        },
      );
    }

    // Generate report using LLM
    const reportContent = await generateSituationAnalysisReport(
      session.title,
      session.context,
      sortedStatements,
      totalParticipants,
      individualResponses,
    );

    // Save report to database
    const report = await prisma.situationAnalysisReport.create({
      data: {
        sessionId,
        contentMarkdown: reportContent,
      },
    });

    return NextResponse.json({
      report: {
        id: report.id,
        sessionId: report.sessionId,
        contentMarkdown: report.contentMarkdown,
        createdAt: report.createdAt,
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
