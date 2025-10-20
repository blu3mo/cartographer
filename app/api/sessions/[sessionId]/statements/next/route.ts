import { NextRequest, NextResponse } from 'next/server';
import { prisma } from '@/lib/prisma';
import { getUserIdFromRequest } from '@/lib/auth';

export async function GET(
  request: NextRequest,
  { params }: { params: Promise<{ sessionId: string }> }
) {
  try {
    const { sessionId } = await params;
    const userId = getUserIdFromRequest(request);

    if (!userId) {
      return NextResponse.json(
        { error: 'Unauthorized: Missing user ID' },
        { status: 401 }
      );
    }

    // Check if user is a participant in this session
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
        { error: 'Unauthorized: Not a participant in this session' },
        { status: 401 }
      );
    }

    // Get excludeStatementId(s) from query params if provided
    const { searchParams } = new URL(request.url);
    const excludeStatementIds = new Set(
      searchParams.getAll('excludeStatementId').filter(Boolean)
    );

    // Get all statements for this session
    const allStatements = await prisma.statement.findMany({
      where: { sessionId },
      orderBy: { orderIndex: 'asc' },
    });

    // Get all responses by this participant
    const existingResponses = await prisma.response.findMany({
      where: {
        participantUserId: userId,
        sessionId,
      },
      select: { statementId: true },
    });

    const answeredStatementIds = new Set(
      existingResponses.map((r) => r.statementId)
    );

    // Filter unanswered statements
    let unansweredStatements = allStatements.filter(
      (s) => !answeredStatementIds.has(s.id)
    );

    // Exclude the currently displayed statement(s) if provided
    if (excludeStatementIds.size > 0) {
      unansweredStatements = unansweredStatements.filter(
        (s) => !excludeStatementIds.has(s.id)
      );
    }

    if (unansweredStatements.length === 0) {
      return NextResponse.json({ statement: null });
    }

    // Return a random unanswered statement
    const randomIndex = Math.floor(Math.random() * unansweredStatements.length);
    const statement = unansweredStatements[randomIndex];

    return NextResponse.json({ statement });
  } catch (error) {
    console.error('Get next statement error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
