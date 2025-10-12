import { NextRequest, NextResponse } from 'next/server';
import { prisma } from '@/lib/prisma';
import { getUserIdFromRequest } from '@/lib/auth';

export async function POST(
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

    const body = await request.json();
    const { statementId, value } = body;

    if (!statementId || value === undefined) {
      return NextResponse.json(
        { error: 'Missing required fields: statementId and value' },
        { status: 400 }
      );
    }

    // Validate value is in range
    if (![-2, -1, 0, 1, 2].includes(value)) {
      return NextResponse.json(
        { error: 'Invalid value: must be -2, -1, 0, 1, or 2' },
        { status: 400 }
      );
    }

    // Verify statement belongs to session
    const statement = await prisma.statement.findFirst({
      where: {
        id: statementId,
        sessionId,
      },
    });

    if (!statement) {
      return NextResponse.json(
        { error: 'Statement not found in this session' },
        { status: 404 }
      );
    }

    // Verify participant exists in session
    const participant = await prisma.participant.findUnique({
      where: {
        userId_sessionId: {
          userId: userId,
          sessionId,
        },
      },
    });

    if (!participant) {
      return NextResponse.json(
        { error: 'Participant not found in this session' },
        { status: 404 }
      );
    }

    // Create or update response
    const response = await prisma.response.upsert({
      where: {
        participantUserId_sessionId_statementId: {
          participantUserId: userId,
          sessionId,
          statementId,
        },
      },
      create: {
        participantUserId: userId,
        sessionId,
        statementId,
        value,
      },
      update: {
        value,
      },
    });

    return NextResponse.json({ success: true, response });
  } catch (error) {
    console.error('Response submission error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
