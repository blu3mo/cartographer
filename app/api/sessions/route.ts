import { NextRequest, NextResponse } from 'next/server';
import { getUserIdFromRequest } from '@/lib/auth';
import { generateInitialStatements } from '@/lib/llm';
import { prisma } from '@/lib/prisma';

export async function GET(request: NextRequest) {
  try {
    const userId = getUserIdFromRequest(request);
    if (!userId) {
      return NextResponse.json(
        { error: 'Unauthorized: Missing user ID' },
        { status: 401 }
      );
    }

    // Get all sessions and compute role flags for the current user
    const sessions = await prisma.session.findMany({
      where: {
        OR: [
          { hostUserId: userId },
          { participants: { some: { userId } } },
          { isPublic: true },
        ],
      },
      include: {
        participants: {
          select: { userId: true },
        },
        _count: {
          select: {
            participants: true,
            statements: true,
          },
        },
      },
      orderBy: {
        createdAt: 'desc',
      },
    });

    const sessionsWithRoles = sessions.map((session) => {
      const { participants, ...rest } = session;
      const isHost = session.hostUserId === userId;
      const isParticipant = participants.some(
        (participant) => participant.userId === userId
      );

      return {
        ...rest,
        isHost,
        isParticipant,
      };
    });

    return NextResponse.json({ sessions: sessionsWithRoles });
  } catch (error) {
    console.error('Sessions fetch error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

export async function POST(request: NextRequest) {
  try {
    const userId = getUserIdFromRequest(request);
    if (!userId) {
      return NextResponse.json(
        { error: 'Unauthorized: Missing user ID' },
        { status: 401 }
      );
    }

    const body = await request.json();
    const { title, context, isPublic } = body;

    if (!title || !context) {
      return NextResponse.json(
        { error: 'Missing required fields: title and context' },
        { status: 400 }
      );
    }

    // Create session
    const session = await prisma.session.create({
      data: {
        title,
        context,
        isPublic: typeof isPublic === 'boolean' ? isPublic : true,
        hostUserId: userId,
      },
    });

    // Generate initial statements using LLM (with fallback to defaults)
    const statementTexts = await generateInitialStatements(title, context);

    // Save statements to database
    await prisma.statement.createMany({
      data: statementTexts.map((text, index) => ({
        sessionId: session.id,
        text,
        orderIndex: index,
      })),
    });

    return NextResponse.json({ session });
  } catch (error) {
    console.error('Session creation error:', error);
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
