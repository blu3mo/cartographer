import { NextRequest, NextResponse } from 'next/server';
import { prisma } from '@/lib/prisma';
import { getUserIdFromRequest } from '@/lib/auth';
import { generateInitialStatements } from '@/lib/llm';

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
    const { title, context } = body;

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
        hostUserId: userId,
      },
    });

    // Generate initial statements using LLM (with fallback to defaults)
    const statementTexts = await generateInitialStatements(context);

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
