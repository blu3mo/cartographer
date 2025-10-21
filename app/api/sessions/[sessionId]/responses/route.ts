import { type NextRequest, NextResponse } from "next/server";
import { getUserIdFromRequest } from "@/lib/auth";
import { prisma } from "@/lib/prisma";

export async function GET(
  request: NextRequest,
  { params }: { params: Promise<{ sessionId: string }> }
) {
  try {
    const { sessionId } = await params;
    const userId = getUserIdFromRequest(request);

    if (!userId) {
      return NextResponse.json(
        { error: "Unauthorized: Missing user ID" },
        { status: 401 },
      );
    }

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
        { error: "Participant not found in this session" },
        { status: 404 },
      );
    }

    const responses = await prisma.response.findMany({
      where: {
        participantUserId: userId,
        sessionId,
      },
      include: {
        statement: {
          select: {
            id: true,
            text: true,
            orderIndex: true,
          },
        },
      },
      orderBy: [
        { createdAt: "desc" },
        {
          statement: {
            orderIndex: "asc",
          },
        },
      ],
    });

    return NextResponse.json({
      responses: responses.map((response) => ({
        id: response.id,
        statementId: response.statementId,
        statementText: response.statement.text,
        orderIndex: response.statement.orderIndex,
        value: response.value,
        createdAt: response.createdAt,
      })),
    });
  } catch (error) {
    console.error("Failed to fetch participant responses:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
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
        { error: "Unauthorized: Missing user ID" },
        { status: 401 },
      );
    }

    const body = await request.json();
    const { statementId, value } = body;

    if (!statementId || value === undefined) {
      return NextResponse.json(
        { error: "Missing required fields: statementId and value" },
        { status: 400 },
      );
    }

    // Validate value is in range
    if (![-2, -1, 0, 1, 2].includes(value)) {
      return NextResponse.json(
        { error: "Invalid value: must be -2, -1, 0, 1, or 2" },
        { status: 400 },
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
        { error: "Statement not found in this session" },
        { status: 404 },
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
        { error: "Participant not found in this session" },
        { status: 404 },
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
    console.error("Response submission error:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}
