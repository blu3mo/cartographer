import { NextRequest, NextResponse } from "next/server";
import { prisma } from "@/lib/prisma";
import { getUserIdFromRequest } from "@/lib/auth";

export async function POST(
  request: NextRequest,
  { params }: { params: Promise<{ sessionId: string }> },
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
    const { name } = body;

    if (!name) {
      return NextResponse.json(
        { error: "Missing required field: name" },
        { status: 400 },
      );
    }

    // Check if session exists
    const session = await prisma.session.findUnique({
      where: { id: sessionId },
    });

    if (!session) {
      return NextResponse.json({ error: "Session not found" }, { status: 404 });
    }

    // Check if participant already exists
    const existingParticipant = await prisma.participant.findUnique({
      where: {
        userId_sessionId: {
          userId: userId,
          sessionId,
        },
      },
    });

    if (existingParticipant) {
      return NextResponse.json(
        { error: "Already participating in this session" },
        { status: 409 },
      );
    }

    // Create participant
    const participant = await prisma.participant.create({
      data: {
        userId: userId,
        sessionId,
        name,
      },
    });

    return NextResponse.json({ participant });
  } catch (error) {
    console.error("Participant registration error:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}
