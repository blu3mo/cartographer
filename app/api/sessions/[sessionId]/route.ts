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
        { error: "Unauthorized: Missing user ID" },
        { status: 401 },
      );
    }

    const session = await prisma.session.findUnique({
      where: { id: sessionId },
      include: {
        participants: {
          select: { userId: true },
        },
      },
    });

    if (!session) {
      return NextResponse.json({ error: "Session not found" }, { status: 404 });
    }

    const isHost = session.hostUserId === userId;
    const isParticipant = session.participants.some(
      (participant) => participant.userId === userId,
    );

    const { participants, ...sessionData } = session;

    return NextResponse.json({
      session: {
        ...sessionData,
        isHost,
        isParticipant,
      },
    });
  } catch (error) {
    console.error("Session fetch error:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}
