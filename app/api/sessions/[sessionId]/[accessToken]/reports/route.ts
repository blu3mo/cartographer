import { type NextRequest, NextResponse } from "next/server";

export const dynamic = "force-dynamic";

import { getUserIdFromRequest } from "@/lib/auth";
import {
  requireSessionAdminAccess,
  requireSessionAdminToken,
  SessionAccessError,
} from "@/lib/server/session-access";
import {
  createSessionReportRecord,
  listSessionReports,
  triggerSessionReportGeneration,
} from "@/lib/server/session-reports";

function handleAccessError(error: unknown) {
  if (error instanceof SessionAccessError) {
    return NextResponse.json(
      { error: error.message },
      { status: error.status },
    );
  }
  console.error(error);
  return NextResponse.json({ error: "Internal Server Error" }, { status: 500 });
}

export async function GET(
  request: NextRequest,
  { params }: { params: Promise<{ sessionId: string; accessToken: string }> },
) {
  const { sessionId, accessToken } = await params;
  const userId = getUserIdFromRequest(request);

  if (!userId) {
    return NextResponse.json({ error: "Unauthorized" }, { status: 401 });
  }

  try {
    await requireSessionAdminToken(sessionId, accessToken);
    const reports = await listSessionReports(sessionId);
    return NextResponse.json({ data: reports });
  } catch (error) {
    return handleAccessError(error);
  }
}

export async function POST(
  request: NextRequest,
  { params }: { params: Promise<{ sessionId: string; accessToken: string }> },
) {
  const { sessionId, accessToken } = await params;
  const userId = getUserIdFromRequest(request);

  if (!userId) {
    return NextResponse.json({ error: "Unauthorized" }, { status: 401 });
  }

  let payload: { requestMarkdown?: string } = {};
  try {
    payload = (await request.json()) as { requestMarkdown?: string };
  } catch (error) {
    console.warn("Invalid request payload for session report:", error);
  }

  try {
    await requireSessionAdminAccess(sessionId, accessToken, userId);
    const report = await createSessionReportRecord({
      sessionId,
      userId,
      requestMarkdown: payload.requestMarkdown,
    });

    void triggerSessionReportGeneration(report.id);

    return NextResponse.json(
      { data: report },
      {
        status: 201,
      },
    );
  } catch (error) {
    return handleAccessError(error);
  }
}
