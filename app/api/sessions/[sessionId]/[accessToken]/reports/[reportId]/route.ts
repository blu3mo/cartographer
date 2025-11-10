import { type NextRequest, NextResponse } from "next/server";

import { getUserIdFromRequest } from "@/lib/auth";
import {
  requireSessionAdminToken,
  SessionAccessError,
} from "@/lib/server/session-access";
import {
  getSessionReportById,
  type SessionReportRecord,
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
  {
    params,
  }: {
    params: Promise<{
      sessionId: string;
      accessToken: string;
      reportId: string;
    }>;
  },
) {
  const { sessionId, accessToken, reportId } = await params;
  const userId = getUserIdFromRequest(request);

  if (!userId) {
    return NextResponse.json({ error: "Unauthorized" }, { status: 401 });
  }

  try {
    await requireSessionAdminToken(sessionId, accessToken);
    const report = await getSessionReportById(reportId);

    if (!report || report.sessionId !== sessionId) {
      return NextResponse.json({ error: "Report not found" }, { status: 404 });
    }

    return NextResponse.json<{ data: SessionReportRecord }>({
      data: report,
    });
  } catch (error) {
    return handleAccessError(error);
  }
}
