import type { Metadata } from "next";

import { buildMetadata, truncateForMeta } from "@/lib/metadata";

import SessionReportPrintPage from "./client-page";

async function fetchReportMeta(
  sessionId: string,
  accessToken: string,
  reportId: string,
) {
  const [{ requireSessionAdminToken }, { getSessionReportById }] =
    await Promise.all([
      import("@/lib/server/session-access"),
      import("@/lib/server/session-reports"),
    ]);

  const session = await requireSessionAdminToken(sessionId, accessToken).catch(
    () => null,
  );
  const report = await getSessionReportById(reportId).catch(() => null);

  if (!session || !report || report.sessionId !== sessionId) {
    return null;
  }

  return { session, report };
}

export async function generateMetadata({
  params,
}: {
  params: Promise<{
    sessionId: string;
    accessToken: string;
    reportId: string;
  }>;
}): Promise<Metadata> {
  const { sessionId, accessToken, reportId } = await params;
  const meta = await fetchReportMeta(sessionId, accessToken, reportId);

  if (!meta) {
    return buildMetadata({
      title: "レポートが見つかりません | 倍速会議",
      description: "指定されたレポートは存在しないか、アクセス権がありません。",
      url: `/sessions/${sessionId}/${accessToken}/reports/${reportId}/print`,
    });
  }

  const versionLabel = `v${String(meta.report.version ?? 0).padStart(2, "0")}`;
  const goal = meta.session.goal?.trim();
  const description = goal
    ? `「${meta.session.title}」のレポート${versionLabel}（印刷用）。目的: ${truncateForMeta(goal)}`
    : `「${meta.session.title}」のレポート${versionLabel}（印刷用）。倍速会議で集計した結果を確認できます。`;

  return buildMetadata({
    title: `${meta.session.title} | レポート${versionLabel} | 倍速会議`,
    description,
    url: `/sessions/${sessionId}/${accessToken}/reports/${reportId}/print`,
  });
}

export default async function Page({
  params,
}: {
  params: Promise<{ sessionId: string; accessToken: string; reportId: string }>;
}) {
  const { sessionId, accessToken, reportId } = await params;
  return (
    <SessionReportPrintPage
      sessionId={sessionId}
      accessToken={accessToken}
      reportId={reportId}
    />
  );
}
