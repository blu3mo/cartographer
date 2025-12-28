import type { Metadata } from "next";

import { buildMetadata, truncateForMeta } from "@/lib/metadata";

import AdminPage from "./client-page";

export async function generateMetadata({
  params,
}: {
  params: Promise<{ sessionId: string; accessToken: string }>;
}): Promise<Metadata> {
  const { sessionId, accessToken } = await params;

  const { requireSessionAdminToken } = await import(
    "@/lib/server/session-access"
  );
  const session = await requireSessionAdminToken(sessionId, accessToken).catch(
    () => null,
  );

  if (!session) {
    return buildMetadata({
      title: "セッションが見つかりません | 倍速会議",
      description:
        "指定されたセッションは存在しないか、アクセス権がありません。",
      url: `/sessions/${sessionId}/${accessToken}`,
    });
  }

  const goal = session.goal?.trim();
  const description = goal
    ? `「${session.title}」の回答・管理ページ。目的: ${truncateForMeta(goal)}`
    : `「${session.title}」の回答・管理ページ。倍速会議で認識を可視化し、合意形成を促進します。`;

  return buildMetadata({
    title: `${session.title} | セッション管理 | 倍速会議`,
    description,
    url: `/sessions/${sessionId}/${accessToken}`,
  });
}

export default async function Page({
  params,
}: {
  params: Promise<{ sessionId: string; accessToken: string }>;
}) {
  const { sessionId, accessToken } = await params;
  return <AdminPage sessionId={sessionId} accessToken={accessToken} />;
}
