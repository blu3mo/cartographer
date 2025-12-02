import type { Metadata } from "next";

import { buildMetadata, truncateForMeta } from "@/lib/metadata";

import SessionPage from "./client-page";

async function getSessionMeta(sessionId: string) {
  const { supabase } = await import("@/lib/supabase");
  const { data, error } = await supabase
    .from("sessions")
    .select("title, goal, context")
    .eq("id", sessionId)
    .single();

  if (error || !data) {
    return null;
  }

  return data;
}

export async function generateMetadata({
  params,
}: {
  params: Promise<{ sessionId: string }>;
}): Promise<Metadata> {
  const { sessionId } = await params;
  const session = await getSessionMeta(sessionId);

  if (!session) {
    return buildMetadata({
      title: "セッションが見つかりません | 倍速会議",
      description:
        "指定されたセッションは存在しないか、アクセス権がありません。",
      url: `/sessions/${sessionId}`,
    });
  }

  const goal = session.goal?.trim();
  const context = session.context?.trim();
  const description = goal
    ? `「${session.title}」の回答ページ。目的: ${truncateForMeta(goal)}`
    : context
      ? `「${session.title}」の回答ページ。背景: ${truncateForMeta(context)}`
      : `「${session.title}」の回答ページ。倍速会議で認識を可視化し、合意形成を促進します。`;

  return buildMetadata({
    title: `${session.title} | 倍速会議`,
    description,
    url: `/sessions/${sessionId}`,
  });
}

export default async function Page({
  params,
}: {
  params: Promise<{ sessionId: string }>;
}) {
  const { sessionId } = await params;
  return <SessionPage sessionId={sessionId} />;
}
