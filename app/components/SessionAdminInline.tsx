"use client";

import AdminPage from "@/app/sessions/[sessionId]/[accessToken]/page";

type SessionAdminInlineProps = {
  sessionId: string;
  accessToken: string;
};

// 管理画面を右ペインに直接描画するための薄いラッパー（ヘッダー等は非表示）
export default function SessionAdminInline({
  sessionId,
  accessToken,
}: SessionAdminInlineProps) {
  const params = Promise.resolve({ sessionId, accessToken });
  return <AdminPage params={params} embedded />;
}


