import type { Metadata } from "next";

import { DashboardClient } from "./_components/DashboardClient";

const title = "セッションダッシュボード | Cartographer";
const description =
  "Cartographer のセッション管理ダッシュボードで、進行状況と参加状況をまとめて確認できます。";

export const metadata: Metadata = {
  title,
  description,
};

export default function DashboardPage() {
  return <DashboardClient />;
}
