import type { Metadata } from "next";

import { buildMetadata } from "@/lib/metadata";

import NewSessionPage from "./client-page";

export const metadata: Metadata = buildMetadata({
  title: "セッションを作成 | 倍速会議",
  description:
    "チームでの合意形成に向けて、倍速会議で新しいセッションを作成します。",
  url: "/sessions/new",
});

export default function Page() {
  return <NewSessionPage />;
}
