import { Loader2 } from "lucide-react";
import type { Metadata } from "next";
import { Suspense } from "react";

import { buildMetadata } from "@/lib/metadata";

import NewSessionClientPage from "./client-page";

export const metadata: Metadata = buildMetadata({
  title: "セッションを作成 | 倍速会議",
  description:
    "チームでの合意形成に向けて、倍速会議で新しいセッションを作成します。",
  url: "/sessions/new",
});

export default function NewSessionPage() {
  return (
    <Suspense
      fallback={
        <div className="min-h-screen flex items-center justify-center">
          <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
        </div>
      }
    >
      <NewSessionClientPage />
    </Suspense>
  );
}
