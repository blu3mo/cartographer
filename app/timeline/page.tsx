// このページはサーバーコンポーネント。メタデータをエクスポートするため `use client` は付与しない。
import type { Metadata } from "next";
import Link from "next/link";
import { Button } from "@/components/ui/Button";
import { ArrowRight, Sparkles } from "lucide-react";
import { AppHeader } from "@/components/AppHeader";
import { MarketingNav } from "@/components/MarketingNav";
import { getPublicSessions } from "@/lib/server/public-sessions";
import { SessionCard } from "./_components/SessionCard";
import { AboutCartographerButton } from "@/components/AboutCartographerButton";

const appUrl = process.env.NEXT_PUBLIC_APP_URL ?? "http://localhost:3000";
const metadataBase = new URL(appUrl);
const timelineTitle = "タイムライン | Cartographer";
const timelineDescription =
  "公開されているセッション一覧。さまざまなチームの認識マップを閲覧できます。";

export const metadata: Metadata = {
  metadataBase,
  title: timelineTitle,
  description: timelineDescription,
  openGraph: {
    title: timelineTitle,
    description: timelineDescription,
    url: `${metadataBase}/timeline`,
    siteName: "Cartographer",
    type: "website",
  },
  twitter: {
    card: "summary_large_image",
    title: timelineTitle,
    description: timelineDescription,
  },
};

export default async function TimelinePage() {
  const sessions = await getPublicSessions();

  return (
    <div className="min-h-screen bg-gradient-to-b from-slate-50 to-white">
      <AppHeader rightSlot={<AboutCartographerButton />}>
        {/* <MarketingNav /> */}
      </AppHeader>

      {/* Hero Section */}
      <section className="container mx-auto px-4 py-12 md:py-20">
        <div className="max-w-4xl mx-auto text-center">
          {/* <div className="inline-flex items-center gap-2 px-4 py-2 rounded-full bg-blue-50 text-blue-700 text-sm font-medium mb-6">
            <Sparkles className="h-4 w-4" />
            公開セッション
          </div> */}

          <h1 className="text-4xl md:text-5xl font-bold tracking-tight mb-4 bg-gradient-to-r from-slate-900 to-slate-700 bg-clip-text text-transparent">
            タイムライン
          </h1>


        </div>
      </section>

      {/* Sessions List */}
      <section className="container mx-auto px-4 py-12">
        <div className="max-w-6xl mx-auto">
          {sessions.length === 0 ? (
            <div className="text-center py-20">
              <div className="max-w-md mx-auto">
                <div className="h-16 w-16 rounded-full bg-slate-100 flex items-center justify-center mx-auto mb-4">
                  <Sparkles className="h-8 w-8 text-slate-400" />
                </div>
                <h3 className="text-xl font-semibold text-slate-900 mb-2">
                  公開セッションがありません
                </h3>
                <p className="text-slate-600 mb-6">
                  まだ公開されているセッションはありません。
                  <br />
                  最初のセッションを作成してみませんか？
                </p>
                <Link href="/dashboard">
                  <Button size="lg">
                    セッションを作成
                    <ArrowRight className="ml-2 h-4 w-4" />
                  </Button>
                </Link>
              </div>
            </div>
          ) : (
            <>
              <div className="mb-8">
                <h2 className="text-2xl font-bold text-slate-900">
                  公開セッション一覧
                </h2>
                <p className="text-slate-600 mt-2">
                  {sessions.length} 件のセッションが公開されています
                </p>
              </div>

              <div className="grid gap-6 md:grid-cols-2 lg:grid-cols-3">
                {sessions.map((session) => (
                  <SessionCard key={session.id} session={session} />
                ))}
              </div>
            </>
          )}
        </div>
      </section>
      <section className="container mx-auto px-4 py-12 md:py-20">
        <div className="max-w-4xl mx-auto text-center">
          <div className="flex flex-col sm:flex-row gap-4 justify-center">
            <Link href="/dashboard">
              <Button size="lg" className="text-base px-8">
                自分のセッションを作成
                <ArrowRight className="ml-2 h-4 w-4" />
              </Button>
            </Link>
          </div>
        </div>
      </section>

      {/* <AppFooter /> */}
    </div>
  );
}
