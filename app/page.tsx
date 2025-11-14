import type { Metadata } from "next";
import Image from "next/image";
import Link from "next/link";
import { Button } from "@/components/ui/Button";
import { Users, BarChart3, Sparkles, ArrowRight, CheckCircle2 } from "lucide-react";
import { AppHeader } from "./components/AppHeader";
import { MarketingNav } from "./components/MarketingNav";
import { AboutCartographerButton } from "./components/AboutCartographerButton";
import { getPublicSessions } from "@/lib/server/public-sessions";

const appUrl = process.env.NEXT_PUBLIC_APP_URL ?? "http://localhost:3000";
const metadataBase = new URL(appUrl);
const homeTitle = "Cartographer | チームの認識をマップする";
const homeDescription =
  "AIを活用してチーム内の多様な視点を収集・分析し、データに基づいた意思決定とコンセンサス形成を支援します。";

const clientLogos = [
  { src: "/logos/sample-logo-1.svg", alt: "クライアントロゴサンプル1" },
  { src: "/logos/sample-logo-2.svg", alt: "クライアントロゴサンプル2" },
  { src: "/logos/sample-logo-3.svg", alt: "クライアントロゴサンプル3" },
  { src: "/logos/sample-logo-4.svg", alt: "クライアントロゴサンプル4" },
  { src: "/logos/sample-logo-5.svg", alt: "クライアントロゴサンプル5" },
  { src: "/logos/sample-logo-6.svg", alt: "クライアントロゴサンプル6" },
];

const testimonials = [
  {
    quote:
      "Cartographer のレポートで論点が整理され、参加者全員が次に議論すべきテーマを迷わず共有できました。",
    name: "黒澤 亮",
    role: "プロダクトマネージャー",
    organization: "DMM.com",
  },
  {
    quote:
      "経営陣の温度感の違いが数分で可視化され、クライアントとの認識合わせが驚くほどスムーズになりました。",
    name: "結城 侑",
    role: "コンサルタント",
    organization: "株式会社BUTAI",
  },
  {
    quote:
      "住民ワークショップの場で Cartographer を使うことで、筋の良い論点が即座に浮かび上がり、合意形成の糸口が見つかります。",
    name: "前田 紘司",
    role: "ファシリテーター",
    organization: "構想日本",
  },
];

export const metadata: Metadata = {
  metadataBase,
  title: homeTitle,
  description: homeDescription,
  openGraph: {
    title: homeTitle,
    description: homeDescription,
    url: metadataBase,
    siteName: "Cartographer",
    type: "website",
  },
  twitter: {
    card: "summary_large_image",
    title: homeTitle,
    description: homeDescription,
  },
};

export default async function LandingPage() {
  const sessions = await getPublicSessions();
  const featuredSessions = sessions.slice(0, 3);

  return (
    <div className="min-h-screen bg-gradient-to-b from-slate-50 to-white">
      <AppHeader rightSlot={<AboutCartographerButton />}>
        {/* <MarketingNav /> */}
      </AppHeader>

      {/* Hero Section */}
      <section className="container mx-auto px-4 py-20 md:py-32">
        <div className="max-w-4xl mx-auto text-center">
          <div className="inline-flex items-center gap-2 px-4 py-2 rounded-full bg-blue-50 text-blue-700 text-sm font-medium mb-6">
            <Sparkles className="h-4 w-4" />
            AI駆動のコンセンサス形成ツール
          </div>

          <h1 className="text-5xl md:text-6xl font-bold tracking-tight mb-6 bg-gradient-to-r from-slate-900 to-slate-700 bg-clip-text text-transparent">
            チームの認識を
            <br />
            可視化する
          </h1>

          <p className="text-xl text-slate-600 mb-8 max-w-2xl mx-auto leading-relaxed">
            Cartographerは、AIを活用してチーム内の多様な視点を収集・分析し、
            データに基づいた意思決定とコンセンサス形成を支援します。
          </p>

          <div className="flex flex-col sm:flex-row gap-4 justify-center">
            <Link href="/dashboard">
              <Button size="lg" className="text-base px-8">
                始める
                <ArrowRight className="ml-2 h-4 w-4" />
              </Button>
            </Link>
            <Link href="/timeline">
              <Button size="lg" variant="outline" className="text-base px-8">
                公開議論を見る
              </Button>
            </Link>
          </div>
        </div>
      </section>

      {/* Client Logos */}
      <section className="container mx-auto px-4 py-16">
        <div className="max-w-5xl mx-auto text-center">
          <h2 className="text-2xl md:text-3xl font-semibold text-slate-900">
            Cartographerをご活用いただいているチーム
          </h2>
          <p className="mt-3 text-slate-600">
            多様なステークホルダーが関わる意思決定の現場で、認識のズレを素早く可視化しています。
          </p>
          <div className="mt-10 grid gap-4 sm:grid-cols-3 lg:grid-cols-6">
            {clientLogos.map((client) => (
              <div
                key={client.src}
                className="flex items-center justify-center rounded-xl border border-slate-200 bg-white px-4 py-6 shadow-sm transition hover:-translate-y-1 hover:border-blue-300 hover:shadow-lg"
              >
                <Image
                  src={client.src}
                  alt={client.alt}
                  width={160}
                  height={64}
                  className="h-12 w-auto object-contain"
                />
              </div>
            ))}
          </div>
        </div>
      </section>

      {featuredSessions.length > 0 && (
        <section className="container mx-auto px-4 py-16 bg-white">
          <div className="max-w-5xl mx-auto">
            <div className="mb-10">
              <h2 className="text-3xl md:text-4xl font-bold text-slate-900">
                公開セッションの記録
              </h2>
              <p className="mt-3 text-slate-600">
                最近公開されたセッションの流れを時系列でご紹介します。
              </p>
            </div>
            <div className="relative">
              <div className="absolute left-4 top-0 hidden h-full w-px bg-slate-200 md:block" />
              <div className="space-y-8">
                {featuredSessions.map((session, index) => {
                  const createdAt = new Date(session.createdAt);
                  const formattedDate = createdAt.toLocaleDateString("ja-JP", {
                    year: "numeric",
                    month: "long",
                    day: "numeric",
                  });
                  const truncatedContext =
                    session.context && session.context.length > 110
                      ? `${session.context.slice(0, 110)}…`
                      : session.context;
                  const truncatedGoal =
                    session.goal && session.goal.length > 110
                      ? `${session.goal.slice(0, 110)}…`
                      : session.goal;

                  return (
                    <div
                      key={session.id}
                      className="relative rounded-2xl border border-slate-200 bg-slate-50 px-6 py-6 shadow-sm"
                    >
                      <div className="absolute left-4 top-6 hidden h-3 w-3 -translate-x-1.5 rounded-full border-2 border-white bg-blue-500 md:block" />
                      <div className="flex flex-col gap-3 md:pl-8">
                        <div className="text-sm font-semibold text-blue-600">
                          {formattedDate}
                        </div>
                        <h3 className="text-xl font-semibold text-slate-900">
                          {session.title || `未設定のセッション ${index + 1}`}
                        </h3>

                        {truncatedContext && (
                          <div>
                            <p className="text-xs font-semibold uppercase tracking-wide text-slate-500">
                              背景
                            </p>
                            <p className="text-sm text-slate-600">
                              {truncatedContext}
                            </p>
                          </div>
                        )}

                        {truncatedGoal && (
                          <div>
                            <p className="text-xs font-semibold uppercase tracking-wide text-slate-500">
                              目的
                            </p>
                            <p className="text-sm text-slate-600">
                              {truncatedGoal}
                            </p>
                          </div>
                        )}

                        <div className="flex flex-wrap items-center gap-4 text-sm text-slate-600">
                          <span>{session._count.participants} 人参加</span>
                          <span>{session._count.statements} 質問</span>
                        </div>

                        <div>
                          <Link
                            href={`/sessions/${session.id}`}
                            className="inline-flex items-center text-sm font-semibold text-blue-600 transition hover:text-blue-700"
                          >
                            詳細を見る
                            <ArrowRight className="ml-1 h-4 w-4" />
                          </Link>
                        </div>
                      </div>
                    </div>
                  );
                })}
              </div>
            </div>
            <div className="mt-10 text-right">
              <Link href="/timeline">
                <Button variant="outline" className="w-full md:w-auto">
                  すべての公開セッションを見る
                  <ArrowRight className="ml-2 h-4 w-4" />
                </Button>
              </Link>
            </div>
          </div>
        </section>
      )}

      {/* Features Section */}
      <section className="container mx-auto px-4 py-20 bg-white">
        <div className="max-w-6xl mx-auto">
          <div className="text-center mb-16">
            <h2 className="text-3xl md:text-4xl font-bold mb-4">
              なぜCartographerなのか
            </h2>
            <p className="text-lg text-slate-600">
              複雑な意見の相違を、明確な合意形成へ
            </p>
          </div>

          <div className="grid md:grid-cols-3 gap-8">
            {/* Feature 1 */}
            <div className="p-6 rounded-xl border border-slate-200 hover:border-blue-300 hover:shadow-lg transition-all">
              <div className="h-12 w-12 rounded-lg bg-blue-100 flex items-center justify-center mb-4">
                <Sparkles className="h-6 w-6 text-blue-600" />
              </div>
              <h3 className="text-xl font-semibold mb-2">AI自動サーベイ生成</h3>
              <p className="text-slate-600 leading-relaxed">
                セッションの目的を入力するだけで、Google Geminiが最適な質問を自動生成。手間なく深い洞察を得られます。
              </p>
            </div>

            {/* Feature 2 */}
            <div className="p-6 rounded-xl border border-slate-200 hover:border-blue-300 hover:shadow-lg transition-all">
              <div className="h-12 w-12 rounded-lg bg-green-100 flex items-center justify-center mb-4">
                <Users className="h-6 w-6 text-green-600" />
              </div>
              <h3 className="text-xl font-semibold mb-2">多様な視点を可視化</h3>
              <p className="text-slate-600 leading-relaxed">
                PCA分析で参加者の意見を2Dマップに配置。誰がどこに立っているのか、一目で理解できます。
              </p>
            </div>

            {/* Feature 3 */}
            <div className="p-6 rounded-xl border border-slate-200 hover:border-blue-300 hover:shadow-lg transition-all">
              <div className="h-12 w-12 rounded-lg bg-purple-100 flex items-center justify-center mb-4">
                <BarChart3 className="h-6 w-6 text-purple-600" />
              </div>
              <h3 className="text-xl font-semibold mb-2">詳細な分析レポート</h3>
              <p className="text-slate-600 leading-relaxed">
                AIが回答パターンを分析し、個人レポートとセッションレポートを自動生成。データに基づいた意思決定を実現します。
              </p>
            </div>
          </div>
        </div>
      </section>

      {/* How It Works */}
      <section className="container mx-auto px-4 py-20">
        <div className="max-w-4xl mx-auto">
          <div className="text-center mb-12">
            <h2 className="text-3xl md:text-4xl font-bold mb-4">
              シンプルな3ステップ
            </h2>
          </div>

          <div className="space-y-6">
            <div className="flex gap-4 items-start">
              <div className="flex-shrink-0 h-10 w-10 rounded-full bg-blue-600 text-white flex items-center justify-center font-bold">
                1
              </div>
              <div>
                <h3 className="text-xl font-semibold mb-2">セッションを作成</h3>
                <p className="text-slate-600">
                  議論したいテーマと背景情報を入力すると、AIが自動的に適切な質問を生成します。
                </p>
              </div>
            </div>

            <div className="flex gap-4 items-start">
              <div className="flex-shrink-0 h-10 w-10 rounded-full bg-blue-600 text-white flex items-center justify-center font-bold">
                2
              </div>
              <div>
                <h3 className="text-xl font-semibold mb-2">チームの意見を収集</h3>
                <p className="text-slate-600">
                  参加者はリッカート尺度（5段階）で各質問に回答。リアルタイムで回答率を追跡できます。
                </p>
              </div>
            </div>

            <div className="flex gap-4 items-start">
              <div className="flex-shrink-0 h-10 w-10 rounded-full bg-blue-600 text-white flex items-center justify-center font-bold">
                3
              </div>
              <div>
                <h3 className="text-xl font-semibold mb-2">洞察を得る</h3>
                <p className="text-slate-600">
                  PCA可視化とAI分析レポートで、チームの認識マップを明らかにし、次のアクションを決定します。
                </p>
              </div>
            </div>
          </div>
        </div>
      </section>

      {/* Testimonials */}
      <section className="container mx-auto px-4 py-20 bg-white">
        <div className="max-w-5xl mx-auto">
          <div className="text-center mb-12">
            <h2 className="text-3xl md:text-4xl font-bold mb-4 text-slate-900">
              現場の声
            </h2>
            <p className="text-lg text-slate-600">
              Cartographerが議論の質とスピードをどう変えたのか、実際のチームのコメントをご紹介します。
            </p>
          </div>
          <div className="grid gap-6 md:grid-cols-3">
            {testimonials.map((testimonial) => (
              <div
                key={testimonial.name}
                className="flex h-full flex-col rounded-2xl border border-slate-200 bg-slate-50 p-6 shadow-sm"
              >
                <p className="text-left text-sm leading-relaxed text-slate-700">
                  “{testimonial.quote}”
                </p>
                <div className="mt-6 pt-4 text-left text-sm text-slate-600 border-t border-slate-200">
                  <p className="font-semibold text-slate-800">{testimonial.name}</p>
                  <p>{testimonial.role}</p>
                  <p className="text-slate-500">{testimonial.organization}</p>
                </div>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* Key Benefits */}
      <section className="container mx-auto px-4 py-20 bg-slate-50">
        <div className="max-w-4xl mx-auto">
          <div className="text-center mb-12">
            <h2 className="text-3xl md:text-4xl font-bold mb-4">
              こんな課題を解決します
            </h2>
          </div>

          <div className="grid md:grid-cols-2 gap-6">
            <div className="flex gap-3">
              <CheckCircle2 className="h-6 w-6 text-green-600 flex-shrink-0 mt-0.5" />
              <div>
                <h4 className="font-semibold mb-1">会議が長引く</h4>
                <p className="text-sm text-slate-600">事前に意見を収集し、データに基づいた議論で効率化</p>
              </div>
            </div>

            <div className="flex gap-3">
              <CheckCircle2 className="h-6 w-6 text-green-600 flex-shrink-0 mt-0.5" />
              <div>
                <h4 className="font-semibold mb-1">声の大きい人の意見に偏る</h4>
                <p className="text-sm text-slate-600">全員の意見を平等に可視化し、バランスのとれた決定</p>
              </div>
            </div>

            <div className="flex gap-3">
              <CheckCircle2 className="h-6 w-6 text-green-600 flex-shrink-0 mt-0.5" />
              <div>
                <h4 className="font-semibold mb-1">意見の相違が見えない</h4>
                <p className="text-sm text-slate-600">PCAマップで立場の違いを一目で理解</p>
              </div>
            </div>

            <div className="flex gap-3">
              <CheckCircle2 className="h-6 w-6 text-green-600 flex-shrink-0 mt-0.5" />
              <div>
                <h4 className="font-semibold mb-1">合意形成に時間がかかる</h4>
                <p className="text-sm text-slate-600">AI分析で共通点と相違点を明確にし、議論を加速</p>
              </div>
            </div>
          </div>
        </div>
      </section>

      {/* CTA Section */}
      <section className="container mx-auto px-4 py-20">
        <div className="max-w-4xl mx-auto text-center bg-gradient-to-r from-blue-600 to-blue-700 rounded-2xl p-12 text-white">
          <h2 className="text-3xl md:text-4xl font-bold mb-4">
            今すぐチームの認識をマップしませんか？
          </h2>
          <p className="text-lg mb-8 text-blue-50">
            無料でセッションを作成して、チームの新しい視点を発見しましょう
          </p>
          <Link href="/dashboard">
            <Button size="lg" variant="secondary" className="text-base px-8">
              ダッシュボードを開く
              <ArrowRight className="ml-2 h-4 w-4" />
            </Button>
          </Link>
        </div>
      </section>

      {/* Footer */}
      <footer className="border-t border-slate-200 bg-white">
        <div className="mx-auto flex w-full items-center justify-between px-4 py-4 text-sm font-medium text-slate-600 sm:px-6 lg:px-8">
          <span>合同会社 多元現実</span>
          <AboutCartographerButton />
        </div>
      </footer>
    </div>
  );
}
