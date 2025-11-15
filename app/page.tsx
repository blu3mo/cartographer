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
  { src: "/logos/sample-logo-3.svg", alt: "クライアントロゴサンプル4" },
  { src: "/logos/sample-logo-3.svg", alt: "クライアントロゴサンプル5" },
  { src: "/logos/sample-logo-3.svg", alt: "クライアントロゴサンプル6" },
];

const productShots = [
  {
    src: "/screens/session-setup.svg",
    alt: "セッション設定UIのサンプル画像",
    label: "セッション設定の入力フォーム",
    description:
      "背景情報や目的を数分で整理する設定画面。議事録や資料を読み込ませる流れを視覚化すると、利用開始の手軽さが伝わります。",
  },
  {
    src: "/screens/live-session.svg",
    alt: "参加者回答画面のサンプル画像",
    label: "回答画面とリアルタイム集計",
    description:
      "参加者が5段階で回答し、回答率や論点の偏りがライブで変化する様子を表現することで、会議内での活用イメージを想起させます。",
  },
  {
    src: "/screens/report-insights.svg",
    alt: "分析レポートのサンプル画像",
    label: "AIレポートとPCAマップ",
    description:
      "合意点・相違点・不明点の整理と、PCAマップによる立場の俯瞰を表示。議論後のアクション決定まで導く価値を補強します。",
  },
];

const testimonials = [
  {
    quote:
      "Cartographer のレポートで論点が整理され、参加者全員が次に議論すべきテーマを迷わず共有できました。",
    name: "黒澤 亮",
    role: "プロダクトマネージャー",
    organization: "DMM.com",
    avatar: "/avatars/testimonial-1.svg",
  },
  {
    quote:
      "経営陣の温度感の違いが数分で可視化され、クライアントとの認識合わせが驚くほどスムーズになりました。",
    name: "結城 侑",
    role: "コンサルタント",
    organization: "株式会社BUTAI",
    avatar: "/avatars/testimonial-2.svg",
  },
  {
    quote:
      "住民ワークショップの場で Cartographer を使うことで、筋の良い論点が即座に浮かび上がり、合意形成の糸口が見つかります。",
    name: "前田 紘司",
    role: "ファシリテーター",
    organization: "構想日本",
    avatar: "/avatars/testimonial-3.svg",
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
      <section className="container mx-auto px-4 py-20 md:py-28">
        <div className="mx-auto max-w-6xl">
          <div className="grid items-center gap-12 lg:grid-cols-2">
            <div className="text-center lg:text-left">
              <div className="inline-flex items-center gap-2 rounded-full bg-blue-50 px-4 py-2 text-sm font-medium text-blue-700">
                <Sparkles className="h-4 w-4" />
                AI駆動のコンセンサス形成ツール
              </div>

              <h1 className="mt-6 text-5xl font-bold tracking-tight text-slate-900 md:text-6xl">
                チームの認識を
                <br />
                可視化する
              </h1>

              <p className="mt-6 text-xl leading-relaxed text-slate-600">
                Cartographerは、AIを活用してチーム内の多様な視点を収集・分析し、
                データに基づいた意思決定とコンセンサス形成を支援します。
              </p>

              <div className="mt-8 flex flex-col justify-stater gap-4 sm:flex-row">
                <Link href="/dashboard">
                  <Button size="lg" className="px-8 text-base">
                    始める
                    <ArrowRight className="ml-2 h-4 w-4" />
                  </Button>
                </Link>
                <Link href="/timeline">
                  <Button size="lg" variant="outline" className="px-8 text-base">
                    公開議論を見る
                  </Button>
                </Link>
              </div>
            </div>

            <div className="flex flex-col items-center gap-4">
              <div className="relative w-full max-w-xl overflow-hidden rounded-3xl border border-slate-200 bg-white p-4 shadow-xl">
                <Image
                  src="/screens/hero-dashboard.svg"
                  alt="Cartographerダッシュボードのサンプル"
                  width={960}
                  height={540}
                  className="w-full rounded-2xl border border-dashed border-slate-300 bg-slate-200 object-cover"
                  priority
                />
                <div className="absolute left-6 top-6 rounded-full bg-slate-900/80 px-3 py-1 text-xs font-semibold text-white">
                  ここにダッシュボード全体のスクリーンショット
                </div>
              </div>
              <p className="text-sm text-slate-500">
                セッションの稼働状況や合意ポイントのハイライトを一目で伝えるビジュアルを想定。
              </p>
            </div>
          </div>
        </div>
      </section>

      {/* Client Logos */}
      <section className="container mx-auto px-4 py-16">
        <div className="max-w-5xl mx-auto text-center">
          <p className="text-2xl md:text-3xl font-semibold text-slate-900">
            Cartographerをご活用いただいているチーム
          </p>
          <div className="mt-10 grid gap-4 sm:grid-cols-3 lg:grid-cols-6">
            {clientLogos.map((client) => (
              <div
                key={client.src}
                className="flex items-center justify-center rounded-xl px-4 py-6"
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

      {/* Product Shots */}
      <section className="container mx-auto px-4 py-20">
        <div className="mx-auto max-w-6xl">
          <div className="mb-12 text-center">
            <h2 className="text-3xl md:text-4xl font-bold text-slate-900">
              プロダクトの流れをイメージ
            </h2>
            <p className="mt-3 text-lg text-slate-600">
              実際のUIキャプチャを差し込む位置づけをサンプルで示しています。
            </p>
          </div>
          <div className="grid gap-10 md:grid-cols-2 lg:grid-cols-3">
            {productShots.map((shot) => (
              <figure
                key={shot.src}
                className="flex flex-col gap-4 rounded-3xl border border-slate-200 bg-white p-5 shadow-sm"
              >
                <div className="relative overflow-hidden rounded-2xl border border-dashed border-slate-300 bg-slate-200">
                  <Image
                    src={shot.src}
                    alt={shot.alt}
                    width={720}
                    height={480}
                    className="w-full object-contain"
                  />
                  <div className="absolute left-5 top-5 rounded-full bg-slate-900/80 px-3 py-1 text-xs font-semibold text-white">
                    {shot.label}
                  </div>
                </div>
                <figcaption className="space-y-2 text-left">
                  <p className="text-sm text-slate-500">{shot.description}</p>
                </figcaption>
              </figure>
            ))}
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
          <div className="space-y-10 divide-y divide-slate-200">
            {testimonials.map((testimonial) => (
              <div
                key={testimonial.name}
                className="pt-10 first:pt-0 md:flex md:items-center md:justify-between md:gap-10"
              >
                <div className="flex items-center gap-4 md:gap-6">
                  <Image
                    src={testimonial.avatar}
                    alt={`${testimonial.name}のアイコン`}
                    width={96}
                    height={96}
                    className="h-16 w-16 rounded-full border border-slate-300 bg-slate-200 object-cover md:h-20 md:w-20"
                  />
                  <div>
                    <p className="text-lg font-semibold text-slate-900">
                      {testimonial.name}
                    </p>
                    <p className="text-sm text-slate-500">{testimonial.role}</p>
                    <p className="text-sm text-slate-400">
                      {testimonial.organization}
                    </p>
                  </div>
                </div>
                <p className="mt-4 text-left text-lg font-medium text-slate-700 md:mt-0 md:w-1/2 md:text-right md:text-xl md:leading-relaxed">
                  “{testimonial.quote}”
                </p>
              </div>
            ))}
          </div>
        </div>
      </section>

      {featuredSessions.length > 0 && (
        <section className="container mx-auto px-4 py-16 bg-white">
          <div className="mx-auto max-w-6xl">
            <div className="mb-10 text-center lg:text-left">
              <h2 className="text-3xl md:text-4xl font-bold text-slate-900">
                公開セッションの記録
              </h2>
              <p className="mt-3 text-slate-600">
                最近公開されたセッションの流れを時系列で示し、詳細ページに誘導します。
              </p>
            </div>
            <div className="grid gap-10 lg:grid-cols-[1.6fr,1fr] lg:items-start">
              <div className="relative rounded-3xl border border-slate-200 bg-slate-50 p-6 shadow-sm">
                <div className="absolute left-8 top-0 hidden h-full w-px -translate-x-1/2 bg-slate-300 lg:block" />
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
                        className="relative rounded-2xl border border-slate-200 bg-white px-6 py-6 shadow-sm lg:pl-12"
                      >
                        <div className="absolute left-6 top-8 hidden h-3 w-3 -translate-x-1/2 rounded-full border-2 border-white bg-blue-500 lg:block" />
                        <div className="flex flex-col gap-3">
                          {/* <div className="text-sm font-semibold text-blue-600">
                            {formattedDate}
                          </div> */}
                          <h3 className="text-xl font-semibold text-slate-900">
                            {session.title || `未設定のセッション ${index + 1}`}
                          </h3>

                          {/* {truncatedContext && (
                            <div>
                              <p className="text-xs font-semibold uppercase tracking-wide text-slate-500">
                                背景
                              </p>
                              <p className="text-sm text-slate-600">
                                {truncatedContext}
                              </p>
                            </div>
                          )} */}

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
                            {/* <span>{session._count.statements} 質問</span> */}
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
            </div>


            <div className="flex flex-col sm:flex-row gap-4 justify-center">
              <Link href="/timeline">
                <Button size="lg" className="text-base px-8">
                  すべての公開セッションを見る
                  <ArrowRight className="ml-2 h-4 w-4" />
                </Button>
              </Link>
            </div>

          </div>
        </section>
      )}


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
