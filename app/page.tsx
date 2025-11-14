import type { Metadata } from "next";
import Link from "next/link";
import { Button } from "@/components/ui/Button";
import { Users, BarChart3, Sparkles, ArrowRight, CheckCircle2 } from "lucide-react";
import { AppHeader } from "./components/AppHeader";
import { MarketingNav } from "./components/MarketingNav";
import { AboutCartographerButton } from "./components/AboutCartographerButton";

const appUrl = process.env.NEXT_PUBLIC_APP_URL ?? "http://localhost:3000";
const metadataBase = new URL(appUrl);
const homeTitle = "Cartographer | チームの認識をマップする";
const homeDescription =
  "AIを活用してチーム内の多様な視点を収集・分析し、データに基づいた意思決定とコンセンサス形成を支援します。";

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

export default function LandingPage() {
  return (
    <div className="min-h-screen bg-gradient-to-b from-slate-50 to-white">
      <AppHeader rightSlot={<AboutCartographerButton />}>
        <MarketingNav />
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
                公開タイムラインを見る
              </Button>
            </Link>
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
