"use client";

import axios from "axios";
import {
  Calendar,
  ChevronLeft,
  ChevronRight,
  ExternalLink,
  FileText,
  Loader2,
  Lock,
  Plus,
  Users,
  X,
} from "lucide-react";
import Link from "next/link";
import { useRouter } from "next/navigation";
import { useEffect, useMemo, useState } from "react";

import { AppHeader } from "@/components/AppHeader";
import { Button, buttonVariants } from "@/components/ui/Button";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { createAuthorizationHeader } from "@/lib/auth";
import { useUserId } from "@/lib/useUserId";

type Session = {
  id: string;
  title: string;
  context: string;
  goal: string;
  hostUserId: string;
  adminAccessToken?: string;
  createdAt: string;
  isPublic: boolean;
  _count: {
    participants: number;
    statements: number;
  };
  isHost: boolean;
  isParticipant: boolean;
};

type AboutSlideContent = {
  title: string;
  description: string;
  details?: string[];
  bullets?: string[];
};

const ABOUT_SLIDES: AboutSlideContent[] = [
  {
    title: "Cartographerとは",
    description:
      "Cartographerとは、「それぞれの認識を洗い出し、合意点、相違点、不明点を可視化するツール」です。",
    details: [
      "合意形成や意思決定の現場で、認識ギャップを素早く把握するためのファシリテーション支援を目的に設計されています。",
    ],
  },
  {
    title: "どんな時に活用できるか",
    description: "以下のような場面で力を発揮します。",
    bullets: [
      "チーム内で目標や成果物イメージがズレており、素早く認識を合わせたいとき",
      "行政・企業・コミュニティなど多様な価値観が混在する場面で、合意できるポイントを見つけたいとき",
      "「誰も答えられない重要な問い」を掘り起こし、次に検証すべき仮説を定めたいワークショップ／ヒアリング",
    ],
  },
  {
    title: "実際のユースケース",
    description: "現場では次のように活用されています。",
    bullets: [
      "千人規模の企業や自治体で、部門間のゴールのズレを洗い出すセッション",
      "市民・コミュニティヒアリングで「個人の欲求」と「共同体としての方針」を切り分けて整理する場面",
      "クライアントと自社で成果物イメージが食い違うときに、期待値の差分を可視化して再調整するケース",
    ],
  },
  {
    title: "利用方法（実務的な流れ・ポイント）",
    description:
      "Human-in-the-loop で問いの質を高めつつ、次のステップで運用するのが推奨です。",
    bullets: [
      "セッションの目的と洗い出したい認識を定義し、タイトル／ゴール／背景コンテキストを準備する",
      "発言を収集（録音・手入力）→ LLM と人のハイブリッドでツリーやグラフィックに構造化する",
      "可視化した論点を見ながら軌道修正し、外部ナレッジやエージェントを差し込んでリアルタイムに検証する",
      "重要な問い・合意点・検証すべき仮説をレポート化し、次回以降のアクションと問いの精度向上につなげる",
    ],
  },
] as const;

export default function HomePageClient() {
  const { userId, isLoading: userLoading } = useUserId();
  const [sessions, setSessions] = useState<Session[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [isAboutOpen, setIsAboutOpen] = useState(false);
  const [aboutPageIndex, setAboutPageIndex] = useState(0);
  const currentSlide = ABOUT_SLIDES[aboutPageIndex];
  const openAboutModal = () => {
    setAboutPageIndex(0);
    setIsAboutOpen(true);
  };
  const closeAboutModal = () => {
    setIsAboutOpen(false);
  };

  useEffect(() => {
    if (!userId || userLoading) return;

    const fetchSessions = async () => {
      try {
        setLoading(true);
        const response = await axios.get("/api/sessions", {
          headers: createAuthorizationHeader(userId),
        });
        setSessions(response.data.sessions);
      } catch (err) {
        console.error("Failed to fetch sessions:", err);
        setError("セッションの取得に失敗しました。");
      } finally {
        setLoading(false);
      }
    };

    fetchSessions();
  }, [userId, userLoading]);

  if (userLoading || loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-background flex flex-col">
      <AppHeader
        rightSlot={
          <button
            type="button"
            onClick={openAboutModal}
            className="text-sm font-medium text-slate-600 transition-colors hover:text-slate-900"
          >
            cartographerについて
          </button>
        }
      />
      {isAboutOpen && (
        <div
          className="fixed inset-0 z-50 flex items-center justify-center bg-slate-950/50 px-4 py-8"
          onClick={closeAboutModal}
        >
          <div
            className="max-w-2xl w-full rounded-3xl border border-slate-200 bg-white p-6 shadow-2xl"
            role="dialog"
            aria-modal="true"
            onClick={(event) => event.stopPropagation()}
          >
            <div className="flex items-center justify-between gap-4">
              <div>
                {/* <p className="text-xs uppercase tracking-[0.3em] text-slate-400">
                  CARTOGRAPHER
                </p> */}
                <h2 className="text-lg font-semibold text-slate-900">
                  {currentSlide.title}
                </h2>
              </div>
              <button
                type="button"
                onClick={closeAboutModal}
                aria-label="閉じる"
                className="rounded-full border border-slate-200 p-2 text-slate-500 transition-colors hover:border-slate-300 hover:text-slate-900"
              >
                <X className="h-4 w-4" />
              </button>
            </div>
            <AboutSlide slide={currentSlide} />
            <div className="mt-6 flex items-center justify-between text-sm">
              <button
                type="button"
                onClick={() =>
                  setAboutPageIndex((prev) => Math.max(0, prev - 1))
                }
                disabled={aboutPageIndex === 0}
                className="flex items-center gap-2 rounded-full border border-slate-200 px-4 py-2 font-medium text-slate-600 transition-colors hover:text-slate-900 disabled:cursor-not-allowed disabled:opacity-50"
              >
                <ChevronLeft className="h-4 w-4" />
                前へ
              </button>
              <div className="text-xs font-medium text-slate-500">
                {aboutPageIndex + 1} / {ABOUT_SLIDES.length}
              </div>
              <button
                type="button"
                onClick={() =>
                  setAboutPageIndex((prev) =>
                    Math.min(ABOUT_SLIDES.length - 1, prev + 1),
                  )
                }
                disabled={aboutPageIndex === ABOUT_SLIDES.length - 1}
                className="flex items-center gap-2 rounded-full border border-slate-900 bg-slate-900 px-4 py-2 font-medium text-white transition-colors hover:bg-slate-800 disabled:cursor-not-allowed disabled:opacity-60"
              >
                次へ
                <ChevronRight className="h-4 w-4" />
              </button>
            </div>
          </div>
        </div>
      )}
      <main className="flex-1">
        <div className="max-w-4xl mx-auto px-4 py-12 sm:px-6 lg:px-8">
          {/* Header with CTA */}
          <div className="flex items-center justify-between mb-6">
            <h2 className="text-2xl font-semibold tracking-tight">
              セッション
            </h2>
            <Link href="/sessions/new">
              <Button>
                <Plus className="h-4 w-4" />
                新しいセッションを作成
              </Button>
            </Link>
          </div>

          {error && (
            <Card className="mb-6 border-destructive">
              <CardContent className="pt-6">
                <p className="text-sm text-destructive">{error}</p>
              </CardContent>
            </Card>
          )}

          {/* Sessions List */}
          {sessions.length === 0 ? (
            <Card>
              <CardContent className="pt-6 pb-6 text-center">
                <div className="flex flex-col items-center gap-3 py-12">
                  <div className="h-16 w-16 rounded-full bg-primary/10 flex items-center justify-center mb-2">
                    <FileText className="h-8 w-8 text-primary" />
                  </div>
                  <div className="space-y-2">
                    <p className="text-base font-semibold">
                      セッションがありません
                    </p>
                    <p className="text-sm text-muted-foreground max-w-sm">
                      新しいセッションを作成して、チームとの対話を始めましょう
                    </p>
                  </div>
                  <Link href="/sessions/new" className="mt-2">
                    <Button>
                      <Plus className="h-4 w-4" />
                      最初のセッションを作成
                    </Button>
                  </Link>
                </div>
              </CardContent>
            </Card>
          ) : (
            <SessionSections sessions={sessions} />
          )}
        </div>
      </main>
      <footer className="border-t border-slate-200 bg-white">
        <div className="mx-auto flex w-full px-4 py-4 text-sm font-medium text-slate-600 sm:px-6 lg:px-8">
          <span>合同会社 多元現実</span>
        </div>
      </footer>
    </div>
  );
}

type SessionSectionsProps = {
  sessions: Session[];
};

type AboutSlideProps = {
  slide: AboutSlideContent;
};

function AboutSlide({ slide }: AboutSlideProps) {
  return (
    <div className="mt-6 space-y-4 rounded-2xl bg-slate-50/70 px-5 py-4 text-sm leading-relaxed text-slate-700">
      <div>
        {/* <p className="text-[11px] font-semibold uppercase tracking-[0.3em] text-slate-400">
          Slide Topic
        </p> */}
        {/* <h3 className="mt-1 text-base font-semibold text-slate-900">
          {slide.title}
        </h3> */}
        <p className="mt-2 text-slate-600">{slide.description}</p>
      </div>
      {slide.details?.map((detail) => (
        <p key={detail} className="text-slate-600">
          {detail}
        </p>
      ))}
      {slide.bullets && (
        <ul className="list-disc space-y-2 pl-6 text-slate-700">
          {slide.bullets.map((bullet) => (
            <li key={bullet}>{bullet}</li>
          ))}
        </ul>
      )}
    </div>
  );
}

function SessionSections({ sessions }: SessionSectionsProps) {
  const router = useRouter();
  const sessionCategories = useMemo(() => {
    const adminSessions = sessions.filter((session) => session.isHost);
    const participatingSessions = sessions.filter(
      (session) => !session.isHost && session.isParticipant,
    );
    const otherSessions = sessions.filter(
      (session) =>
        !session.isHost && !session.isParticipant && session.isPublic,
    );

    return [
      {
        title: "管理中のセッション",
        sessions: adminSessions,
      },
      {
        title: "参加中のセッション",
        sessions: participatingSessions,
      },
      {
        title: "未参加の公開セッション",
        sessions: otherSessions,
      },
    ].filter((category) => category.sessions.length > 0);
  }, [sessions]);

  return (
    <div className="space-y-6">
      {sessionCategories.map((category) => (
        <div key={category.title} className="space-y-3">
          <h3 className="text-lg font-semibold text-muted-foreground">
            {category.title}
          </h3>
          <div className="space-y-3">
            {category.sessions.map((session) => (
              <Card
                key={session.id}
                className="hover:shadow-md transition-shadow"
              >
                <CardHeader className="pb-3">
                  <div className="flex items-start justify-between gap-4">
                    <CardTitle className="text-lg font-semibold">
                      {session.title}
                    </CardTitle>
                    {!session.isPublic && (
                      <span className="inline-flex items-center gap-1.5 rounded-full bg-muted px-2.5 py-1 text-xs font-medium text-muted-foreground">
                        <Lock className="h-3.5 w-3.5" />
                        非公開
                      </span>
                    )}
                  </div>
                  <CardDescription className="text-sm text-muted-foreground">
                    {session.goal || session.context}
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="flex items-center justify-between">
                    <div className="flex flex-wrap items-center gap-6 text-sm text-muted-foreground">
                      <div className="flex items-center gap-2">
                        <Users className="h-4 w-4" />
                        <div className="flex flex-col leading-tight">
                          <span className="text-xs">参加者</span>
                          <span className="text-sm font-semibold text-foreground">
                            {session._count.participants}
                          </span>
                        </div>
                      </div>
                      <div className="flex items-center gap-2">
                        <FileText className="h-4 w-4" />
                        <div className="flex flex-col leading-tight">
                          <span className="text-xs">生成された質問数</span>
                          <span className="text-sm font-semibold text-foreground">
                            {session._count.statements}
                          </span>
                        </div>
                      </div>
                      <div className="flex items-center gap-2">
                        <Calendar className="h-4 w-4" />
                        <div className="flex flex-col leading-tight">
                          <span className="text-xs">セッション作成日</span>
                          <span className="text-sm font-semibold text-foreground">
                            {new Date(session.createdAt).toLocaleDateString(
                              "ja-JP",
                            )}
                          </span>
                        </div>
                      </div>
                    </div>
                    <div className="flex gap-2">
                      {session.isHost && (
                        <Button
                          variant="secondary"
                          size="sm"
                          onClick={(event) => {
                            event.stopPropagation();
                            if (!session.adminAccessToken) return;
                            router.push(
                              `/sessions/${session.id}/${session.adminAccessToken}`,
                            );
                          }}
                        >
                          管理
                        </Button>
                      )}
                      <Link
                        href={`/sessions/${session.id}`}
                        target="_blank"
                        rel="noreferrer"
                        className={buttonVariants({ size: "sm" })}
                      >
                        <span>参加</span>
                        <ExternalLink className="h-3.5 w-3.5" />
                      </Link>
                    </div>
                  </div>
                </CardContent>
              </Card>
            ))}
          </div>
        </div>
      ))}
    </div>
  );
}
