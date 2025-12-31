"use client";

import axios from "axios";
import {
  Calendar,
  FileText,
  Loader2,
  Lock,
  Plus,
  SquareArrowOutUpRight,
  Users,
} from "lucide-react";
import Link from "next/link";
import { useRouter } from "next/navigation";
import { useEffect, useMemo, useState } from "react";

import { Button } from "@/components/ui/Button";
import { Badge } from "@/components/ui/badge";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import {
  Empty,
  EmptyContent,
  EmptyDescription,
  EmptyHeader,
  EmptyMedia,
  EmptyTitle,
} from "@/components/ui/empty";
import { ThemeToggle } from "@/components/ThemeToggle";
import { createAuthorizationHeader } from "@/lib/auth";
import { useUserId } from "@/lib/useUserId";

type Session = {
  id: string;
  title: string;
  context: string;
  goal: string;
  hostUserId: string;
  adminAccessToken: string;
  createdAt: string;
  isPublic: boolean;
  _count: {
    participants: number;
    statements: number;
  };
  isHost: boolean;
  isParticipant: boolean;
};

export default function Home() {
  const { userId, isLoading: userLoading } = useUserId();
  const [sessions, setSessions] = useState<Session[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

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
    <div className="min-h-screen bg-background">
      <div className="max-w-4xl mx-auto px-4 py-12 sm:px-6 lg:px-8">
        {sessions.length === 0 ? (
          <>
            {/* Hero Section - Minimal for Empty State */}
            <div className="mb-12">
              <div className="flex items-start justify-between gap-4 mb-3">
                <h1 className="text-4xl font-bold tracking-tight">倍速会議</h1>
                <ThemeToggle />
              </div>
              <p className="text-lg text-muted-foreground">
                認識を可視化し、合意形成を促進するツール
              </p>
            </div>

            {/* Empty State */}
            <div className="flex items-center justify-center min-h-[500px]">
              <Empty>
                <EmptyHeader>
                  <EmptyMedia variant="icon">
                    <FileText />
                  </EmptyMedia>
                  <EmptyTitle>まだセッションはありません</EmptyTitle>
                  <EmptyDescription>
                    まだセッションを作成していません。まずは最初のセッションを作成しましょう。
                  </EmptyDescription>
                </EmptyHeader>
                <EmptyContent>
                  <div className="flex gap-2">
                    <Link href="/sessions/new">
                      <Button>
                        <Plus className="h-4 w-4" />
                        セッションを作成
                      </Button>
                    </Link>
                  </div>
                </EmptyContent>
                <a
                  href="https://scrapbox.io/baisoku-kaigi/"
                  target="_blank"
                  rel="noopener noreferrer"
                  className="inline-flex items-center gap-1 text-sm text-muted-foreground hover:text-foreground transition-colors"
                >
                  もっと詳しく知る
                  <SquareArrowOutUpRight className="h-3.5 w-3.5" />
                </a>
              </Empty>
            </div>
          </>
        ) : (
          <>
            {/* Hero Section */}
            <div className="mb-12">
              <div className="flex items-start justify-between gap-4 mb-3">
                <h1 className="text-4xl font-bold tracking-tight">倍速会議</h1>
                <ThemeToggle />
              </div>
              <p className="text-lg text-muted-foreground">
                認識を可視化し、合意形成を促進するツール
              </p>
            </div>

            {/* Header with CTA */}
            <div className="flex items-center justify-between mb-6">
              <h2 className="text-2xl font-semibold tracking-tight">セッション</h2>
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
            <SessionSections sessions={sessions} />
          </>
        )}
      </div>
    </div>
  );
}

type SessionSectionsProps = {
  sessions: Session[];
};

function SessionSections({ sessions }: SessionSectionsProps) {
  const router = useRouter();
  const stripSupplementalInfo = (text: string) => {
    if (!text) return "";
    const lines = text.split(/\r?\n/);
    const cutoffIndex = lines.findIndex((line) => line.includes("補足情報"));
    const keptLines = cutoffIndex === -1 ? lines : lines.slice(0, cutoffIndex);
    return keptLines.join("\n").trim();
  };
  const getContextPreview = (text: string) => {
    const stripped = stripSupplementalInfo(text);
    if (!stripped) return "";
    const maxLength = 240;
    if (stripped.length <= maxLength) return stripped;
    return `${stripped.slice(0, maxLength)}…`;
  };
  const sessionCategories = useMemo(() => {
    const adminSessions = sessions.filter((session) => session.isHost);
    const participatingSessions = sessions.filter(
      (session) => !session.isHost && session.isParticipant,
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
                      <Badge variant="secondary" className="gap-1.5">
                        <Lock className="h-3.5 w-3.5" />
                        非公開
                      </Badge>
                    )}
                  </div>
                  <CardDescription className="space-y-2 text-base leading-6">
                    {session.goal && (
                      <div className="space-y-1">
                        <p className="text-sm font-semibold text-muted-foreground">
                          セッション概要
                        </p>
                        <p className="whitespace-pre-line">{session.goal}</p>
                      </div>
                    )}
                    {session.context && (
                      <div className="space-y-1">
                        <p className="text-sm font-semibold text-muted-foreground">
                          補足情報
                        </p>
                        <p className="whitespace-pre-line">
                          {getContextPreview(session.context)}
                        </p>
                      </div>
                    )}
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="flex items-center justify-between">
                    <div className="flex items-center gap-4 text-sm text-muted-foreground">
                      <div className="flex items-center gap-1.5">
                        <Users className="h-4 w-4" />
                        <span>{session._count.participants}人参加</span>
                      </div>
                      <div className="flex items-center gap-1.5">
                        <FileText className="h-4 w-4" />
                        <span>{session._count.statements}質問</span>
                      </div>
                      <div className="flex items-center gap-1.5">
                        <Calendar className="h-4 w-4" />
                        <span>
                          {new Date(session.createdAt).toLocaleDateString(
                            "ja-JP",
                          )}
                          作成
                        </span>
                      </div>
                    </div>
                    <div className="flex gap-2 items-center">
                      {session.isHost && (
                        <Button
                          variant="outline"
                          size="sm"
                          className="h-8"
                          onClick={(event) => {
                            event.stopPropagation();
                            router.push(
                              `/sessions/${session.id}/${session.adminAccessToken}`,
                            );
                          }}
                        >
                          管理
                        </Button>
                      )}
                      <Button
                        size="sm"
                        className="h-8"
                        onClick={(event) => {
                          event.stopPropagation();
                          window.open(
                            `/sessions/${session.id}`,
                            "_blank",
                            "noopener,noreferrer",
                          );
                        }}
                      >
                        参加
                        <SquareArrowOutUpRight className="h-3.5 w-3.5" />
                      </Button>
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
