"use client";

import axios from "axios";
import {
  Calendar,
  ExternalLink,
  FileText,
  Loader2,
  Lock,
  Plus,
  Users,
} from "lucide-react";
import Link from "next/link";
import { useRouter } from "next/navigation";
import { useEffect, useMemo, useState } from "react";

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
        {/* Hero Section */}
        <div className="mb-12">
          <h1 className="text-4xl font-bold tracking-tight mb-3">
            Cartographer
          </h1>
          <p className="text-lg text-muted-foreground">
            認識を可視化し、合意形成を促進するワークショップツール
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
    </div>
  );
}

type SessionSectionsProps = {
  sessions: Session[];
};

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
