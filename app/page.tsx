'use client';

import Link from "next/link";
import { useEffect, useState } from "react";
import { useUserId } from "@/lib/useUserId";
import { createAuthorizationHeader } from "@/lib/auth";
import axios from "axios";
import { Button } from "@/components/ui/Button";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Plus, Users, FileText, Calendar, Loader2 } from "lucide-react";

type Session = {
  id: string;
  title: string;
  context: string;
  hostUserId: string;
  createdAt: string;
  _count: {
    participants: number;
    statements: number;
  };
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
        const response = await axios.get('/api/sessions', {
          headers: createAuthorizationHeader(userId),
        });
        setSessions(response.data.sessions);
      } catch (err) {
        console.error('Failed to fetch sessions:', err);
        setError('セッションの取得に失敗しました。');
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
          <h2 className="text-2xl font-semibold tracking-tight">
            セッション
          </h2>
          <Link href="/sessions/new">
            <Button>
              <Plus className="h-4 w-4" />
              新しいセッション
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
                  <p className="text-base font-semibold">セッションがありません</p>
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
          <div className="space-y-3">
            {sessions.map((session) => (
              <Card
                key={session.id}
                className="hover:shadow-md transition-shadow cursor-pointer"
              >
                <CardHeader className="pb-3">
                  <CardTitle className="text-lg font-semibold">
                    {session.title}
                  </CardTitle>
                  <CardDescription className="line-clamp-2">
                    {session.context}
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="flex items-center justify-between">
                    <div className="flex items-center gap-4 text-sm text-muted-foreground">
                      <div className="flex items-center gap-1.5">
                        <Users className="h-4 w-4" />
                        <span>{session._count.participants}</span>
                      </div>
                      <div className="flex items-center gap-1.5">
                        <FileText className="h-4 w-4" />
                        <span>{session._count.statements}</span>
                      </div>
                      <div className="flex items-center gap-1.5">
                        <Calendar className="h-4 w-4" />
                        <span>{new Date(session.createdAt).toLocaleDateString('ja-JP')}</span>
                      </div>
                    </div>
                    <div className="flex gap-2">
                      {session.hostUserId === userId && (
                        <Link href={`/sessions/${session.id}/admin`}>
                          <Button variant="secondary" size="sm">
                            管理
                          </Button>
                        </Link>
                      )}
                      <Link href={`/sessions/${session.id}`}>
                        <Button size="sm">
                          参加
                        </Button>
                      </Link>
                    </div>
                  </div>
                </CardContent>
              </Card>
            ))}
          </div>
        )}
      </div>
    </div>
  );
}
