"use client";

import axios from "axios";
import { FileText, Loader2, Plus } from "lucide-react";
import Link from "next/link";
import { usePathname, useRouter, useSearchParams } from "next/navigation";
import { useCallback, useEffect, useMemo, useState } from "react";

import { DashboardLayout } from "@/components/layout/DashboardLayout";
import { Button } from "@/components/ui/Button";
import { Card, CardContent } from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import {
  Sidebar,
  SidebarContent,
  SidebarGroup,
  SidebarGroupContent,
  SidebarGroupLabel,
  SidebarHeader,
  SidebarTrigger,
} from "@/components/ui/sidebar";
import { createAuthorizationHeader } from "@/lib/auth";
import { useUserId } from "@/lib/useUserId";
import { cn } from "@/lib/utils";
import { SessionAdminDashboard } from "@/sessions/_components/SessionAdminDashboard";

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

export default function HomePageClient() {
  const router = useRouter();
  const pathname = usePathname();
  const searchParams = useSearchParams();
  const { userId, isLoading: userLoading } = useUserId();
  const [sessions, setSessions] = useState<Session[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [searchTerm, setSearchTerm] = useState("");
  const [selectedSessionId, setSelectedSessionId] = useState<string | null>(
    () => searchParams.get("sessionId"),
  );
  const syncSelectedSessionQuery = useCallback(
    (sessionId: string | null) => {
      const currentParam = searchParams.get("sessionId");
      if (currentParam === sessionId || (!currentParam && !sessionId)) {
        return;
      }
      const params = new URLSearchParams(searchParams.toString());
      if (sessionId) {
        params.set("sessionId", sessionId);
      } else {
        params.delete("sessionId");
      }
      const query = params.toString();
      router.replace(query ? `${pathname}?${query}` : pathname);
    },
    [pathname, router, searchParams],
  );

  useEffect(() => {
    const paramSessionId = searchParams.get("sessionId");
    setSelectedSessionId((current) => {
      if (current === paramSessionId) {
        return current;
      }
      return paramSessionId;
    });
  }, [searchParams]);

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

  const filteredSessions = useMemo(() => {
    if (!searchTerm) return sessions;
    const normalized = searchTerm.trim().toLowerCase();
    return sessions.filter((session) => {
      const title = session.title?.toLowerCase() ?? "";
      const goal = session.goal?.toLowerCase() ?? "";
      const context = session.context?.toLowerCase() ?? "";
      return (
        title.includes(normalized) ||
        goal.includes(normalized) ||
        context.includes(normalized)
      );
    });
  }, [sessions, searchTerm]);

  const adminSessions = useMemo(
    () => filteredSessions.filter((session) => session.isHost),
    [filteredSessions],
  );
  const participantSessions = useMemo(
    () =>
      filteredSessions.filter(
        (session) => !session.isHost && session.isParticipant,
      ),
    [filteredSessions],
  );
  const discoverSessions = useMemo(
    () =>
      filteredSessions.filter(
        (session) => !session.isHost && !session.isParticipant,
      ),
    [filteredSessions],
  );
  const handleSessionSelect = useCallback(
    (sessionId: string) => {
      setSelectedSessionId(sessionId);
      syncSelectedSessionQuery(sessionId);
    },
    [syncSelectedSessionQuery],
  );

  useEffect(() => {
    if (adminSessions.length === 0) {
      if (selectedSessionId !== null) {
        setSelectedSessionId(null);
        syncSelectedSessionQuery(null);
      }
      return;
    }

    if (!selectedSessionId) {
      const fallback = adminSessions[0].id;
      setSelectedSessionId(fallback);
      syncSelectedSessionQuery(fallback);
      return;
    }

    const exists = adminSessions.some(
      (session) => session.id === selectedSessionId,
    );
    if (!exists) {
      const fallback = adminSessions[0].id;
      setSelectedSessionId(fallback);
      syncSelectedSessionQuery(fallback);
    }
  }, [adminSessions, selectedSessionId, syncSelectedSessionQuery]);

  const selectedAdminSession =
    adminSessions.find((session) => session.id === selectedSessionId) ?? null;
  const selectedAdminAccessToken =
    selectedAdminSession?.adminAccessToken ?? null;

  if (userLoading || loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
      </div>
    );
  }

  const sidebar = (
    <Sidebar>
      <SidebarHeader className="space-y-4">
        <div className="flex items-center justify-between">
          <div>
            <p className="text-[11px] font-semibold uppercase tracking-wider text-[var(--sidebar-foreground)]/70">
              現在のセッション
            </p>
            <p className="text-lg font-semibold text-[var(--sidebar-foreground)]">
              {sessions.length} 件
            </p>
          </div>
          <SidebarTrigger />
        </div>
        <div className="flex items-center gap-2">
          <Input
            type="search"
            placeholder="セッションを検索"
            value={searchTerm}
            onChange={(event) => setSearchTerm(event.target.value)}
            className="h-9 flex-1 rounded-full border-[var(--sidebar-border)] bg-white/80 px-4 text-sm focus-visible:ring-[var(--sidebar-ring)]"
          />
          <Button
            asChild
            size="icon"
            className="h-9 w-9 rounded-full bg-slate-900 text-white hover:bg-slate-900/90"
            aria-label="新しいセッションを作成"
          >
            <Link href="/sessions/new">
              <Plus className="h-4 w-4" />
            </Link>
          </Button>
        </div>
      </SidebarHeader>
      <SidebarContent className="space-y-4">
        <SidebarSessionsSection
          title="管理中"
          description="あなたが管理できるセッションです。"
          sessions={adminSessions}
          selectedSessionId={selectedSessionId}
          onSelectSession={handleSessionSelect}
          mode="select"
        />
        <SidebarSessionsSection
          title="参加中"
          description="参加者として登録されているセッションです。"
          sessions={participantSessions}
          onNavigate={(id) => router.push(`/sessions/${id}`)}
          mode="link"
        />
        <SidebarSessionsSection
          title="公開セッション"
          description="まだ参加していない公開セッションです。"
          sessions={discoverSessions}
          onNavigate={(id) => router.push(`/sessions/${id}`)}
          mode="link"
        />
      </SidebarContent>
    </Sidebar>
  );

  return (
    <DashboardLayout sidebar={sidebar} headerContent={null} showFooter={false}>
      <div className="flex w-full flex-col gap-6 px-4 py-6 sm:px-6 lg:px-8">
        {error && (
          <Card className="border-destructive">
            <CardContent className="pt-6">
              <p className="text-sm text-destructive">{error}</p>
            </CardContent>
          </Card>
        )}

        {selectedAdminSession ? (
          selectedAdminAccessToken ? (
            <div className="space-y-6">
              <div className="rounded-3xl border border-slate-200 bg-white px-6 py-5 shadow-sm">
                <p className="text-xs font-semibold uppercase tracking-wide text-slate-500">
                  選択中のセッション
                </p>
                <div className="mt-1 space-y-1.5">
                  <h1 className="text-2xl font-bold text-slate-900">
                    {selectedAdminSession.title || "名称未設定"}
                  </h1>
                  <p className="text-sm text-slate-500">
                    {selectedAdminSession.goal ||
                      selectedAdminSession.context ||
                      "詳細情報は未設定です。"}
                  </p>
                </div>
              </div>
              <SessionAdminDashboard
                key={selectedAdminSession.id}
                sessionId={selectedAdminSession.id}
                accessToken={selectedAdminAccessToken}
                embedded
              />
            </div>
          ) : (
            <div className="flex min-h-[40vh] flex-col items-center justify-center rounded-3xl border border-dashed border-slate-200 bg-slate-50 text-sm text-slate-500">
              <p className="font-semibold text-slate-600">
                管理トークンを確認できません
              </p>
              <p className="text-xs text-slate-500">
                管理者権限を発行してから再度お試しください。
              </p>
            </div>
          )
        ) : sessions.length === 0 ? (
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
        ) : adminSessions.length > 0 ? (
          <div className="flex min-h-[40vh] flex-col items-center justify-center rounded-3xl border border-dashed border-slate-200 bg-slate-50 text-center text-sm text-slate-500">
            <p className="font-semibold text-slate-600">
              サイドバーからセッションを選択してください
            </p>
            <p className="text-xs text-slate-500">
              「管理」ボタンを押すと、ここにセッションの管理ビューが表示されます。
            </p>
          </div>
        ) : (
          <div className="flex min-h-[60vh] flex-col items-center justify-center rounded-3xl border border-dashed border-slate-200 bg-slate-50 text-sm text-slate-500">
            <p className="font-semibold text-slate-600">
              管理可能なセッションが見つかりません
            </p>
            <p className="text-xs text-slate-500">
              新しいセッションを作成するか、サイドバーから管理セッションを追加してください。
            </p>
          </div>
        )}
      </div>
    </DashboardLayout>
  );
}

type SidebarSessionsSectionProps = {
  title: string;
  description: string;
  sessions: Session[];
  selectedSessionId?: string | null;
  onSelectSession?: (sessionId: string) => void;
  onNavigate?: (sessionId: string) => void;
  mode: "select" | "link";
};

function SidebarSessionsSection({
  title,
  description,
  sessions,
  selectedSessionId,
  onSelectSession,
  onNavigate,
  mode,
}: SidebarSessionsSectionProps) {
  const router = useRouter();
  const handleParticipate = (sessionId: string) => {
    router.push(`/sessions/${sessionId}`);
  };

  const handleManage = (session: Session) => {
    // 遷移せず、右側のパネルに直接描画するため選択だけ行う
    onSelectSession?.(session.id);
  };

  return (
    <SidebarGroup>
      <SidebarGroupLabel>
        <span>{title}</span>
        <span className="text-[10px] font-semibold text-[var(--sidebar-foreground)]/60">
          {sessions.length}件
        </span>
      </SidebarGroupLabel>
      <SidebarGroupContent>
        {sessions.length === 0 ? (
          <p className="rounded-xl bg-white/70 px-3 py-3 text-[11px] text-[var(--sidebar-foreground)]/70">
            {description}
          </p>
        ) : (
          <div className="space-y-3">
            {sessions.map((session) => {
              const isActive = selectedSessionId === session.id;
              const context =
                session.context?.trim() || "まだ入力されていません";
              const goal = session.goal?.trim() || "まだ入力されていません";

              const cardClick = () => {
                if (mode === "select") {
                  onSelectSession?.(session.id);
                } else {
                  onNavigate?.(session.id);
                }
              };

              return (
                <div
                  key={session.id}
                  className={cn(
                    "space-y-3 rounded-2xl border px-4 py-4 shadow-sm transition hover:border-slate-300",
                    isActive
                      ? "border-slate-900 bg-slate-900/5"
                      : "border-slate-200 bg-white",
                  )}
                >
                  <button
                    type="button"
                    onClick={cardClick}
                    className="flex w-full flex-col items-start text-left"
                  >
                    <div className="flex items-center justify-between gap-3">
                      <p className="text-sm font-semibold text-slate-900">
                        {session.title || "名称未設定"}
                      </p>
                      {!session.isPublic && (
                        <span className="rounded-full bg-slate-100 px-2 py-0.5 text-[10px] font-semibold text-slate-500">
                          非公開
                        </span>
                      )}
                    </div>
                    <p className="mt-2 text-[11px] text-slate-600">
                      <span className="font-semibold text-slate-700">
                        【何の認識を洗い出しますか？】
                      </span>{" "}
                      {context}
                    </p>
                    <p className="mt-1 text-[11px] text-slate-600">
                      <span className="font-semibold text-slate-700">
                        【何のために洗い出しますか？】
                      </span>{" "}
                      {goal}
                    </p>
                  </button>
                  <div className="grid grid-cols-3 gap-2 text-[11px] text-slate-600">
                    <SidebarStat
                      label="参加者"
                      value={session._count.participants}
                    />
                    <SidebarStat
                      label="生成された質問数"
                      value={session._count.statements}
                    />
                    <SidebarStat
                      label="セッション作成日"
                      value={new Date(session.createdAt).toLocaleDateString(
                        "ja-JP",
                      )}
                    />
                  </div>
                  <div className="flex flex-wrap gap-2 border-t border-slate-100 pt-3">
                    {session.isHost &&
                      session.adminAccessToken &&
                      mode === "select" && (
                        <Button
                          size="sm"
                          onClick={() => handleManage(session)}
                          className="min-w-[80px]"
                        >
                          管理
                        </Button>
                      )}
                    <Button
                      variant="outline"
                      size="sm"
                      className="min-w-[80px]"
                      onClick={() => handleParticipate(session.id)}
                    >
                      参加
                    </Button>
                  </div>
                </div>
              );
            })}
          </div>
        )}
      </SidebarGroupContent>
    </SidebarGroup>
  );
}

function SidebarStat({
  label,
  value,
}: {
  label: string;
  value: string | number;
}) {
  return (
    <div className="rounded-xl bg-slate-100/70 px-3 py-2">
      <p className="text-[10px] font-semibold uppercase tracking-wide text-slate-500">
        {label}
      </p>
      <p className="text-sm font-bold text-slate-900">{value}</p>
    </div>
  );
}
