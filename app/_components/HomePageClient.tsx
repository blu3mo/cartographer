"use client";

import axios from "axios";
import {
  ChevronDown,
  ChevronUp,
  ExternalLink,
  FileText,
  Loader2,
  Plus,
  Trash2,
  X,
} from "lucide-react";
import { usePathname, useRouter, useSearchParams } from "next/navigation";
import { useCallback, useEffect, useMemo, useState } from "react";

import { DashboardLayout } from "@/components/layout/DashboardLayout";
import {
  type CreatedSession,
  CreateSessionForm,
} from "@/components/sessions/CreateSessionForm";
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
  const [isCreateModalOpen, setIsCreateModalOpen] = useState(false);
  const [shareBaseUrl, setShareBaseUrl] = useState<string>("");
  const [deletingSessionId, setDeletingSessionId] = useState<string | null>(
    null,
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
    if (typeof window === "undefined") return;
    setShareBaseUrl(window.location.origin);
  }, []);

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

  const openCreateModal = () => setIsCreateModalOpen(true);
  const closeCreateModal = () => setIsCreateModalOpen(false);

  const handleSessionCreated = useCallback(
    (createdSession: CreatedSession) => {
      setSessions((previous) => {
        const normalizedSession: Session = {
          id: createdSession.id,
          title: createdSession.title,
          context: createdSession.context,
          goal: createdSession.goal,
          hostUserId: createdSession.hostUserId,
          adminAccessToken: createdSession.adminAccessToken,
          createdAt: createdSession.createdAt,
          isPublic: createdSession.isPublic,
          isHost: true,
          isParticipant: false,
          _count: {
            participants: 0,
            statements: 0,
          },
        };

        const withoutDuplicate = previous.filter(
          (session) => session.id !== createdSession.id,
        );
        return [normalizedSession, ...withoutDuplicate];
      });
      setSelectedSessionId(createdSession.id);
      syncSelectedSessionQuery(createdSession.id);
      setIsCreateModalOpen(false);
    },
    [syncSelectedSessionQuery],
  );

  const handleDeleteSession = useCallback(async () => {
    if (!selectedAdminSession || !selectedAdminAccessToken || !userId) {
      return;
    }

    const confirmed = window.confirm(
      `セッション「${selectedAdminSession.title || "名称未設定"}」を削除します。よろしいですか？`,
    );
    if (!confirmed) return;

    try {
      setDeletingSessionId(selectedAdminSession.id);
      await axios.delete(
        `/api/sessions/${selectedAdminSession.id}/${selectedAdminAccessToken}`,
        { headers: createAuthorizationHeader(userId) },
      );
      setSessions((previous) =>
        previous.filter((session) => session.id !== selectedAdminSession.id),
      );
      setSelectedSessionId(null);
      syncSelectedSessionQuery(null);
    } catch (err) {
      console.error("Failed to delete session:", err);
      window.alert("セッションの削除に失敗しました。もう一度お試しください。");
    } finally {
      setDeletingSessionId(null);
    }
  }, [
    selectedAdminAccessToken,
    selectedAdminSession,
    syncSelectedSessionQuery,
    userId,
  ]);

  const selectedSessionShareLink = selectedAdminSession
    ? `${shareBaseUrl}/sessions/${selectedAdminSession.id}`
    : "";

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
            size="icon"
            className="h-9 w-9 rounded-full bg-slate-900 text-white hover:bg-slate-900/90"
            aria-label="新しいセッションを作成"
            onClick={openCreateModal}
          >
            <Plus className="h-4 w-4" />
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
          collapsible
          defaultCollapsed
        />
        <SidebarSessionsSection
          title="公開セッション"
          description="まだ参加していない公開セッションです。"
          sessions={discoverSessions}
          onNavigate={(id) => router.push(`/sessions/${id}`)}
          mode="link"
          collapsible
          defaultCollapsed
        />
      </SidebarContent>
    </Sidebar>
  );

  return (
    <>
      <DashboardLayout
        sidebar={sidebar}
        headerContent={null}
        showFooter={false}
      >
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
                  <div className="flex flex-wrap items-start justify-between gap-4">
                    <div>
                      <p className="text-xs font-semibold uppercase tracking-wide text-slate-500">
                        選択中のセッション
                      </p>
                      <div className="mt-1 space-y-1.5">
                        <div className="flex items-center gap-2">
                          <h1 className="text-2xl font-bold text-slate-900">
                            {selectedAdminSession.title || "名称未設定"}
                          </h1>
                          {!selectedAdminSession.isPublic && (
                            <span className="rounded-full border border-slate-200 bg-slate-50 px-2 py-0.5 text-[11px] font-semibold text-slate-600">
                              非公開
                            </span>
                          )}
                        </div>
                        <p className="text-sm text-slate-500 whitespace-pre-wrap">
                          {selectedAdminSession.goal ||
                            selectedAdminSession.context ||
                            "詳細情報は未設定です。"}
                        </p>
                      </div>
                    </div>
                    <div className="flex flex-wrap gap-2">
                      <Button
                        type="button"
                        variant="outline"
                        size="sm"
                        className="gap-1.5"
                        disabled={!selectedSessionShareLink}
                        onClick={() => {
                          if (!selectedSessionShareLink) return;
                          window.open(
                            selectedSessionShareLink,
                            "_blank",
                            "noreferrer",
                          );
                        }}
                      >
                        <ExternalLink className="h-3.5 w-3.5" />
                        参加用URL
                      </Button>
                      <Button
                        type="button"
                        variant="destructive"
                        size="sm"
                        className="gap-1.5"
                        onClick={handleDeleteSession}
                        disabled={deletingSessionId === selectedAdminSession.id}
                        isLoading={
                          deletingSessionId === selectedAdminSession.id
                        }
                      >
                        <Trash2 className="h-3.5 w-3.5" />
                        セッションを削除
                      </Button>
                    </div>
                  </div>
                  <div className="mt-4 grid gap-3 sm:grid-cols-3">
                    <SessionHighlightStat
                      label="参加者"
                      value={selectedAdminSession._count.participants}
                    />
                    <SessionHighlightStat
                      label="生成された質問数"
                      value={selectedAdminSession._count.statements}
                    />
                    <SessionHighlightStat
                      label="セッション作成日"
                      value={new Date(
                        selectedAdminSession.createdAt,
                      ).toLocaleDateString("ja-JP")}
                    />
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
                  <Button onClick={openCreateModal} className="mt-2">
                    <Plus className="h-4 w-4" />
                    最初のセッションを作成
                  </Button>
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

      {isCreateModalOpen && (
        <div className="fixed inset-0 z-50">
          <button
            type="button"
            aria-label="モーダルを閉じる"
            tabIndex={-1}
            className="absolute inset-0 h-full w-full bg-slate-950/60"
            onClick={closeCreateModal}
          />
          <div className="relative z-10 flex h-full w-full items-center justify-center px-4 py-8">
            <div
              className="w-full max-w-3xl rounded-3xl border border-slate-200 bg-white shadow-2xl"
              role="dialog"
              aria-modal="true"
            >
              <div className="flex items-center justify-between border-b border-slate-100 px-6 py-4">
                <div>
                  <p className="text-xs font-semibold uppercase tracking-wide text-slate-500">
                    新しいセッション
                  </p>
                  <h2 className="text-xl font-bold text-slate-900">
                    セッションを作成
                  </h2>
                </div>
                <button
                  type="button"
                  onClick={closeCreateModal}
                  aria-label="閉じる"
                  className="rounded-full border border-slate-200 p-2 text-slate-500 transition-colors hover:border-slate-300 hover:text-slate-900"
                >
                  <X className="h-4 w-4" />
                </button>
              </div>
              <div className="max-h-[75vh] overflow-y-auto px-6 py-6">
                <CreateSessionForm
                  userId={userId}
                  onSuccess={handleSessionCreated}
                  submitButtonLabel="セッションを作成"
                />
              </div>
            </div>
          </div>
        </div>
      )}
    </>
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
  collapsible?: boolean;
  defaultCollapsed?: boolean;
};

function SidebarSessionsSection({
  title,
  description,
  sessions,
  selectedSessionId,
  onSelectSession,
  onNavigate,
  mode,
  collapsible = false,
  defaultCollapsed = false,
}: SidebarSessionsSectionProps) {
  const [collapsed, setCollapsed] = useState(defaultCollapsed);

  useEffect(() => {
    setCollapsed(defaultCollapsed);
  }, [defaultCollapsed]);

  const toggleCollapse = () => {
    if (!collapsible) return;
    setCollapsed((previous) => !previous);
  };

  return (
    <SidebarGroup>
      <SidebarGroupLabel>
        {collapsible ? (
          <button
            type="button"
            onClick={toggleCollapse}
            aria-expanded={!collapsed}
            className="flex w-full items-center justify-between text-left"
          >
            <span>{title}</span>
            <div className="flex items-center gap-2 text-[10px] font-semibold text-[var(--sidebar-foreground)]/60">
              <span>{sessions.length}件</span>
              {collapsed ? (
                <ChevronDown className="h-3 w-3" />
              ) : (
                <ChevronUp className="h-3 w-3" />
              )}
            </div>
          </button>
        ) : (
          <div className="flex w-full items-center justify-between">
            <span>{title}</span>
            <span className="text-[10px] font-semibold text-[var(--sidebar-foreground)]/60">
              {sessions.length}件
            </span>
          </div>
        )}
      </SidebarGroupLabel>
      {!collapsible || !collapsed ? (
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
                    <div className="relative w-full">
                      <button
                        type="button"
                        onClick={cardClick}
                        className="flex w-full flex-col items-start text-left pr-10 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-slate-400"
                      >
                        <div className="flex items-center gap-3">
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
                      <a
                        href={`/sessions/${session.id}`}
                        target="_blank"
                        rel="noreferrer"
                        onClick={(event) => event.stopPropagation()}
                        className="absolute right-0 top-0 inline-flex items-center justify-center rounded-full border border-slate-200 p-1.5 text-slate-500 transition-colors hover:border-slate-300 hover:text-slate-900"
                      >
                        <ExternalLink className="h-3.5 w-3.5" />
                        <span className="sr-only">セッションをプレビュー</span>
                      </a>
                    </div>
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
                  </div>
                );
              })}
            </div>
          )}
        </SidebarGroupContent>
      ) : null}
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

function SessionHighlightStat({
  label,
  value,
}: {
  label: string;
  value: string | number;
}) {
  return (
    <div className="rounded-2xl border border-slate-100 bg-slate-50 px-3 py-2">
      <p className="text-[10px] font-semibold uppercase tracking-wide text-slate-500">
        {label}
      </p>
      <p className="text-lg font-bold text-slate-900">{value}</p>
    </div>
  );
}
