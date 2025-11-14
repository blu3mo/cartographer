"use client";

import axios from "axios";
import type { LucideIcon } from "lucide-react";
import {
  CalendarDays,
  ChevronDown,
  ChevronUp,
  ExternalLink,
  FileText,
  Loader2,
  Plus,
  Trash2,
  Users,
  X,
} from "lucide-react";
import { usePathname, useRouter, useSearchParams } from "next/navigation";
import { useCallback, useEffect, useMemo, useState } from "react";

import { DesktopLayout } from "@/components/home/desktop/DesktopLayout";
import { Header as DesktopHeader } from "@/components/home/desktop/Header";
import { LeftSidebar } from "@/components/home/desktop/leftsidebar/LeftSidebar";
import { LeftSidebarHeader } from "@/components/home/desktop/leftsidebar/LeftSidebarHeader";
import { Session as LeftSidebarSession } from "@/components/home/desktop/leftsidebar/Session";
import { Chat as ChatSection } from "@/components/home/desktop/main/group1/Chat";
import { Group1 } from "@/components/home/desktop/main/group1/Group1";
import { Report as ReportSection } from "@/components/home/desktop/main/group1/Report";
import { RightSidebar as RightSidebarSection } from "@/components/home/desktop/main/group1/RightSidebar";
import { Main as DesktopMain } from "@/components/home/desktop/main/Main";
import { MainHeader } from "@/components/home/desktop/main/MainHeader";
import {
  type CreatedSession,
  CreateSessionForm,
} from "@/components/sessions/CreateSessionForm";
import { SessionSurveyPreview } from "@/components/sessions/SessionSurveyPreview";
import { Button } from "@/components/ui/Button";
import { Input } from "@/components/ui/input";
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
  const [previewSession, setPreviewSession] = useState<CreatedSession | null>(
    null,
  );
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

  const openCreateModal = () => {
    setPreviewSession(null);
    setIsCreateModalOpen(true);
  };
  const closeCreateModal = () => {
    setPreviewSession(null);
    setIsCreateModalOpen(false);
  };

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
      setPreviewSession(createdSession);
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

  const sessionInsightItems = useMemo(
    () =>
      selectedAdminSession
        ? [
            {
              label: "参加者",
              value: selectedAdminSession._count.participants,
            },
            {
              label: "生成された質問数",
              value: selectedAdminSession._count.statements,
            },
            {
              label: "公開状態",
              value: selectedAdminSession.isPublic ? "公開" : "非公開",
            },
            {
              label: "セッション作成日",
              value: new Date(
                selectedAdminSession.createdAt,
              ).toLocaleDateString("ja-JP"),
            },
            {
              label: "共有リンク",
              value: selectedSessionShareLink ? "発行済み" : "未発行",
            },
          ]
        : [],
    [selectedAdminSession, selectedSessionShareLink],
  );

  if (userLoading || loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
      </div>
    );
  }

  const totalSessions = sessions.length;
  const manageableCount = adminSessions.length;
  const participantCount = participantSessions.length;
  const discoverCount = discoverSessions.length;
  const hasSelectedSession = Boolean(
    selectedAdminSession && selectedAdminAccessToken,
  );

  const headerNode = (
    <DesktopHeader>
      <div className="space-y-1">
        <p className="text-3xl font-bold tracking-tight text-slate-900">
          Cartographer
        </p>
        <p className="text-sm text-slate-500">
          チームのアジェンダと認識を同じマップで共有しましょう
        </p>
      </div>
      <div className="hidden items-center gap-3 lg:flex">
        <Button size="sm" className="gap-1.5" onClick={openCreateModal}>
          <Plus className="h-4 w-4" />
          新規セッション
        </Button>
      </div>
    </DesktopHeader>
  );

  const leftSidebarHeaderNode = (
    <LeftSidebarHeader
      title={<span>セッション一覧</span>}
      description={
        <span className="text-slate-400">
          合計 {totalSessions} 件のセッション
        </span>
      }
      toolbar={
        <>
          <Input
            type="search"
            placeholder="タイトルや目的で検索"
            value={searchTerm}
            onChange={(event) => setSearchTerm(event.target.value)}
            className="flex-1 rounded-xl border border-slate-700 bg-slate-900/70 px-3 py-2 text-sm text-slate-100 placeholder:text-slate-500 focus-visible:ring-slate-500"
          />
          <Button
            type="button"
            size="icon"
            variant="secondary"
            className="bg-slate-100 text-slate-900 hover:bg-slate-200"
            onClick={openCreateModal}
          >
            <Plus className="h-4 w-4" />
          </Button>
        </>
      }
    />
  );

  const sessionSummaryCards = (
    <div className="grid gap-3 text-[11px] uppercase tracking-[0.18em] text-slate-400">
      <div className="rounded-2xl border border-slate-800/50 bg-slate-900/60 px-4 py-3">
        <p className="font-semibold text-slate-300">管理中</p>
        <p className="text-2xl font-bold text-white">{manageableCount}</p>
      </div>
      <div className="rounded-2xl border border-slate-800/40 bg-slate-900/50 px-4 py-3">
        <p className="font-semibold text-slate-300">参加中</p>
        <p className="text-2xl font-bold text-white">{participantCount}</p>
      </div>
      <div className="rounded-2xl border border-slate-800/40 bg-slate-900/50 px-4 py-3">
        <p className="font-semibold text-slate-300">公開</p>
        <p className="text-2xl font-bold text-white">{discoverCount}</p>
      </div>
    </div>
  );

  const sessionSections = (
    <LeftSidebarSession>
      {sessionSummaryCards}
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
    </LeftSidebarSession>
  );

  const mobileSessions = (
    <div className="space-y-4">
      <div className="grid grid-cols-3 gap-2 text-center text-xs text-slate-500">
        <div className="rounded-xl border border-slate-200 bg-white px-3 py-2">
          <p className="font-semibold uppercase tracking-[0.18em] text-slate-500">
            管理中
          </p>
          <p className="text-lg font-bold text-slate-700">{manageableCount}</p>
        </div>
        <div className="rounded-xl border border-slate-200 bg-white px-3 py-2">
          <p className="font-semibold uppercase tracking-[0.18em] text-slate-500">
            参加中
          </p>
          <p className="text-lg font-bold text-slate-700">{participantCount}</p>
        </div>
        <div className="rounded-xl border border-slate-200 bg-white px-3 py-2">
          <p className="font-semibold uppercase tracking-[0.18em] text-slate-500">
            公開
          </p>
          <p className="text-lg font-bold text-slate-700">{discoverCount}</p>
        </div>
      </div>
      <SidebarSessionsSection
        title="管理中"
        description="あなたが管理できるセッションです。"
        sessions={adminSessions}
        selectedSessionId={selectedSessionId}
        onSelectSession={handleSessionSelect}
        mode="select"
        collapsible
        defaultCollapsed={false}
      />
    </div>
  );

  const shareButton = selectedSessionShareLink ? (
    <Button
      type="button"
      variant="outline"
      size="sm"
      className="gap-1.5"
      onClick={() => {
        if (!selectedSessionShareLink) return;
        window.open(selectedSessionShareLink, "_blank", "noreferrer");
      }}
    >
      <ExternalLink className="h-3.5 w-3.5" />
      参加用URL
    </Button>
  ) : null;

  const deleteButton = selectedAdminSession ? (
    <Button
      type="button"
      variant="destructive"
      size="sm"
      className="gap-1.5"
      onClick={handleDeleteSession}
      disabled={deletingSessionId === selectedAdminSession.id}
      isLoading={deletingSessionId === selectedAdminSession.id}
    >
      <Trash2 className="h-3.5 w-3.5" />
      セッションを削除
    </Button>
  ) : null;

  const mainHeaderNode = (
    <MainHeader
      title={
        selectedAdminSession ? (
          <div className="flex flex-wrap items-center gap-2">
            <span>{selectedAdminSession.title || "名称未設定"}</span>
            {!selectedAdminSession.isPublic && (
              <span className="rounded-full border border-slate-200 bg-slate-50 px-2 py-0.5 text-xs font-semibold text-slate-600">
                非公開
              </span>
            )}
          </div>
        ) : sessions.length === 0 ? (
          "まだセッションがありません"
        ) : (
          "セッションを選択してください"
        )
      }
      description={
        selectedAdminSession
          ? selectedAdminSession.goal ||
            selectedAdminSession.context ||
            "目的や背景は未設定です。"
          : adminSessions.length > 0
            ? "左のセッション一覧から管理対象を選ぶと、レポートが表示されます。"
            : "まずは新しいセッションを作成して、議論の土台を準備しましょう。"
      }
      actions={
        hasSelectedSession && (
          <div className="flex flex-wrap items-center gap-2">
            {shareButton}
            {deleteButton}
          </div>
        )
      }
    />
  );

  const errorBanner = error ? (
    <div className="rounded-2xl border border-amber-300 bg-amber-50 px-4 py-3 text-sm text-amber-700">
      {error}
    </div>
  ) : null;

  const reportHeaderContent = (
    <div className="space-y-4">
      {errorBanner}
      {hasSelectedSession ? (
        <div className="grid gap-3 sm:grid-cols-3">
          <SessionHighlightStat
            label="参加者"
            value={selectedAdminSession?._count.participants ?? 0}
          />
          <SessionHighlightStat
            label="生成された質問数"
            value={selectedAdminSession?._count.statements ?? 0}
          />
          <SessionHighlightStat
            label="セッション作成日"
            value={
              selectedAdminSession
                ? new Date(selectedAdminSession.createdAt).toLocaleDateString(
                    "ja-JP",
                  )
                : "-"
            }
          />
        </div>
      ) : (
        <p className="text-sm text-slate-500">
          {adminSessions.length > 0
            ? "左のセッションから選択するとレポートが表示されます。"
            : "セッションを作成するとここにレポートが表示されます。"}
        </p>
      )}
    </div>
  );

  const reportContent =
    hasSelectedSession && selectedAdminSession && selectedAdminAccessToken ? (
      <SessionAdminDashboard
        key={selectedAdminSession.id}
        sessionId={selectedAdminSession.id}
        accessToken={selectedAdminAccessToken}
        embedded
        showHeader={false}
      />
    ) : (
      <div className="flex h-full flex-col items-center justify-center gap-3 text-center text-sm text-slate-500">
        {sessions.length === 0 ? (
          <>
            <FileText className="h-8 w-8 text-slate-400" />
            <p>セッションを作成すると、ここにレポートが表示されます。</p>
            <Button onClick={openCreateModal}>
              <Plus className="mr-1 h-4 w-4" />
              新しいセッションを作成
            </Button>
          </>
        ) : (
          <p>セッションを選択するとレポートが表示されます。</p>
        )}
      </div>
    );

  const chatNode = (
    <ChatSection
      title={
        hasSelectedSession
          ? "コメント"
          : "セッションに紐づくコメントを表示します"
      }
      description={
        hasSelectedSession
          ? "チームから寄せられた意見やメモをまとめて、次のアクションにつなげましょう。"
          : "左のリストからセッションを選択すると、コメント履歴にアクセスできます。"
      }
      action={
        hasSelectedSession ? (
          <Button
            type="button"
            variant="outline"
            size="sm"
            className="gap-1.5"
            onClick={() => {
              if (!selectedAdminSession) return;
              router.push(`/sessions/${selectedAdminSession.id}`);
            }}
          >
            <ExternalLink className="h-3.5 w-3.5" />
            詳細ビューへ
          </Button>
        ) : (
          <Button
            type="button"
            variant="outline"
            size="sm"
            onClick={openCreateModal}
          >
            <Plus className="mr-1 h-4 w-4" />
            セッションを作成
          </Button>
        )
      }
    />
  );

  const rightSidebarSummary = hasSelectedSession ? (
    <div className="space-y-3">
      {sessionInsightItems.map((item) => (
        <SessionHighlightStat
          key={item.label}
          label={item.label}
          value={item.value}
        />
      ))}
    </div>
  ) : (
    <p className="text-sm text-slate-500">
      セッションを選択すると、主要な統計情報とクイックアクションが表示されます。
    </p>
  );

  const rightSidebarActions =
    hasSelectedSession && (shareButton || deleteButton) ? (
      <div className="flex flex-col gap-2">
        {shareButton}
        {deleteButton}
      </div>
    ) : undefined;

  return (
    <>
      <DesktopLayout
        header={headerNode}
        leftSidebar={
          <LeftSidebar
            header={leftSidebarHeaderNode}
            session={sessionSections}
          />
        }
        main={
          <DesktopMain
            header={mainHeaderNode}
            mobileSessions={mobileSessions}
            group={
              <Group1
                report={
                  <ReportSection
                    header={reportHeaderContent}
                    content={reportContent}
                  />
                }
                chat={chatNode}
                rightSidebar={
                  <RightSidebarSection
                    summary={rightSidebarSummary}
                    actions={rightSidebarActions}
                  />
                }
              />
            }
          />
        }
      />

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
                {previewSession ? (
                  <SessionSurveyPreview
                    session={previewSession}
                    userId={userId}
                    onReset={() => setPreviewSession(null)}
                  />
                ) : (
                  <CreateSessionForm
                    userId={userId}
                    onSuccess={handleSessionCreated}
                    submitButtonLabel="セッションを作成"
                  />
                )}
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
    <div className="space-y-2">
      <button
        type="button"
        className="flex w-full items-center justify-between rounded-xl border border-slate-800/50 bg-slate-900/40 px-3 py-2 text-left text-xs font-semibold uppercase tracking-[0.18em] text-slate-300"
        onClick={toggleCollapse}
        aria-expanded={!collapsed}
      >
        <span>{title}</span>
        {collapsible && (
          <span className="text-slate-400">
            {collapsed ? (
              <ChevronDown className="h-4 w-4" />
            ) : (
              <ChevronUp className="h-4 w-4" />
            )}
          </span>
        )}
      </button>
      <p className="text-[11px] text-slate-400">{description}</p>
      {!collapsed && (
        <div className="space-y-2">
          {sessions.length === 0 ? (
            <p className="rounded-xl border border-dashed border-slate-700 px-3 py-3 text-[11px] text-slate-400">
              {description}
            </p>
          ) : (
            sessions.map((session) => {
              const isActive = selectedSessionId === session.id;
              const context =
                session.context?.trim() || "まだ入力されていません";
              const goal = session.goal?.trim() || "まだ入力されていません";

              const handleClick = () => {
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
                    "rounded-2xl border bg-slate-900/40 p-3 text-left shadow-sm",
                    isActive
                      ? "border-slate-100/80 bg-slate-100/20 text-white"
                      : "border-transparent text-slate-100",
                  )}
                >
                  <button
                    type="button"
                    onClick={handleClick}
                    className="flex w-full flex-col items-start gap-2 text-left"
                  >
                    <div className="flex w-full items-center gap-2">
                      <p className="text-sm font-semibold">
                        {session.title || "名称未設定"}
                      </p>
                      {!session.isPublic && (
                        <span className="rounded-full bg-slate-100/20 px-2 py-0.5 text-[10px] font-semibold text-slate-200">
                          非公開
                        </span>
                      )}
                      <a
                        href={`/sessions/${session.id}`}
                        target="_blank"
                        rel="noreferrer"
                        onClick={(event) => event.stopPropagation()}
                        className="ml-auto inline-flex items-center justify-center rounded-full border border-white/20 p-1.5 text-slate-200 transition-colors hover:border-white/40 hover:text-white"
                      >
                        <ExternalLink className="h-3.5 w-3.5" />
                        <span className="sr-only">
                          セッションを新しいタブで開く
                        </span>
                      </a>
                    </div>
                    <p className="text-[11px] text-slate-200/80">{context}</p>
                    <p className="text-[11px] text-slate-200/80">{goal}</p>
                  </button>
                  <div className="mt-3 flex flex-wrap items-center gap-x-3 gap-y-1 text-slate-200/80">
                    <SidebarStat
                      icon={Users}
                      label="参加者"
                      value={session._count.participants}
                    />
                    <SidebarStat
                      icon={FileText}
                      label="質問"
                      value={session._count.statements}
                    />
                    <SidebarStat
                      icon={CalendarDays}
                      label="作成日"
                      value={new Date(session.createdAt).toLocaleDateString(
                        "ja-JP",
                      )}
                    />
                  </div>
                </div>
              );
            })
          )}
        </div>
      )}
    </div>
  );
}

function SidebarStat({
  icon: Icon,
  label,
  value,
}: {
  icon: LucideIcon;
  label: string;
  value: string | number;
}) {
  return (
    <span className="inline-flex items-center gap-1.5 text-[11px]">
      <Icon className="h-3.5 w-3.5 text-slate-300" aria-hidden />
      <span className="font-semibold text-white">{value}</span>
      <span className="sr-only">{label}</span>
    </span>
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
