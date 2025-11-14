"use client";

import axios from "axios";
import { ExternalLink, Loader2, Plus, Trash2, X } from "lucide-react";
import { usePathname, useRouter, useSearchParams } from "next/navigation";
import { useCallback, useEffect, useMemo, useState } from "react";
import {
  type CreatedSession,
  CreateSessionForm,
} from "@/components/sessions/CreateSessionForm";
import { SessionSurveyPreview } from "@/components/sessions/SessionSurveyPreview";
import { Button } from "@/components/ui/Button";
import { DesktopLayout } from "@/dashboard/_components/layout/desktop/DesktopLayout";
import { Header as DesktopHeader } from "@/dashboard/_components/layout/desktop/Header";
import { Main as DesktopMain } from "@/dashboard/_components/layout/desktop/main/Main";
import { MainHeader } from "@/dashboard/_components/layout/desktop/main/MainHeader";
import {
  SessionNavigatorMobile,
  SessionNavigatorSidebar,
} from "@/dashboard/_components/SessionNavigator";
import { SessionSummary } from "@/dashboard/_components/SessionSummary";
import { SessionWorkspace } from "@/dashboard/_components/SessionWorkspace";
import type { Session } from "@/dashboard/_components/types";
import { createAuthorizationHeader } from "@/lib/auth";
import { useUserId } from "@/lib/useUserId";
import { SessionAdminDashboard } from "@/sessions/_components/SessionAdminDashboard";

export function DashboardClient() {
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

  const openCreateModal = useCallback(() => {
    setPreviewSession(null);
    setIsCreateModalOpen(true);
  }, []);

  const closeCreateModal = useCallback(() => {
    setPreviewSession(null);
    setIsCreateModalOpen(false);
  }, []);

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

  const summaryInsights = useMemo(
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

  const workspaceMetrics = useMemo(
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
              label: "セッション作成日",
              value: new Date(
                selectedAdminSession.createdAt,
              ).toLocaleDateString("ja-JP"),
            },
          ]
        : [],
    [selectedAdminSession],
  );

  const renderShareButton = useCallback(
    () =>
      selectedSessionShareLink ? (
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
      ) : null,
    [selectedSessionShareLink],
  );

  const renderDeleteButton = useCallback(
    () =>
      selectedAdminSession ? (
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
      ) : null,
    [deletingSessionId, handleDeleteSession, selectedAdminSession],
  );

  if (userLoading || loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
      </div>
    );
  }

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

  const hasSelectedSession = Boolean(
    selectedAdminSession && selectedAdminAccessToken,
  );

  const mainHeader = (
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
      actions={null}
    />
  );

  return (
    <>
      <DesktopLayout
        header={headerNode}
        leftSidebar={
          <SessionNavigatorSidebar
            totalSessions={sessions.length}
            searchTerm={searchTerm}
            onSearchTermChange={setSearchTerm}
            manageableCount={adminSessions.length}
            participantCount={participantSessions.length}
            discoverCount={discoverSessions.length}
            adminSessions={adminSessions}
            participantSessions={participantSessions}
            discoverSessions={discoverSessions}
            selectedSessionId={selectedSessionId}
            onSelectSession={handleSessionSelect}
            onNavigateSession={(id) => router.push(`/sessions/${id}`)}
            onCreateSession={openCreateModal}
          />
        }
        main={
          <DesktopMain
            header={mainHeader}
            mobileSessions={
              <SessionNavigatorMobile
                totalSessions={sessions.length}
                searchTerm={searchTerm}
                onSearchTermChange={setSearchTerm}
                manageableCount={adminSessions.length}
                participantCount={participantSessions.length}
                discoverCount={discoverSessions.length}
                adminSessions={adminSessions}
                participantSessions={participantSessions}
                discoverSessions={discoverSessions}
                selectedSessionId={selectedSessionId}
                onSelectSession={handleSessionSelect}
                onNavigateSession={(id) => router.push(`/sessions/${id}`)}
                onCreateSession={openCreateModal}
              />
            }
            group={
              <SessionWorkspace
                selectedSession={selectedAdminSession}
                hasAccessToken={Boolean(selectedAdminAccessToken)}
                metrics={workspaceMetrics}
                reportContent={
                  hasSelectedSession && selectedAdminSession ? (
                    <SessionAdminDashboard
                      key={selectedAdminSession.id}
                      sessionId={selectedAdminSession.id}
                      accessToken={selectedAdminAccessToken ?? ""}
                      embedded
                      showHeader={false}
                    />
                  ) : null
                }
                shareButton={hasSelectedSession ? renderShareButton() : null}
                deleteButton={hasSelectedSession ? renderDeleteButton() : null}
                error={error}
                onCreateSession={openCreateModal}
                totalSessions={sessions.length}
                adminSessionsCount={adminSessions.length}
                rightSidebar={
                  <SessionSummary
                    insights={summaryInsights}
                    hasSelection={hasSelectedSession}
                    shareButton={
                      hasSelectedSession ? renderShareButton() : null
                    }
                    deleteButton={
                      hasSelectedSession ? renderDeleteButton() : null
                    }
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
