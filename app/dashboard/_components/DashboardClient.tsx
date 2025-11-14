"use client";

import axios from "axios";
import {
  Calendar,
  ChevronLeft,
  ChevronRight,
  Copy,
  ExternalLink,
  FileText,
  Loader2,
  Lock,
  Maximize2,
  MessageSquare,
  Plus,
  Trash2,
  Users,
  X,
} from "lucide-react";
import Image from "next/image";
import { usePathname, useRouter, useSearchParams } from "next/navigation";
import {
  type ReactNode,
  useCallback,
  useEffect,
  useMemo,
  useState,
} from "react";

import {
  type CreatedSession,
  CreateSessionForm,
} from "@/components/sessions/CreateSessionForm";
import { SessionSurveyPreview } from "@/components/sessions/SessionSurveyPreview";
import { Button } from "@/components/ui/Button";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import type { Session } from "@/dashboard/_components/types";
import { createAuthorizationHeader } from "@/lib/auth";
import { useUserId } from "@/lib/useUserId";
import { cn } from "@/lib/utils";
import { SessionAdminDashboard } from "@/sessions/_components/SessionAdminDashboard";

type StatementResponseStats = {
  strongYes: number;
  yes: number;
  dontKnow: number;
  no: number;
  strongNo: number;
  totalCount: number;
};

type StatementWithStats = {
  id: string;
  sessionId: string;
  text: string;
  orderIndex: number;
  responses: StatementResponseStats;
  agreementScore: number;
};

type ParticipantProgress = {
  userId: string;
  name: string;
  answeredCount: number;
  completionRate: number;
  totalStatements: number;
  updatedAt: string;
};

type AdminSessionDetail = {
  id: string;
  title: string;
  context: string;
  goal: string;
  isPublic: boolean;
  createdAt: string;
  statements: StatementWithStats[];
  participants: ParticipantProgress[];
  totalStatements: number;
  totalParticipants: number;
};

type TimelineEventStatement = {
  id: string;
  text: string;
  orderIndex: number;
};

type TimelineEvent = {
  id: string;
  type: string;
  agentId: string | null;
  userId: string | null;
  progress: number | string | null;
  payload: Record<string, unknown>;
  orderIndex: number;
  createdAt: string;
  updatedAt: string;
  statements: TimelineEventStatement[];
};

type StatementHighlight = {
  id: string;
  text: string;
  totalResponses: number;
  agreementScore: number;
  positiveShare: number;
  negativeShare: number;
};

const EVENT_TYPE_LABELS: Record<string, string> = {
  plan: "プラン生成",
  survey: "アンケート実施",
  survey_analysis: "分析",
  user_message: "メッセージ",
};

const SHARE_QR_SIZE = 176;
const FULLSCREEN_QR_SIZE = 768;

const buildQrUrl = (url: string, size: number) =>
  `https://api.qrserver.com/v1/create-qr-code/?size=${size}x${size}&data=${encodeURIComponent(
    url,
  )}`;

const formatRelativeTime = (value: string) => {
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) return "";
  const now = new Date();
  const diff = now.getTime() - date.getTime();
  const minutes = Math.floor(diff / 60000);
  if (minutes < 1) return "たった今";
  if (minutes < 60) return `${minutes}分前`;
  const hours = Math.floor(minutes / 60);
  if (hours < 24) return `${hours}時間前`;
  const days = Math.floor(hours / 24);
  if (days < 7) return `${days}日前`;
  return date.toLocaleString("ja-JP", {
    month: "short",
    day: "numeric",
    hour: "2-digit",
    minute: "2-digit",
    hour12: false,
  });
};

const formatDate = (value: string) => {
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) return "-";
  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, "0");
  const day = String(date.getDate()).padStart(2, "0");
  return `${year}/${month}/${day}`;
};

const formatPercent = (value: number) => {
  if (Number.isNaN(value)) return "0%";
  const rounded = Math.round(value * 10) / 10;
  return Number.isInteger(rounded)
    ? `${rounded.toFixed(0)}%`
    : `${rounded.toFixed(1)}%`;
};

function VisibilityBadge({ isPublic }: { isPublic: boolean }) {
  if (isPublic) {
    return (
      <span className="inline-flex items-center rounded-full border border-emerald-200 bg-emerald-50 px-2 py-0.5 text-[10px] font-semibold uppercase tracking-wide text-emerald-700">
        公開
      </span>
    );
  }

  return (
    <span className="inline-flex items-center justify-center rounded-full border border-slate-300 bg-slate-100 p-1.5">
      <Lock className="h-3 w-3 text-slate-700" />
    </span>
  );
}

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
  const [isSidebarCollapsed, setIsSidebarCollapsed] = useState(false);
  const [sessionDetail, setSessionDetail] = useState<AdminSessionDetail | null>(
    null,
  );
  const [detailLoading, setDetailLoading] = useState(false);
  const [detailError, setDetailError] = useState<string | null>(null);
  const [eventLog, setEventLog] = useState<TimelineEvent[]>([]);
  const [eventLogLoading, setEventLogLoading] = useState(false);
  const [eventLogError, setEventLogError] = useState<string | null>(null);
  const [copyStatus, setCopyStatus] = useState<"idle" | "copied" | "error">(
    "idle",
  );
  const [reportRequestControls, setReportRequestControls] =
    useState<ReactNode | null>(null);
  const [reportHeader, setReportHeader] = useState<ReactNode | null>(null);
  const [isShareQrFullscreen, setIsShareQrFullscreen] = useState(false);
  const [isShareQrErrored, setIsShareQrErrored] = useState(false);

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

  const managedSessions = useMemo(
    () => filteredSessions.filter((session) => session.isHost),
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
    if (managedSessions.length === 0) {
      if (selectedSessionId !== null) {
        setSelectedSessionId(null);
        syncSelectedSessionQuery(null);
      }
      return;
    }

    if (!selectedSessionId) {
      const fallback = managedSessions[0].id;
      setSelectedSessionId(fallback);
      syncSelectedSessionQuery(fallback);
      return;
    }

    const exists = managedSessions.some(
      (session) => session.id === selectedSessionId,
    );
    if (!exists) {
      const fallback = managedSessions[0].id;
      setSelectedSessionId(fallback);
      syncSelectedSessionQuery(fallback);
    }
  }, [managedSessions, selectedSessionId, syncSelectedSessionQuery]);

  const selectedAdminSession =
    managedSessions.find((session) => session.id === selectedSessionId) ?? null;
  const selectedAdminAccessToken =
    selectedAdminSession?.adminAccessToken ?? null;

  const selectedSessionShareLink = selectedAdminSession
    ? `${shareBaseUrl}/sessions/${selectedAdminSession.id}`
    : "";

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

  useEffect(() => {
    setCopyStatus("idle");
    setIsShareQrFullscreen(false);
    if (!selectedAdminSession || !selectedAdminAccessToken || !userId) {
      setSessionDetail(null);
      setDetailError(null);
      setEventLog([]);
      setEventLogError(null);
      setReportRequestControls(null);
      setReportHeader(null);
      setIsShareQrErrored(false);
      return;
    }

    let cancelled = false;
    setReportRequestControls(null);
    setReportHeader(null);

    const fetchDetail = async () => {
      try {
        setDetailLoading(true);
        setDetailError(null);
        const response = await axios.get(
          `/api/sessions/${selectedAdminSession.id}/${selectedAdminAccessToken}`,
          {
            headers: createAuthorizationHeader(userId),
          },
        );
        if (cancelled) return;
        setSessionDetail(response.data.data as AdminSessionDetail);
      } catch (err) {
        if (cancelled) return;
        console.error("Failed to fetch session detail:", err);
        setDetailError("セッション詳細の取得に失敗しました。");
        setSessionDetail(null);
      } finally {
        if (!cancelled) {
          setDetailLoading(false);
        }
      }
    };

    const fetchEventThread = async () => {
      try {
        setEventLogLoading(true);
        setEventLogError(null);
        const response = await axios.get(
          `/api/sessions/${selectedAdminSession.id}/${selectedAdminAccessToken}/event-thread`,
          {
            headers: createAuthorizationHeader(userId),
          },
        );
        if (cancelled) return;
        setEventLog((response.data?.events ?? []) as TimelineEvent[]);
      } catch (err) {
        if (cancelled) return;
        console.error("Failed to fetch event thread:", err);
        setEventLogError("進行ログの取得に失敗しました。");
        setEventLog([]);
      } finally {
        if (!cancelled) {
          setEventLogLoading(false);
        }
      }
    };

    fetchDetail();
    fetchEventThread();
    setIsShareQrErrored(false);

    return () => {
      cancelled = true;
    };
  }, [selectedAdminAccessToken, selectedAdminSession, userId]);

  const statementHighlights = useMemo<StatementHighlight[]>(() => {
    if (!sessionDetail?.statements?.length) return [];
    const sorted = [...sessionDetail.statements].sort(
      (a, b) => b.responses.totalCount - a.responses.totalCount,
    );
    return sorted.slice(0, 3).map((statement) => {
      const positive = statement.responses.strongYes + statement.responses.yes;
      const negative = statement.responses.strongNo + statement.responses.no;
      return {
        id: statement.id,
        text: statement.text,
        totalResponses: statement.responses.totalCount,
        agreementScore: statement.agreementScore,
        positiveShare: Math.round(positive * 10) / 10,
        negativeShare: Math.round(negative * 10) / 10,
      };
    });
  }, [sessionDetail]);

  const participantProgress = useMemo(() => {
    if (!sessionDetail?.participants?.length) return [];
    return [...sessionDetail.participants].sort(
      (a, b) => b.completionRate - a.completionRate,
    );
  }, [sessionDetail]);

  const latestEvents = useMemo(() => {
    if (!eventLog.length) return [];
    return [...eventLog]
      .sort(
        (a, b) =>
          new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime(),
      )
      .slice(0, 5);
  }, [eventLog]);

  const shareQrSrc = useMemo(() => {
    if (!selectedSessionShareLink) return "";
    return buildQrUrl(selectedSessionShareLink, SHARE_QR_SIZE);
  }, [selectedSessionShareLink]);

  const shareQrFullscreenSrc = useMemo(() => {
    if (!selectedSessionShareLink) return "";
    return buildQrUrl(selectedSessionShareLink, FULLSCREEN_QR_SIZE);
  }, [selectedSessionShareLink]);

  const handleCopyShareLink = useCallback(async () => {
    if (!selectedSessionShareLink) return;
    if (
      typeof navigator === "undefined" ||
      !navigator.clipboard ||
      typeof navigator.clipboard.writeText !== "function"
    ) {
      setCopyStatus("error");
      return;
    }
    try {
      await navigator.clipboard.writeText(selectedSessionShareLink);
      setCopyStatus("copied");
      window.setTimeout(() => setCopyStatus("idle"), 2000);
    } catch (err) {
      console.error("Failed to copy share link:", err);
      setCopyStatus("error");
      window.setTimeout(() => setCopyStatus("idle"), 2000);
    }
  }, [selectedSessionShareLink]);

  const headerStats = useMemo(() => {
    if (!selectedAdminSession) return [];
    return [
      {
        label: "参加者",
        value: String(
          sessionDetail?.totalParticipants ??
            selectedAdminSession._count.participants,
        ),
        icon: Users,
      },
      {
        label: "質問数",
        value: String(
          sessionDetail?.totalStatements ??
            selectedAdminSession._count.statements,
        ),
        icon: FileText,
      },
      {
        label: "作成",
        value: `${formatDate(selectedAdminSession.createdAt)}`,
        icon: Calendar,
      },
    ];
  }, [selectedAdminSession, sessionDetail]);

  if (userLoading || loading) {
    return (
      <div className="flex min-h-screen items-center justify-center">
        <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
      </div>
    );
  }

  const hasSelectedSession = Boolean(
    selectedAdminSession && selectedAdminAccessToken,
  );

  return (
    <>
      <div className="flex h-screen flex-col bg-background">
        <header className="flex h-20 flex-col justify-center gap-2 border-b bg-primary px-6 text-primary-foreground">
          <div className="flex items-center gap-3">
            {/* <FileText className="h-5 w-5" /> */}
            <span className="text-sm font-semibold uppercase tracking-wide">
              Cartographer
            </span>
          </div>
          {/* <div className="flex items-center justify-between">
            <h1 className="text-lg font-semibold">
              {selectedAdminSession
                ? selectedAdminSession.title || "名称未設定"
                : "セッションを選択してください"}
            </h1>
            <Button variant="ghost" size="icon">
              <User className="h-4 w-4" />
            </Button>
          </div> */}
        </header>

        <div className="flex flex-1 overflow-hidden">
          <aside
            className={cn(
              "flex flex-col border-r bg-muted/30 transition-all duration-300",
              isSidebarCollapsed ? "w-0" : "w-80",
            )}
          >
            {!isSidebarCollapsed && (
              <>
                <div className="flex h-16 items-center gap-2 border-b px-4">
                  <Input
                    placeholder="セッションを検索..."
                    value={searchTerm}
                    onChange={(event) => setSearchTerm(event.target.value)}
                    className="h-9 flex-1"
                  />
                  <Button
                    size="icon"
                    variant="outline"
                    className="h-9 w-9"
                    onClick={openCreateModal}
                  >
                    <Plus className="h-4 w-4" />
                  </Button>
                </div>
                <div className="flex-1 overflow-y-auto px-4 py-4">
                  {managedSessions.length > 0 ? (
                    <div className="space-y-3">
                      {managedSessions.map((session) => {
                        const isSelected = selectedSessionId === session.id;
                        return (
                          <button
                            key={session.id}
                            type="button"
                            onClick={() => handleSessionSelect(session.id)}
                            className={cn(
                              "w-full rounded-xl border px-4 py-3 text-left transition-all",
                              isSelected
                                ? "border-primary bg-primary/10 shadow-sm"
                                : "border-transparent bg-card hover:border-border hover:bg-muted/40",
                            )}
                          >
                            <div className="flex items-start justify-between gap-3">
                              <div className="min-w-0 space-y-1">
                                <p className="truncate text-sm font-semibold text-card-foreground">
                                  {session.title || "名称未設定"}
                                </p>
                                <p className="line-clamp-2 text-xs text-muted-foreground">
                                  {session.goal ||
                                    session.context ||
                                    "セッションの説明は未設定です。"}
                                </p>
                              </div>
                              <VisibilityBadge isPublic={session.isPublic} />
                            </div>
                            <div className="mt-3 flex items-center gap-3 text-[11px] text-muted-foreground">
                              <span className="inline-flex items-center gap-1">
                                <Users className="h-3 w-3" />
                                {session._count.participants}
                              </span>
                              <span className="inline-flex items-center gap-1">
                                <FileText className="h-3 w-3" />
                                {session._count.statements}
                              </span>
                              <span className="inline-flex items-center gap-1">
                                <Calendar className="h-3 w-3" />
                                {formatDate(session.createdAt)}
                              </span>
                            </div>
                          </button>
                        );
                      })}
                    </div>
                  ) : (
                    <div className="flex min-h-[200px] items-center justify-center rounded-xl border border-dashed border-muted-foreground/40 bg-muted/20 px-4 text-center text-xs text-muted-foreground">
                      管理中のセッションがありません。新規作成してチームの対話を始めましょう。
                    </div>
                  )}
                </div>
              </>
            )}
          </aside>

          <main className="flex flex-1 flex-col overflow-hidden">
            <div className="flex h-16 items-center justify-between border-b bg-card px-6">
              <div className="flex items-center gap-4">
                <Button
                  variant="ghost"
                  size="icon"
                  className="h-9 w-9"
                  onClick={() => setIsSidebarCollapsed((value) => !value)}
                >
                  {isSidebarCollapsed ? (
                    <ChevronRight className="h-4 w-4" />
                  ) : (
                    <ChevronLeft className="h-4 w-4" />
                  )}
                </Button>
                <div className="space-y-1">
                  <h2 className="text-lg font-semibold text-card-foreground">
                    {selectedAdminSession
                      ? selectedAdminSession.title || "名称未設定"
                      : sessions.length === 0
                        ? "まだセッションがありません"
                        : "管理可能なセッションを選択してください"}
                  </h2>
                  <p className="text-xs text-muted-foreground">
                    {selectedAdminSession
                      ? selectedAdminSession.goal ||
                        selectedAdminSession.context ||
                        "セッションの目的や背景は未設定です。"
                      : managedSessions.length > 0
                        ? "左側の一覧からセッションを選ぶとレポートが表示されます。"
                        : "まずは新しいセッションを作成して、対話を設計しましょう。"}
                  </p>
                </div>
              </div>
              {headerStats.length > 0 && (
                <div className="hidden items-center gap-3 md:flex">
                  {headerStats.map((stat) => {
                    const Icon = stat.icon;
                    return (
                      <div
                        key={stat.label}
                        className="flex items-center gap-2 rounded-full bg-muted px-3 py-1 text-xs text-muted-foreground"
                      >
                        <Icon className="h-3.5 w-3.5 text-muted-foreground" />
                        <span className="font-semibold text-foreground">
                          {stat.value}
                        </span>
                        <span>{stat.label}</span>
                      </div>
                    );
                  })}
                </div>
              )}
            </div>

            {/* <div className="border-b bg-card px-6 py-4">
              <div className="space-y-4">
                <div className="space-y-1.5">
                  <h3 className="text-lg font-semibold text-card-foreground">
                    分析レポート
                  </h3>
                  <p className="text-xs text-muted-foreground">
                    参加者の回答結果や、セッション情報をもとに、現状把握レポートを生成します。
                  </p>
                </div>
                {reportHeader && <div>{reportHeader}</div>}
              </div>
            </div> */}

            <div className="flex flex-1 overflow-hidden">
              <div className="flex flex-1 flex-col overflow-hidden border-r">

                <div className="border-b bg-card px-6 py-4">
                  <div className="space-y-4">
                    <div className="space-y-1.5">
                      <h3 className="text-lg font-semibold text-card-foreground">
                        分析レポート
                      </h3>
                      <p className="text-xs text-muted-foreground">
                        参加者の回答結果や、セッション情報をもとに、現状把握レポートを生成します。
                      </p>
                    </div>
                    {reportHeader && <div>{reportHeader}</div>}
                  </div>
                </div>

                <div className="flex-1 overflow-hidden bg-muted/30 p-6">
                  <div className="flex h-full flex-col gap-4">
                    {error && (
                      <Card className="border-destructive/40 bg-destructive/10 text-destructive">
                        <CardHeader className="pb-2">
                          <CardTitle className="text-sm font-semibold">
                            データの取得に失敗しました
                          </CardTitle>
                          <CardDescription className="text-xs text-destructive">
                            {error}
                          </CardDescription>
                        </CardHeader>
                      </Card>
                    )}
                    <Card className="flex h-full flex-col border-border/60">
                      <CardContent className="flex-1 overflow-hidden p-4">
                        {detailLoading && hasSelectedSession ? (
                          <div className="flex h-full items-center justify-center">
                            <Loader2 className="h-6 w-6 animate-spin text-muted-foreground" />
                          </div>
                        ) : hasSelectedSession && selectedAdminAccessToken ? (
                          <SessionAdminDashboard
                            key={selectedAdminSession?.id}
                            sessionId={selectedAdminSession?.id ?? ""}
                            accessToken={selectedAdminAccessToken}
                            embedded
                            showHeader={false}
                            disableLocalSidebar
                            externalizeReportRequestControls
                            onReportRequestControlsRender={
                              setReportRequestControls
                            }
                            externalizeReportHeader
                            onReportHeaderRender={setReportHeader}
                          />
                        ) : (
                          <div className="flex h-full flex-col items-center justify-center gap-2 text-sm text-muted-foreground">
                            {sessions.length === 0 ? (
                              <>
                                <p>セッションがありません。</p>
                                <Button onClick={openCreateModal}>
                                  <Plus className="h-4 w-4" />
                                  新規セッションを作成
                                </Button>
                              </>
                            ) : (
                              <p>
                                管理権限のあるセッションを選択してください。
                              </p>
                            )}
                          </div>
                        )}
                        {detailError && hasSelectedSession && (
                          <div className="mt-4 rounded-lg border border-amber-300 bg-amber-50 px-4 py-3 text-sm text-amber-700">
                            {detailError}
                          </div>
                        )}
                      </CardContent>
                    </Card>
                  </div>
                </div>
                <div className="border-t bg-card px-6 py-4">
                  {reportRequestControls ? (
                    <div className="space-y-2">
                      <h4 className="text-sm font-semibold text-card-foreground">
                        レポートへのリクエスト
                      </h4>
                      <p className="text-xs text-muted-foreground">
                        追加で伝えたい条件があれば、ここに入力してください。
                      </p>
                      {reportRequestControls}
                    </div>
                  ) : (
                    <p className="text-xs text-muted-foreground">
                      レポートが生成されると、ここから追記リクエストを入力できます。
                    </p>
                  )}
                </div>
              </div>

              <aside className="w-80 bg-muted/20">
                <div className="flex h-full flex-col overflow-y-auto px-6 py-6">
                  <div className="space-y-4">
                    <Card>
                      <CardHeader className="pb-2">
                        <CardTitle className="text-sm font-semibold">
                          参加用リンク
                        </CardTitle>
                        <CardDescription className="text-xs">
                          招待URLとQRコードをまとめて共有できます。
                        </CardDescription>
                      </CardHeader>
                      <CardContent className="space-y-3">
                        {selectedSessionShareLink ? (
                          <>
                            <div className="rounded-md border border-border/70 bg-muted/40 px-3 py-2 text-xs text-muted-foreground">
                              <span className="break-all">
                                {selectedSessionShareLink}
                              </span>
                            </div>
                            <div className="flex flex-wrap gap-2">
                              <Button
                                variant="outline"
                                size="sm"
                                onClick={() => {
                                  void handleCopyShareLink();
                                }}
                              >
                                <Copy className="h-3.5 w-3.5" />
                                {copyStatus === "copied"
                                  ? "コピーしました"
                                  : copyStatus === "error"
                                    ? "コピーできません"
                                    : "リンクをコピー"}
                              </Button>
                              <Button
                                variant="ghost"
                                size="sm"
                                onClick={() =>
                                  window.open(
                                    selectedSessionShareLink,
                                    "_blank",
                                    "noreferrer",
                                  )
                                }
                              >
                                <ExternalLink className="h-3.5 w-3.5" />
                                新しいタブで開く
                              </Button>
                              <Button
                                variant="ghost"
                                size="sm"
                                onClick={() => setIsShareQrFullscreen(true)}
                                disabled={!shareQrSrc || isShareQrErrored}
                              >
                                <Maximize2 className="h-3.5 w-3.5" />
                                QRを拡大
                              </Button>
                            </div>
                            {shareQrSrc && !isShareQrErrored ? (
                              <div className="flex justify-center">
                                <Image
                                  src={shareQrSrc}
                                  alt="参加用QRコード"
                                  width={SHARE_QR_SIZE}
                                  height={SHARE_QR_SIZE}
                                  className="rounded-lg border border-border/60 bg-white p-2"
                                  onError={() => setIsShareQrErrored(true)}
                                  onLoadingComplete={() =>
                                    setIsShareQrErrored(false)
                                  }
                                />
                              </div>
                            ) : shareQrSrc ? (
                              <p className="text-[11px] text-muted-foreground">
                                QRコードを生成できませんでした。
                              </p>
                            ) : null}
                          </>
                        ) : (
                          <p className="text-xs text-muted-foreground">
                            管理権限のあるセッションを選択すると、共有リンクが表示されます。
                          </p>
                        )}
                      </CardContent>
                    </Card>

                    <Card>
                      <CardHeader className="pb-2">
                        <CardTitle className="text-sm font-semibold">
                          モニタリング
                        </CardTitle>
                        <CardDescription className="text-xs">
                          参加者ごとの回答率をトラッキングできます。
                        </CardDescription>
                      </CardHeader>
                      <CardContent className="space-y-3">
                        {detailLoading && hasSelectedSession ? (
                          <div className="flex items-center justify-center py-6">
                            <Loader2 className="h-5 w-5 animate-spin text-muted-foreground" />
                          </div>
                        ) : participantProgress.length > 0 ? (
                          participantProgress.slice(0, 5).map((participant) => (
                            <div
                              key={participant.userId}
                              className="rounded-md border border-border/50 bg-background px-3 py-3 text-xs"
                            >
                              <div className="flex items-center justify-between">
                                <span className="font-semibold text-foreground">
                                  {participant.name || "匿名ユーザー"}
                                </span>
                                <span className="rounded-full bg-muted px-2 py-0.5 text-[10px] font-semibold text-muted-foreground">
                                  {formatPercent(participant.completionRate)}
                                </span>
                              </div>
                              <div className="mt-2 text-[11px] text-muted-foreground">
                                回答 {participant.answeredCount} /{" "}
                                {participant.totalStatements} 件・更新{" "}
                                {formatRelativeTime(participant.updatedAt)}
                              </div>
                            </div>
                          ))
                        ) : (
                          <p className="text-xs text-muted-foreground">
                            まだ参加者がいないか、回答が記録されていません。
                          </p>
                        )}
                      </CardContent>
                    </Card>

                    <Card>
                      <CardHeader className="pb-2">
                        <CardTitle className="text-sm font-semibold">
                          ステートメントのハイライト
                        </CardTitle>
                        <CardDescription className="text-xs">
                          回答が集まっているテーマを把握し、次の打ち手を検討します。
                        </CardDescription>
                      </CardHeader>
                      <CardContent className="space-y-3">
                        {detailLoading && hasSelectedSession ? (
                          <div className="flex items-center justify-center py-6">
                            <Loader2 className="h-5 w-5 animate-spin text-muted-foreground" />
                          </div>
                        ) : statementHighlights.length > 0 ? (
                          statementHighlights.map((highlight) => (
                            <div
                              key={highlight.id}
                              className="space-y-2 rounded-md border border-border/50 bg-background px-3 py-3"
                            >
                              <p className="text-xs font-semibold text-muted-foreground">
                                回答 {highlight.totalResponses} 件 / スコア{" "}
                                {highlight.agreementScore}
                              </p>
                              <p className="text-sm leading-relaxed text-foreground">
                                {highlight.text}
                              </p>
                              <div className="flex items-center gap-2 text-[11px] font-semibold">
                                <span className="rounded-full bg-emerald-100 px-2 py-0.5 text-emerald-700">
                                  YES {formatPercent(highlight.positiveShare)}
                                </span>
                                <span className="rounded-full bg-rose-100 px-2 py-0.5 text-rose-700">
                                  NO {formatPercent(highlight.negativeShare)}
                                </span>
                              </div>
                            </div>
                          ))
                        ) : (
                          <p className="text-xs text-muted-foreground">
                            十分な回答がまだ集まっていません。
                          </p>
                        )}
                      </CardContent>
                    </Card>

                    <Card>
                      <CardHeader className="pb-2">
                        <CardTitle className="text-sm font-semibold">
                          進行ログ
                        </CardTitle>
                        <CardDescription className="text-xs">
                          Cartographer エージェントの動作履歴を確認します。
                        </CardDescription>
                      </CardHeader>
                      <CardContent className="space-y-3">
                        {eventLogLoading && hasSelectedSession ? (
                          <div className="flex items-center justify-center py-6">
                            <Loader2 className="h-5 w-5 animate-spin text-muted-foreground" />
                          </div>
                        ) : eventLogError ? (
                          <p className="text-xs text-amber-600">
                            {eventLogError}
                          </p>
                        ) : latestEvents.length > 0 ? (
                          latestEvents.map((event) => {
                            const label =
                              EVENT_TYPE_LABELS[event.type] ?? "イベント";
                            return (
                              <div
                                key={event.id}
                                className="rounded-md border border-border/50 bg-background px-3 py-3 text-xs"
                              >
                                <div className="flex items-center justify-between">
                                  <span className="inline-flex items-center gap-1 font-semibold text-foreground">
                                    <MessageSquare className="h-3.5 w-3.5 text-muted-foreground" />
                                    {label}
                                  </span>
                                  <span className="text-muted-foreground">
                                    {formatRelativeTime(event.createdAt)}
                                  </span>
                                </div>
                                {event.statements.length > 0 && (
                                  <ul className="mt-2 space-y-1 text-[11px] text-muted-foreground">
                                    {event.statements
                                      .slice(0, 2)
                                      .map((statement) => (
                                        <li
                                          key={statement.id}
                                          className="line-clamp-2"
                                        >
                                          ・{statement.text}
                                        </li>
                                      ))}
                                  </ul>
                                )}
                              </div>
                            );
                          })
                        ) : (
                          <p className="text-xs text-muted-foreground">
                            まだ記録されたイベントはありません。
                          </p>
                        )}
                      </CardContent>
                    </Card>

                    <Card>
                      <CardHeader className="pb-2">
                        <CardTitle className="text-sm font-semibold">
                          セッション情報
                        </CardTitle>
                        <CardDescription className="text-xs">
                          目的や背景、公開設定を確認できます。
                        </CardDescription>
                      </CardHeader>
                      <CardContent className="space-y-3 text-xs text-muted-foreground">
                        {selectedAdminSession ? (
                          <>
                            <div className="flex items-center justify-between">
                              <span>公開状態</span>
                              <VisibilityBadge
                                isPublic={selectedAdminSession.isPublic}
                              />
                            </div>
                            <div>
                              <p className="font-semibold text-foreground">
                                ゴール
                              </p>
                              <p className="mt-1 whitespace-pre-wrap">
                                {selectedAdminSession.goal || "未設定です。"}
                              </p>
                            </div>
                            <div>
                              <p className="font-semibold text-foreground">
                                コンテキスト
                              </p>
                              <p className="mt-1 whitespace-pre-wrap">
                                {selectedAdminSession.context || "未設定です。"}
                              </p>
                            </div>
                          </>
                        ) : (
                          <p>管理セッションを選択すると情報が表示されます。</p>
                        )}
                      </CardContent>
                    </Card>

                    <Card>
                      <CardHeader className="pb-2">
                        <CardTitle className="text-sm font-semibold">
                          進行設定
                        </CardTitle>
                        <CardDescription className="text-xs">
                          セッション運営に関する操作を行えます。
                        </CardDescription>
                      </CardHeader>
                      <CardContent className="space-y-3 text-xs text-muted-foreground">
                        {selectedAdminSession ? (
                          <>
                            <p>
                              セッションの詳細管理や削除などの操作を実行できます。
                            </p>
                            <div className="flex flex-col gap-2 pt-1">
                              <Button
                                variant="outline"
                                size="sm"
                                onClick={() =>
                                  router.push(
                                    `/sessions/${selectedAdminSession.id}`,
                                  )
                                }
                              >
                                <ExternalLink className="h-3.5 w-3.5" />
                                管理ビューを開く
                              </Button>
                              <Button
                                variant="destructive"
                                size="sm"
                                onClick={() => {
                                  void handleDeleteSession();
                                }}
                                disabled={
                                  deletingSessionId === selectedAdminSession.id
                                }
                                isLoading={
                                  deletingSessionId === selectedAdminSession.id
                                }
                              >
                                <Trash2 className="h-3.5 w-3.5" />
                                セッションを削除
                              </Button>
                            </div>
                          </>
                        ) : (
                          <p>
                            管理セッションを選択すると操作メニューが表示されます。
                          </p>
                        )}
                      </CardContent>
                    </Card>
                  </div>
                </div>
              </aside>
            </div>
          </main>
        </div>
      </div>

      {isShareQrFullscreen && shareQrFullscreenSrc && (
        <div className="fixed inset-0 z-50 flex items-center justify-center">
          <button
            type="button"
            aria-label="QR表示を閉じる"
            className="absolute inset-0 h-full w-full bg-slate-950/70"
            onClick={() => setIsShareQrFullscreen(false)}
          />
          <div className="relative z-10 flex flex-col items-center gap-4 rounded-3xl border border-border/70 bg-white px-6 py-6 shadow-2xl">
            <div className="flex w-full items-center justify-between">
              <span className="text-sm font-semibold text-foreground">
                参加用QRコード
              </span>
              <Button
                variant="ghost"
                size="icon"
                onClick={() => setIsShareQrFullscreen(false)}
              >
                <X className="h-4 w-4" />
              </Button>
            </div>
            <Image
              src={shareQrFullscreenSrc}
              alt="参加用QRコード（拡大表示）"
              width={FULLSCREEN_QR_SIZE}
              height={FULLSCREEN_QR_SIZE}
              className="max-h-[70vh] max-w-[70vw] rounded-3xl border border-border/80 bg-white p-6"
              onError={() => {
                setIsShareQrErrored(true);
                setIsShareQrFullscreen(false);
              }}
            />
            <p className="text-xs text-muted-foreground">
              スマートフォンでスキャンしてセッションに参加できます。
            </p>
          </div>
        </div>
      )}

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
