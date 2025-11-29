"use client";

import axios from "axios";
import {
  Bot,
  Check,
  ChevronDown,
  ChevronUp,
  Copy,
  ExternalLink,
  FileText,
  Loader2,
  Maximize2,
  Pause,
  Play,
  Printer,
  Send,
  Trash2,
  X,
} from "lucide-react";
import Image from "next/image";
import {
  type ReactElement,
  use,
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";

import { Button } from "@/components/ui/Button";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import { useUserId } from "@/lib/useUserId";

type ThreadEventType = "plan" | "survey" | "survey_analysis" | "user_message";

interface StatementResponseStats {
  strongYes: number;
  yes: number;
  dontKnow: number;
  no: number;
  strongNo: number;
  totalCount: number;
  freeTextCount: number;
  freeTextSamples: Array<{ participantUserId: string | null; text: string }>;
}

interface StatementWithStats {
  id: string;
  sessionId: string;
  text: string;
  orderIndex: number;
  responses: StatementResponseStats;
  agreementScore: number;
}

interface ParticipantProgress {
  userId: string;
  name: string;
  answeredCount: number;
  completionRate: number;
  totalStatements: number;
  updatedAt: string;
}

interface SessionAdminData {
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
}

interface TimelineStatement {
  id: string;
  text: string;
  orderIndex: number;
}

interface TimelineEvent {
  id: string;
  type: ThreadEventType;
  agentId: string | null;
  userId: string | null;
  progress: number | string | null;
  payload: Record<string, unknown>;
  orderIndex: number;
  createdAt: string;
  updatedAt: string;
  statements: TimelineStatement[];
}

interface EventThreadSummary {
  id: string;
  shouldProceed: boolean;
  createdAt: string;
  updatedAt: string;
}

interface EventThreadResponse {
  session: {
    id: string;
    title: string;
    context: string;
    goal: string;
    isPublic: boolean;
  };
  thread: EventThreadSummary;
  events: TimelineEvent[];
  agents: unknown[];
}

type SessionReportStatus = "pending" | "generating" | "completed" | "failed";

interface SessionReport {
  id: string;
  sessionId: string;
  version: number;
  status: SessionReportStatus;
  requestMarkdown: string;
  contentMarkdown: string | null;
  createdBy: string;
  model: string;
  errorMessage: string | null;
  createdAt: string;
  updatedAt: string;
  completedAt: string | null;
}

const REPORT_STATUS_META: Record<
  SessionReportStatus,
  { label: string; dot: string; text: string }
> = {
  pending: {
    label: "待機中",
    dot: "bg-amber-500",
    text: "text-amber-600",
  },
  generating: {
    label: "生成中",
    dot: "bg-sky-500",
    text: "text-sky-600",
  },
  completed: {
    label: "完了",
    dot: "bg-emerald-500",
    text: "text-emerald-600",
  },
  failed: {
    label: "失敗",
    dot: "bg-rose-500",
    text: "text-rose-600",
  },
};

const EVENT_TYPE_META: Record<
  ThreadEventType,
  { label: string; accent: string; badge: string }
> = {
  plan: {
    label: "Plan",
    accent: "text-sky-600",
    badge: "bg-sky-50 text-sky-700 border-sky-200",
  },
  survey: {
    label: "Survey",
    accent: "text-emerald-600",
    badge: "bg-emerald-50 text-emerald-700 border-emerald-200",
  },
  survey_analysis: {
    label: "Analysis",
    accent: "text-purple-600",
    badge: "bg-purple-50 text-purple-700 border-purple-200",
  },
  user_message: {
    label: "You",
    accent: "text-indigo-600",
    badge: "bg-indigo-50 text-indigo-700 border-indigo-200",
  },
};

const SHARE_QR_SIZE = 176;
const FULLSCREEN_QR_SIZE = 768;

const formatDateTime = (value: string) => {
  const date = new Date(value);
  const now = new Date();
  const diff = now.getTime() - date.getTime();
  const minutes = Math.floor(diff / 60000);
  const hours = Math.floor(diff / 3600000);
  const days = Math.floor(diff / 86400000);

  if (minutes < 1) return "たった今";
  if (minutes < 60) return `${minutes}分前`;
  if (hours < 24) return `${hours}時間前`;
  if (days < 7) return `${days}日前`;

  return date.toLocaleString("ja-JP", {
    month: "short",
    day: "numeric",
    hour: "2-digit",
    minute: "2-digit",
    hour12: false,
  });
};

const truncateText = (text: string, maxLength: number) => {
  if (text.length <= maxLength) return text;
  return `${text.substring(0, maxLength)}…`;
};

const formatPercentage = (value: number) => {
  if (Number.isNaN(value)) return "0%";
  const rounded = Math.round(value * 10) / 10;
  if (Math.abs(rounded - Math.round(rounded)) < 0.05) {
    return `${Math.round(rounded)}%`;
  }
  return `${rounded.toFixed(1)}%`;
};

export default function AdminPage({
  params,
}: {
  params: Promise<{ sessionId: string; accessToken: string }>;
}) {
  const { sessionId, accessToken } = use(params);
  const { userId, isLoading: isUserIdLoading } = useUserId();

  const [data, setData] = useState<SessionAdminData | null>(null);
  const [canEdit, setCanEdit] = useState(false);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [deleting, setDeleting] = useState(false);

  const [threadData, setThreadData] = useState<EventThreadResponse | null>(
    null,
  );
  const [threadLoading, setThreadLoading] = useState(true);
  const [threadError, setThreadError] = useState<string | null>(null);

  const [isEditingSettings, setIsEditingSettings] = useState(false);
  const [editingTitle, setEditingTitle] = useState("");
  const [editingContext, setEditingContext] = useState("");
  const [editingGoal, setEditingGoal] = useState("");
  const [editingVisibility, setEditingVisibility] = useState<
    "public" | "private"
  >("public");
  const [isSavingSettings, setIsSavingSettings] = useState(false);
  const [settingsMessage, setSettingsMessage] = useState<string | null>(null);
  const [settingsError, setSettingsError] = useState<string | null>(null);

  const [messageDraft, setMessageDraft] = useState("");
  const [sendingMessage, setSendingMessage] = useState(false);
  const [togglingProceed, setTogglingProceed] = useState(false);
  const [expandedEvents, setExpandedEvents] = useState<Record<string, boolean>>(
    {},
  );
  const [shareUrl, setShareUrl] = useState("");
  const [copyStatus, setCopyStatus] = useState<"idle" | "copied" | "error">(
    "idle",
  );
  const [isShareQrFullscreen, setIsShareQrFullscreen] = useState(false);
  const threadContainerRef = useRef<HTMLDivElement | null>(null);
  const lastThreadEventIdRef = useRef<string | null>(null);
  const [reports, setReports] = useState<SessionReport[]>([]);
  const [reportsLoading, setReportsLoading] = useState(true);
  const [reportsError, setReportsError] = useState<string | null>(null);
  const [selectedReportId, setSelectedReportId] = useState<string | null>(null);
  const [reportRequest, setReportRequest] = useState("");
  const [creatingReport, setCreatingReport] = useState(false);
  const [reportCopyStatus, setReportCopyStatus] = useState<
    "idle" | "copied" | "error"
  >("idle");

  const [isGeneratingQuestion, setIsGeneratingQuestion] = useState(false);

  const fetchAdminData = useCallback(async () => {
    if (!userId) return;

    try {
      setLoading(true);
      const response = await axios.get(
        `/api/sessions/${sessionId}/${accessToken}`,
        {
          headers: { Authorization: `Bearer ${userId}` },
        },
      );
      const responseData = response.data.data as SessionAdminData & {
        canEdit?: boolean;
      };
      setData({
        ...responseData,
        goal: responseData.goal ?? "",
        context: responseData.context ?? "",
        statements: responseData.statements ?? [],
        participants: responseData.participants ?? [],
        totalStatements:
          typeof responseData.totalStatements === "number"
            ? responseData.totalStatements
            : (responseData.statements?.length ?? 0),
        totalParticipants:
          typeof responseData.totalParticipants === "number"
            ? responseData.totalParticipants
            : (responseData.participants?.length ?? 0),
      });
      setCanEdit(responseData.canEdit ?? false);
      setError(null);
    } catch (err: unknown) {
      console.error("Failed to fetch admin data:", err);
      if (axios.isAxiosError(err) && err.response?.status === 403) {
        setError("このセッションへのアクセス権限がありません。");
      } else {
        setError("データの取得に失敗しました。");
      }
    } finally {
      setLoading(false);
    }
  }, [sessionId, accessToken, userId]);

  const fetchEventThread = useCallback(
    async (withSpinner = false) => {
      if (!userId) return;
      if (withSpinner) {
        setThreadLoading(true);
      }
      try {
        const response = await axios.get(
          `/api/sessions/${sessionId}/${accessToken}/event-thread`,
          {
            headers: { Authorization: `Bearer ${userId}` },
          },
        );
        setThreadData(response.data);
        setThreadError(null);
      } catch (err) {
        console.error("Failed to fetch event thread:", err);
        setThreadError("Event Threadの取得に失敗しました。");
      } finally {
        setThreadLoading(false);
      }
    },
    [sessionId, accessToken, userId],
  );

  const fetchReports = useCallback(
    async (withSpinner = false) => {
      if (!userId) return;
      if (withSpinner) {
        setReportsLoading(true);
      }
      try {
        const response = await axios.get(
          `/api/sessions/${sessionId}/${accessToken}/reports`,
          {
            headers: { Authorization: `Bearer ${userId}` },
          },
        );
        const list = (response.data.data ?? []) as SessionReport[];
        setReports(list);
        setReportsError(null);
        setSelectedReportId((current) => {
          if (list.length === 0) {
            return null;
          }
          if (current && list.some((report) => report.id === current)) {
            return current;
          }
          return list[0].id;
        });
      } catch (err) {
        console.error("Failed to fetch session reports:", err);
        setReportsError("レポート一覧の取得に失敗しました。");
      } finally {
        setReportsLoading(false);
      }
    },
    [sessionId, accessToken, userId],
  );

  useEffect(() => {
    if (isUserIdLoading) return;
    void fetchAdminData();
  }, [fetchAdminData, isUserIdLoading]);

  useEffect(() => {
    if (isUserIdLoading || !userId) return;
    void fetchEventThread(true);
    const intervalId = window.setInterval(() => {
      void fetchEventThread();
    }, 6000);
    return () => window.clearInterval(intervalId);
  }, [fetchEventThread, isUserIdLoading, userId]);

  useEffect(() => {
    if (isUserIdLoading || !userId) return;
    void fetchReports(true);
  }, [fetchReports, isUserIdLoading, userId]);

  const hasActiveReport = useMemo(
    () =>
      reports.some(
        (report) =>
          report.status === "pending" || report.status === "generating",
      ),
    [reports],
  );

  useEffect(() => {
    if (!userId || !hasActiveReport) return;
    const intervalId = window.setInterval(() => {
      void fetchReports();
    }, 5000);
    return () => window.clearInterval(intervalId);
  }, [fetchReports, hasActiveReport, userId]);

  const selectedReport = useMemo(
    () => reports.find((report) => report.id === selectedReportId) ?? null,
    [reports, selectedReportId],
  );

  const latestReport = reports[0] ?? null;
  const isViewingLatestReport =
    selectedReport !== null &&
    latestReport !== null &&
    selectedReport.id === latestReport.id;

  useEffect(() => {
    if (data) {
      setEditingTitle(data.title);
      setEditingContext(data.context);
      setEditingGoal(data.goal);
      setEditingVisibility(data.isPublic ? "public" : "private");
    }
  }, [data]);

  useEffect(() => {
    if (threadData?.events) {
      setExpandedEvents((prev) => {
        const next = { ...prev };
        threadData.events.forEach((event) => {
          if (typeof next[event.id] === "undefined") {
            next[event.id] = false;
          }
        });
        return next;
      });
    }
  }, [threadData]);

  useEffect(() => {
    const events = threadData?.events ?? [];
    if (!events.length) return;
    const lastEventId = events[events.length - 1]?.id;
    if (!lastEventId) return;
    const isInitial = lastThreadEventIdRef.current === null;
    if (lastThreadEventIdRef.current !== lastEventId) {
      lastThreadEventIdRef.current = lastEventId;
      const container = threadContainerRef.current;
      if (!container) return;
      window.requestAnimationFrame(() => {
        container.scrollTo({
          top: container.scrollHeight,
          behavior: isInitial ? "auto" : "smooth",
        });
      });
    }
  }, [threadData?.events]);

  useEffect(() => {
    if (data?.title) {
      document.title = `${data.title} - 管理画面 - 倍速会議`;
    }
    return () => {
      document.title = "倍速会議";
    };
  }, [data]);

  useEffect(() => {
    if (typeof window === "undefined") return;
    setShareUrl(`${window.location.origin}/sessions/${sessionId}`);
  }, [sessionId]);

  useEffect(() => {
    if (!isShareQrFullscreen || typeof document === "undefined") return;
    const { style } = document.body;
    const previousOverflow = style.overflow;
    style.overflow = "hidden";
    return () => {
      style.overflow = previousOverflow;
    };
  }, [isShareQrFullscreen]);

  useEffect(() => {
    if (!isShareQrFullscreen) return;
    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.key === "Escape") {
        setIsShareQrFullscreen(false);
      }
    };
    window.addEventListener("keydown", handleKeyDown);
    return () => window.removeEventListener("keydown", handleKeyDown);
  }, [isShareQrFullscreen]);

  const shareQrUrl = shareUrl
    ? `https://api.qrserver.com/v1/create-qr-code/?size=${SHARE_QR_SIZE}x${SHARE_QR_SIZE}&data=${encodeURIComponent(
        shareUrl,
      )}`
    : null;
  const fullscreenQrUrl = shareUrl
    ? `https://api.qrserver.com/v1/create-qr-code/?size=${FULLSCREEN_QR_SIZE}x${FULLSCREEN_QR_SIZE}&data=${encodeURIComponent(
        shareUrl,
      )}`
    : null;

  const handleSaveSettings = async (event: React.FormEvent) => {
    event.preventDefault();
    if (!userId || !canEdit) return;

    setIsSavingSettings(true);
    setSettingsMessage(null);
    setSettingsError(null);

    try {
      const response = await axios.patch(
        `/api/sessions/${sessionId}/${accessToken}`,
        {
          title: editingTitle,
          context: editingContext,
          goal: editingGoal,
          isPublic: editingVisibility === "public",
        },
        {
          headers: { Authorization: `Bearer ${userId}` },
        },
      );

      const updated = response.data.data as {
        title: string;
        context: string;
        goal: string;
        isPublic: boolean;
      };

      setData((prev) =>
        prev
          ? {
              ...prev,
              title: updated.title,
              context: updated.context,
              goal: updated.goal,
              isPublic: updated.isPublic,
            }
          : prev,
      );
      setSettingsMessage("セッション情報を更新しました。");
      setIsEditingSettings(false);
    } catch (err) {
      console.error("Failed to update session settings:", err);
      setSettingsError("セッション情報の更新に失敗しました。");
    } finally {
      setIsSavingSettings(false);
    }
  };

  const handleSendMessage = async () => {
    if (!userId || !canEdit || messageDraft.trim().length === 0) return;
    setSendingMessage(true);
    try {
      await axios.post(
        `/api/sessions/${sessionId}/event-thread/events/user-message`,
        { markdown: messageDraft },
        {
          headers: { Authorization: `Bearer ${userId}` },
        },
      );
      setMessageDraft("");
      await fetchEventThread();
    } catch (err) {
      console.error("Failed to send message:", err);
      alert("メッセージの送信に失敗しました。");
    } finally {
      setSendingMessage(false);
    }
  };

  const handleToggleShouldProceed = async () => {
    if (!userId || !canEdit || !threadData?.thread) return;
    setTogglingProceed(true);
    try {
      const response = await axios.patch(
        `/api/sessions/${sessionId}/event-thread/should-proceed`,
        { shouldProceed: !threadData.thread.shouldProceed },
        {
          headers: { Authorization: `Bearer ${userId}` },
        },
      );
      const updatedThread = response.data.thread as {
        id: string;
        shouldProceed: boolean;
        createdAt: string;
        updatedAt: string;
      };
      setThreadData((prev) =>
        prev
          ? {
              ...prev,
              thread: updatedThread,
            }
          : prev,
      );
    } catch (err) {
      console.error("Failed to toggle shouldProceed:", err);
      alert("自動生成の切り替えに失敗しました。");
    } finally {
      setTogglingProceed(false);
    }
  };

  const handleDeleteSession = async () => {
    if (!canEdit) return;

    if (
      !confirm("このセッションを完全に削除しますか？この操作は取り消せません。")
    ) {
      return;
    }

    try {
      setDeleting(true);
      await axios.delete(`/api/sessions/${sessionId}/${accessToken}`, {
        headers: { Authorization: `Bearer ${userId}` },
      });
      alert("セッションを削除しました。");
      window.location.href = "/";
    } catch (err) {
      console.error("Failed to delete session:", err);
      alert("セッションの削除に失敗しました。");
    } finally {
      setDeleting(false);
    }
  };

  const handleCopyLink = async () => {
    if (!shareUrl) return;
    try {
      await navigator.clipboard.writeText(shareUrl);
      setCopyStatus("copied");
      window.setTimeout(() => setCopyStatus("idle"), 2000);
    } catch (err) {
      console.error("Failed to copy link:", err);
      setCopyStatus("error");
      window.setTimeout(() => setCopyStatus("idle"), 2000);
    }
  };

  const handleCreateReport = async (
    event: React.FormEvent<HTMLFormElement>,
  ) => {
    event.preventDefault();
    if (!userId) return;
    if (creatingReport) return;

    try {
      setCreatingReport(true);
      const response = await axios.post(
        `/api/sessions/${sessionId}/${accessToken}/reports`,
        {
          requestMarkdown: reportRequest,
        },
        {
          headers: { Authorization: `Bearer ${userId}` },
        },
      );

      const created = response.data.data as SessionReport;
      setReports((prev) => {
        const others = prev.filter((report) => report.id !== created.id);
        return [created, ...others];
      });
      setSelectedReportId(created.id);
      setReportRequest("");
      setReportsError(null);
    } catch (err) {
      console.error("Failed to start session report generation:", err);
      setReportsError("レポート生成の開始に失敗しました。");
    } finally {
      setCreatingReport(false);
    }
  };

  const handleCopyReportMarkdown = async () => {
    if (!selectedReport?.contentMarkdown) return;
    try {
      await navigator.clipboard.writeText(selectedReport.contentMarkdown);
      setReportCopyStatus("copied");
    } catch (err) {
      console.error("Failed to copy report markdown:", err);
      setReportCopyStatus("error");
    } finally {
      window.setTimeout(() => setReportCopyStatus("idle"), 2000);
    }
  };

  const handleGenerateQuestion = async () => {
    if (!userId || !canEdit || isGeneratingQuestion) return;

    try {
      setIsGeneratingQuestion(true);
      const response = await axios.post(
        `/api/sessions/${sessionId}/statements/generate`,
        {},
        {
          headers: { Authorization: `Bearer ${userId}` },
        },
      );

      const newStatements = response.data.statements;
      if (newStatements && newStatements.length > 0) {
        // Refresh data to show the new statements
        await fetchAdminData();
        alert(`${newStatements.length}問の質問を生成しました。`);
      }
    } catch (err) {
      console.error("Failed to generate questions:", err);
      alert("質問の生成に失敗しました。");
    } finally {
      setIsGeneratingQuestion(false);
    }
  };

  const participants = data?.participants ?? [];
  const totalParticipants =
    data?.totalParticipants ?? participants?.length ?? 0;
  const _totalStatements =
    data?.totalStatements ?? data?.statements?.length ?? 0;
  const statements = data?.statements ?? [];

  const participantSummary = useMemo(() => {
    if (participants.length === 0) {
      return {
        averageCompletion: 0,
        inProgressCount: 0,
        completedCount: 0,
        notStartedCount: 0,
      };
    }

    const averageCompletion =
      participants.reduce((sum, item) => sum + item.completionRate, 0) /
      participants.length;
    const completedCount = participants.filter(
      (participant) => participant.completionRate >= 99.9,
    ).length;
    const notStartedCount = participants.filter(
      (participant) => participant.answeredCount === 0,
    ).length;
    const inProgressCount =
      participants.length - completedCount - notStartedCount;

    return {
      averageCompletion,
      inProgressCount,
      completedCount,
      notStartedCount,
    };
  }, [participants]);

  const rankedParticipants = useMemo(() => {
    return [...participants].sort(
      (a, b) => b.completionRate - a.completionRate,
    );
  }, [participants]);

  const statementHighlights = useMemo(() => {
    if (!statements.length) {
      return {
        agreement: [],
        conflict: [],
        dontKnow: [],
      } as Record<
        "agreement" | "conflict" | "dontKnow",
        Array<StatementHighlight>
      >;
    }

    const enriched = statements.map<StatementHighlight>((statement) => {
      const positive = statement.responses.strongYes + statement.responses.yes;
      const negative = statement.responses.strongNo + statement.responses.no;
      const neutral = statement.responses.dontKnow;
      const totalForRate =
        statement.responses.totalCount + statement.responses.freeTextCount;
      const conflict = Math.min(positive, negative);
      const responseRate =
        totalParticipants > 0
          ? Math.round((totalForRate / totalParticipants) * 100 * 10) / 10
          : 0;
      return {
        statement,
        positive,
        negative,
        neutral,
        conflict,
        responseRate,
      };
    });

    const agreement = enriched
      .filter(
        (item) =>
          item.statement.responses.totalCount +
            item.statement.responses.freeTextCount >
          0,
      )
      .sort((a, b) => {
        if (b.positive === a.positive) {
          return (
            b.statement.responses.totalCount - a.statement.responses.totalCount
          );
        }
        return b.positive - a.positive;
      })
      .slice(0, 3);

    const conflict = enriched
      .filter(
        (item) =>
          item.statement.responses.totalCount +
            item.statement.responses.freeTextCount >
          0,
      )
      .sort((a, b) => {
        if (b.conflict === a.conflict) {
          return (
            b.statement.responses.totalCount - a.statement.responses.totalCount
          );
        }
        return b.conflict - a.conflict;
      })
      .slice(0, 3);

    const dontKnow = enriched
      .filter(
        (item) =>
          item.statement.responses.totalCount +
            item.statement.responses.freeTextCount >
          0,
      )
      .sort((a, b) => {
        if (b.neutral === a.neutral) {
          return (
            b.statement.responses.totalCount - a.statement.responses.totalCount
          );
        }
        return b.neutral - a.neutral;
      })
      .slice(0, 3);

    return { agreement, conflict, dontKnow };
  }, [statements, totalParticipants]);

  if (isUserIdLoading || loading) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-slate-50">
        <Loader2 className="h-8 w-8 animate-spin text-slate-400" />
      </div>
    );
  }

  if (error) {
    return (
      <div className="min-h-screen bg-slate-50">
        <div className="max-w-6xl mx-auto px-6 py-16">
          <Card className="border-red-200/70 bg-red-50/80">
            <CardContent className="pt-6">
              <p className="text-red-700">{error}</p>
            </CardContent>
          </Card>
        </div>
      </div>
    );
  }

  if (!data) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-slate-50">
        <p className="text-muted-foreground">セッションが見つかりません。</p>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-slate-50">
      <div className="max-w-[90rem] mx-auto px-6 py-10 space-y-10">
        <header className="space-y-4">
          <div className="flex flex-wrap items-start justify-between gap-4">
            <div className="space-y-2">
              <h1 className="text-3xl font-semibold text-slate-900">
                {data.title}
              </h1>
            </div>
          </div>
        </header>

        <div className="grid gap-8 lg:grid-cols-[minmax(0,2fr)_minmax(320px,1fr)]">
          <div className="space-y-8">
            <Card className="border-none bg-white/80 shadow-sm">
              <CardHeader className="pb-4">
                <CardTitle className="text-lg">参加者状況の把握</CardTitle>
                <CardDescription>
                  このセッションに参加している人の回答状況をリアルタイムに確認できます。更新する場合は、ページを再読み込みしてください。
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-6">
                <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-4">
                  <MonitoringMetric
                    label="参加者"
                    value={`${totalParticipants}人`}
                  />
                  <MonitoringMetric
                    label="平均回答率"
                    value={formatPercentage(
                      participantSummary.averageCompletion,
                    )}
                    tone="emerald"
                  />
                  <MonitoringMetric
                    label="回答済み"
                    value={`${participantSummary.completedCount}人`}
                  />
                  <MonitoringMetric
                    label="回答進行中"
                    value={`${
                      participantSummary.inProgressCount +
                      participantSummary.notStartedCount
                    }人`}
                  />
                </div>

                {participants.length === 0 ? (
                  <p className="text-sm text-slate-500">
                    まだ参加者はいません。リンクを共有して参加を促しましょう。
                  </p>
                ) : (
                  <div className="grid gap-2.5 sm:grid-cols-2 xl:grid-cols-3">
                    {rankedParticipants.map((participant) => (
                      <ParticipantProgressRow
                        key={participant.userId}
                        participant={participant}
                      />
                    ))}
                  </div>
                )}
              </CardContent>
            </Card>

            <Card className="border-none bg-white/80 shadow-sm">
              <CardHeader className="pb-4">
                <div className="flex flex-wrap items-start justify-between gap-4">
                  <div className="space-y-1.5">
                    <CardTitle className="text-lg">
                      セッションレポート
                    </CardTitle>
                    <CardDescription>
                      参加者の回答やEvent Threadをもとに洞察レポートを生成します
                    </CardDescription>
                  </div>
                  {selectedReport ? (
                    <div className="text-right">
                      <p className="text-[10px] uppercase tracking-[0.2em] text-slate-400">
                        最新の状態
                      </p>
                      <p className="text-sm font-semibold text-slate-900">
                        v{String(selectedReport.version).padStart(2, "0")}
                      </p>
                    </div>
                  ) : null}
                </div>
              </CardHeader>
              <CardContent className="space-y-6">
                {canEdit ? (
                  <form
                    onSubmit={handleCreateReport}
                    className="space-y-4 rounded-3xl border border-slate-200 bg-white/80 p-4 shadow-inner"
                  >
                    <Button
                      type="submit"
                      size="lg"
                      disabled={creatingReport}
                      isLoading={creatingReport}
                      className="w-full justify-center gap-2 rounded-2xl py-6 text-base shadow-lg shadow-slate-900/10"
                    >
                      <FileText className="h-4 w-4" />
                      新しいレポートを生成
                    </Button>
                    <label
                      htmlFor="reportRequest"
                      className="text-xs font-semibold uppercase tracking-[0.18em] text-slate-500"
                    >
                      レポートに対するリクエスト（任意）
                    </label>
                    <textarea
                      id="reportRequest"
                      value={reportRequest}
                      onChange={(event) => setReportRequest(event.target.value)}
                      rows={3}
                      maxLength={1200}
                      className="w-full resize-none rounded-2xl border border-slate-200 bg-white px-3 py-2 text-sm text-slate-800 placeholder:text-slate-400 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-slate-200"
                      placeholder="例:「共有している価値観について重点的に分析してほしい」「易しい言葉を使った分かりやすいレポートを出力してほしい」"
                    />
                  </form>
                ) : (
                  <div className="rounded-3xl border border-slate-200/70 bg-slate-50/80 px-4 py-3 text-xs text-slate-500">
                    レポート生成はセッションのホストのみ利用できます。
                  </div>
                )}

                {reportsError && (
                  <div className="rounded-2xl border border-amber-200 bg-amber-50 px-4 py-3 text-sm text-amber-700">
                    {reportsError}
                  </div>
                )}

                {reportsLoading ? (
                  <div className="space-y-4">
                    <div className="h-12 animate-pulse rounded-2xl bg-slate-100/80" />
                    <div className="h-[420px] animate-pulse rounded-3xl bg-slate-100/80" />
                  </div>
                ) : reports.length === 0 ? (
                  <div className="rounded-2xl border border-dashed border-slate-200/80 bg-white/70 px-4 py-6 text-center text-sm text-slate-500">
                    まだレポートはありません。上のフォームから生成してみましょう。
                  </div>
                ) : selectedReport ? (
                  <div className="space-y-6">
                    <div className="flex flex-col gap-4 lg:flex-row lg:items-center lg:justify-between">
                      <div className="space-y-2">
                        <label
                          htmlFor="reportVersionSelect"
                          className="text-xs font-semibold uppercase tracking-[0.18em] text-slate-500"
                        >
                          表示するバージョン
                        </label>
                        <div className="flex flex-wrap items-center gap-3">
                          <select
                            id="reportVersionSelect"
                            value={selectedReportId ?? ""}
                            onChange={(event) =>
                              setSelectedReportId(event.target.value)
                            }
                            className="max-w-xs rounded-2xl border border-slate-200 bg-white px-3 py-2 text-sm font-medium text-slate-800 shadow-sm focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-slate-200"
                          >
                            {reports.map((report) => {
                              const meta = REPORT_STATUS_META[report.status];
                              return (
                                <option key={report.id} value={report.id}>
                                  v{String(report.version).padStart(2, "0")}・
                                  {meta.label}
                                </option>
                              );
                            })}
                          </select>
                          <span className="inline-flex items-center gap-1 rounded-full border border-slate-200 px-3 py-1 text-[11px] font-medium text-slate-600">
                            <span
                              className={`h-2 w-2 rounded-full ${REPORT_STATUS_META[selectedReport.status].dot}`}
                            />
                            {REPORT_STATUS_META[selectedReport.status].label}
                          </span>
                          <span className="rounded-full bg-slate-100 px-3 py-1 text-[11px] text-slate-500">
                            {isViewingLatestReport
                              ? "最新バージョンを表示中"
                              : latestReport
                                ? `最新: v${String(latestReport.version).padStart(2, "0")}`
                                : "最新バージョン情報なし"}
                          </span>
                        </div>
                      </div>
                      <div className="flex flex-wrap gap-2">
                        <Button
                          type="button"
                          variant="outline"
                          size="sm"
                          disabled={
                            selectedReport.status !== "completed" ||
                            !selectedReport.contentMarkdown
                          }
                          onClick={handleCopyReportMarkdown}
                          className="gap-1.5 text-xs"
                        >
                          <Copy className="h-3.5 w-3.5" />
                          {reportCopyStatus === "copied"
                            ? "コピー済み"
                            : reportCopyStatus === "error"
                              ? "コピー失敗"
                              : "Markdownをコピー"}
                        </Button>
                        <Button
                          type="button"
                          variant="ghost"
                          size="sm"
                          className="gap-1.5 text-xs"
                          disabled={
                            selectedReport.status !== "completed" ||
                            !selectedReport.contentMarkdown
                          }
                          onClick={() =>
                            window.open(
                              `/sessions/${sessionId}/${accessToken}/reports/${selectedReport.id}/print`,
                              "_blank",
                            )
                          }
                        >
                          <Printer className="h-3.5 w-3.5" />
                          印刷用ページ
                        </Button>
                      </div>
                    </div>

                    {selectedReport.requestMarkdown ? (
                      <div className="rounded-2xl border border-indigo-100 bg-indigo-50/70 p-4 text-sm text-indigo-900">
                        <p className="text-[11px] font-semibold uppercase tracking-[0.18em] text-indigo-400">
                          Admin Request
                        </p>
                        <p className="mt-1 whitespace-pre-wrap leading-relaxed">
                          {selectedReport.requestMarkdown}
                        </p>
                      </div>
                    ) : null}

                    <div className="min-h-[360px] rounded-3xl border border-slate-200 bg-white/80 p-6 shadow-inner">
                      <div className="flex h-full flex-col gap-4">
                        <div className="flex flex-wrap items-center justify-between gap-3">
                          <div>
                            <p className="text-[10px] uppercase tracking-[0.2em] text-slate-400">
                              Version
                            </p>
                            <p className="text-xl font-semibold text-slate-900">
                              v{String(selectedReport.version).padStart(2, "0")}
                            </p>
                          </div>
                          <div className="inline-flex items-center gap-2 text-[11px] uppercase tracking-[0.2em] text-slate-400">
                            <div className="inline-flex items-center gap-1 rounded-full border border-slate-200 px-2 py-1 text-[10px] font-semibold uppercase tracking-[0.18em] text-slate-500">
                              {selectedReport.model}
                            </div>
                            <div className="inline-flex items-center gap-1 rounded-full border border-slate-200 px-2 py-1 text-[10px] font-semibold uppercase tracking-[0.18em]">
                              <span
                                className={`h-2 w-2 rounded-full ${REPORT_STATUS_META[selectedReport.status].dot}`}
                              />
                              {REPORT_STATUS_META[selectedReport.status].label}
                            </div>
                          </div>
                        </div>

                        <div className="flex-1 overflow-y-auto rounded-2xl border border-slate-200 bg-white/90 p-4">
                          {selectedReport.status === "completed" &&
                          selectedReport.contentMarkdown ? (
                            <div className="markdown-body prose prose-slate max-w-none text-sm leading-relaxed">
                              <ReactMarkdown remarkPlugins={[remarkGfm]}>
                                {selectedReport.contentMarkdown}
                              </ReactMarkdown>
                            </div>
                          ) : selectedReport.status === "failed" ? (
                            <div className="text-sm text-rose-600">
                              レポート生成に失敗しました。
                              <br />
                              {selectedReport.errorMessage ??
                                "詳細はログを確認してください。"}
                            </div>
                          ) : (
                            <div className="flex h-full flex-col items-center justify-center gap-3 text-sm text-slate-500">
                              <Loader2 className="h-5 w-5 animate-spin text-slate-400" />
                              <p>レポートを生成しています…</p>
                              <p className="text-[11px] text-slate-400">
                                完了まで数十秒ほどかかる場合があります。
                              </p>
                            </div>
                          )}
                        </div>

                        <div className="flex flex-wrap gap-4 text-[11px] uppercase tracking-[0.2em] text-slate-400">
                          <span>
                            作成: {formatDateTime(selectedReport.createdAt)}
                          </span>
                          {selectedReport.completedAt ? (
                            <span>
                              最終更新:{" "}
                              {formatDateTime(selectedReport.completedAt)}
                            </span>
                          ) : null}
                        </div>
                      </div>
                    </div>
                  </div>
                ) : (
                  <div className="rounded-2xl border border-dashed border-slate-200/80 bg-white/70 px-4 py-6 text-center text-sm text-slate-500">
                    レポートを選択するとここに表示されます。
                  </div>
                )}
              </CardContent>
            </Card>

            <Card className="border-none bg-white/80 shadow-sm">
              <CardHeader className="pb-4">
                <div className="flex items-center justify-between gap-4">
                  <div>
                    <CardTitle className="text-lg">意見の可視化</CardTitle>
                    <CardDescription>
                      参加者の回答をもとに、合意できている点、対立している点、みんなが分からない点を把握できます
                    </CardDescription>
                  </div>
                  {canEdit && (
                    <Button
                      variant="outline"
                      size="sm"
                      onClick={handleGenerateQuestion}
                      disabled={isGeneratingQuestion}
                      isLoading={isGeneratingQuestion}
                      className="gap-1.5 text-xs"
                    >
                      <Bot className="h-3.5 w-3.5" />
                      質問を生成
                    </Button>
                  )}
                </div>
              </CardHeader>

              <CardContent className="space-y-6">
                <div className="grid gap-6 lg:grid-cols-3">
                  <StatementHighlightColumn
                    title="合意度トップ3"
                    tone="emerald"
                    items={statementHighlights.agreement}
                  />
                  <StatementHighlightColumn
                    title="対立度トップ3"
                    tone="amber"
                    items={statementHighlights.conflict}
                  />
                  <StatementHighlightColumn
                    title="わからない度トップ3"
                    tone="slate"
                    items={statementHighlights.dontKnow}
                  />
                </div>
              </CardContent>
            </Card>

            <Card className="border-none bg-white/80 shadow-lg">
              <CardHeader className="pb-4">
                <div className="flex flex-wrap items-start justify-between gap-4">
                  <div>
                    <CardTitle className="text-lg">進行ログ</CardTitle>
                    <CardDescription>
                      ファシリテーターAIの進行状況をここから確認できます
                    </CardDescription>
                  </div>
                  <div className="flex items-center gap-3">
                    <ThreadStatusPill
                      shouldProceed={threadData?.thread?.shouldProceed ?? false}
                    />
                  </div>
                </div>
              </CardHeader>
              <CardContent className="space-y-6">
                <div className="relative overflow-hidden rounded-3xl border border-slate-200 bg-white/70 shadow-inner">
                  {threadLoading && (
                    <div className="absolute inset-x-0 top-0 z-10 bg-gradient-to-b from-white/90 to-white/30 py-2 text-center text-xs text-slate-500">
                      更新中…
                    </div>
                  )}
                  <div
                    ref={threadContainerRef}
                    className="h-[620px] overflow-y-auto px-6 py-6 space-y-5"
                  >
                    {threadError ? (
                      <div className="rounded-2xl border border-amber-200 bg-amber-50 px-4 py-3 text-sm text-amber-700">
                        {threadError}
                      </div>
                    ) : threadData?.events.length ? (
                      threadData.events.map((event) => {
                        const isHostMessage = event.type === "user_message";
                        const expanded = Boolean(expandedEvents[event.id]);
                        return (
                          <ThreadEventBubble
                            key={event.id}
                            event={event}
                            isHostMessage={isHostMessage}
                            expanded={expanded}
                            onToggle={() =>
                              setExpandedEvents((prev) => ({
                                ...prev,
                                [event.id]: !prev[event.id],
                              }))
                            }
                          />
                        );
                      })
                    ) : (
                      <p className="text-sm text-slate-500">
                        まだイベントはありません。Agentとの会話はここに表示されます。
                      </p>
                    )}
                  </div>
                </div>

                {canEdit && (
                  <div className="space-y-2 rounded-3xl border border-slate-200 bg-white/80 p-4 shadow-sm">
                    <label
                      htmlFor="adminMessage"
                      className="text-xs font-medium text-slate-600"
                    >
                      ファシリテーターAIへのメッセージ
                    </label>
                    <textarea
                      id="adminMessage"
                      value={messageDraft}
                      onChange={(event) => setMessageDraft(event.target.value)}
                      rows={3}
                      className="w-full rounded-2xl border border-slate-200 bg-white px-3 py-2 text-sm text-slate-800 placeholder:text-slate-400 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-slate-200 resize-none"
                      placeholder="ファシリテーターAIへ伝えたい情報や、与えたい指示を書き込めます。"
                    />
                    <div className="flex items-center justify-between">
                      <Button
                        type="button"
                        onClick={handleSendMessage}
                        disabled={
                          sendingMessage || messageDraft.trim().length === 0
                        }
                        isLoading={sendingMessage}
                        size="sm"
                        className="gap-1.5 text-xs"
                      >
                        <Send className="h-3.5 w-3.5" />
                        送信
                      </Button>
                    </div>
                  </div>
                )}
              </CardContent>
            </Card>
          </div>

          <div className="space-y-8">
            <Card className="border-none bg-white/80 shadow-sm">
              <CardHeader className="pb-4">
                <CardTitle className="text-lg">参加用リンク</CardTitle>
                <CardDescription>
                  共有リンクやQRコードから参加者を招待できます
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-5">
                <div className="space-y-2">
                  <label
                    htmlFor="shareLink"
                    className="text-xs font-medium text-slate-600"
                  >
                    コピー用URL
                  </label>
                  <div className="flex items-center gap-2">
                    <Input
                      id="shareLink"
                      readOnly
                      value={shareUrl}
                      className="text-sm"
                      onFocus={(event) => event.currentTarget.select()}
                    />
                    <Button
                      type="button"
                      variant="outline"
                      size="sm"
                      onClick={handleCopyLink}
                      className="gap-1.5 text-xs"
                    >
                      {copyStatus === "copied" ? (
                        <Check className="h-3.5 w-3.5 text-emerald-600" />
                      ) : (
                        <Copy className="h-3.5 w-3.5" />
                      )}
                      {copyStatus === "copied"
                        ? "コピー済み"
                        : copyStatus === "error"
                          ? "コピー失敗"
                          : "コピー"}
                    </Button>
                  </div>
                </div>
                <div className="relative rounded-2xl border border-slate-200/80 bg-gradient-to-br from-slate-50 to-white px-6 py-6 text-center shadow-inner">
                  {shareQrUrl && (
                    <Button
                      type="button"
                      variant="ghost"
                      size="sm"
                      onClick={() => setIsShareQrFullscreen(true)}
                      className="absolute right-4 top-4 gap-1.5 rounded-full border border-slate-200 bg-white/90 px-3 text-xs text-slate-700 shadow-sm hover:bg-white"
                    >
                      <Maximize2 className="h-3.5 w-3.5" />
                    </Button>
                  )}
                  {shareQrUrl ? (
                    <Image
                      src={shareQrUrl}
                      alt="参加用QRコード"
                      width={SHARE_QR_SIZE}
                      height={SHARE_QR_SIZE}
                      className="mx-auto h-[176px] w-[176px] rounded-xl border border-slate-200 bg-white object-contain p-2 shadow-sm"
                    />
                  ) : (
                    <div className="mx-auto flex h-[176px] w-[176px] items-center justify-center rounded-xl border border-dashed border-slate-300 bg-white text-xs text-slate-400">
                      QRコードを生成できませんでした
                    </div>
                  )}
                </div>
              </CardContent>
            </Card>
            {isShareQrFullscreen && fullscreenQrUrl && (
              <div className="fixed inset-0 z-50 m-0 flex items-center justify-center bg-slate-950/85 p-4 sm:p-10 backdrop-blur-sm relative">
                <button
                  type="button"
                  aria-label="全画面表示を閉じる"
                  className="absolute inset-0 z-0 h-full w-full cursor-pointer bg-transparent focus:outline-none"
                  onClick={() => setIsShareQrFullscreen(false)}
                  onKeyDown={(event) => {
                    if (event.key === "Escape") {
                      event.preventDefault();
                      setIsShareQrFullscreen(false);
                    }
                  }}
                />
                <div
                  className="relative z-10 flex w-full max-w-5xl flex-col items-center gap-6 text-center"
                  role="dialog"
                  aria-modal="true"
                  aria-label="参加用QRコードの全画面表示"
                >
                  <Button
                    type="button"
                    variant="ghost"
                    size="icon"
                    onClick={() => setIsShareQrFullscreen(false)}
                    className="absolute right-0 top-0 text-white hover:bg-white/10 focus-visible:ring-white"
                  >
                    <X className="h-5 w-5" />
                  </Button>
                  <div className="rounded-3xl border border-white/10 bg-white/5 p-4 shadow-2xl backdrop-blur">
                    <Image
                      src={fullscreenQrUrl}
                      alt="参加用QRコード"
                      width={FULLSCREEN_QR_SIZE}
                      height={FULLSCREEN_QR_SIZE}
                      className="h-auto w-full max-w-[min(95vw,880px)] rounded-2xl border border-white bg-white p-6 shadow-lg"
                    />
                  </div>
                  <div className="space-y-1">
                    <h2 className="text-3xl font-semibold text-white">
                      QRコードを携帯でスキャン
                    </h2>
                  </div>
                </div>
              </div>
            )}

            <Card className="border-none bg-white/80 shadow-sm">
              <CardHeader className="pb-4">
                <div className="flex items-start justify-between gap-4">
                  <div>
                    <CardTitle className="text-lg">セッション情報</CardTitle>
                    <CardDescription>
                      {canEdit
                        ? "基本情報を編集してアップデートできます"
                        : "セッションの基本情報"}
                    </CardDescription>
                  </div>
                  {!isEditingSettings && canEdit && (
                    <Button
                      variant="ghost"
                      size="sm"
                      onClick={() => setIsEditingSettings(true)}
                      className="gap-1.5 text-xs"
                    >
                      編集
                    </Button>
                  )}
                </div>
              </CardHeader>
              <CardContent className="space-y-5">
                {!isEditingSettings ? (
                  <div className="space-y-4 text-sm text-slate-600">
                    <div>
                      <p className="text-xs font-medium text-slate-500 uppercase tracking-[0.12em]">
                        公開設定
                      </p>
                      <p className="mt-1 text-slate-800 font-medium">
                        {data.isPublic ? "公開" : "非公開"}
                      </p>
                    </div>
                    <div>
                      <p className="text-xs font-medium text-slate-500 uppercase tracking-[0.12em]">
                        ゴール
                      </p>
                      <p
                        className="mt-1 leading-relaxed"
                        title={data.goal ?? undefined}
                      >
                        {data.goal ? truncateText(data.goal, 160) : "未設定"}
                      </p>
                    </div>
                    <div>
                      <p className="text-xs font-medium text-slate-500 uppercase tracking-[0.12em]">
                        背景情報
                      </p>
                      <p
                        className="mt-1 leading-relaxed whitespace-pre-wrap"
                        title={data.context ?? undefined}
                      >
                        {data.context
                          ? truncateText(data.context, 160)
                          : "未設定"}
                      </p>
                    </div>
                  </div>
                ) : (
                  <form onSubmit={handleSaveSettings} className="space-y-4">
                    <div className="space-y-1.5">
                      <label
                        htmlFor="sessionTitle"
                        className="text-xs font-medium text-slate-600"
                      >
                        タイトル
                      </label>
                      <Input
                        id="sessionTitle"
                        type="text"
                        value={editingTitle}
                        onChange={(event) =>
                          setEditingTitle(event.target.value)
                        }
                        required
                        className="text-sm"
                      />
                    </div>

                    <div className="space-y-1.5">
                      <span className="text-xs font-medium text-slate-600">
                        公開設定
                      </span>
                      <div className="grid grid-cols-2 gap-2">
                        <label className="flex flex-1 items-center gap-2 rounded-xl border border-slate-200 bg-slate-50/60 px-3 py-2 text-xs text-slate-700 shadow-sm">
                          <input
                            type="radio"
                            name="sessionVisibility"
                            value="public"
                            checked={editingVisibility === "public"}
                            onChange={() => setEditingVisibility("public")}
                          />
                          <span>公開</span>
                        </label>
                        <label className="flex flex-1 items-center gap-2 rounded-xl border border-slate-200 bg-slate-50/60 px-3 py-2 text-xs text-slate-700 shadow-sm">
                          <input
                            type="radio"
                            name="sessionVisibility"
                            value="private"
                            checked={editingVisibility === "private"}
                            onChange={() => setEditingVisibility("private")}
                          />
                          <span>非公開</span>
                        </label>
                      </div>
                    </div>

                    <div className="space-y-1.5">
                      <label
                        htmlFor="sessionGoal"
                        className="text-xs font-medium text-slate-600"
                      >
                        ゴール
                      </label>
                      <textarea
                        id="sessionGoal"
                        value={editingGoal}
                        onChange={(event) => setEditingGoal(event.target.value)}
                        required
                        rows={5}
                        className="flex w-full rounded-xl border border-slate-200 bg-white px-3 py-2 text-sm text-slate-800 shadow-sm placeholder:text-slate-400 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-slate-200 resize-none"
                      />
                    </div>

                    <div className="space-y-1.5">
                      <label
                        htmlFor="sessionContext"
                        className="text-xs font-medium text-slate-600"
                      >
                        背景情報
                      </label>
                      <textarea
                        id="sessionContext"
                        value={editingContext}
                        onChange={(event) =>
                          setEditingContext(event.target.value)
                        }
                        rows={5}
                        className="flex w-full rounded-xl border border-slate-200 bg-white px-3 py-2 text-sm text-slate-800 shadow-sm placeholder:text-slate-400 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-slate-200 resize-none"
                      />
                    </div>

                    {(settingsMessage || settingsError) && (
                      <div
                        className={`rounded-xl px-3 py-2 text-xs ${
                          settingsError
                            ? "bg-red-50 text-red-600"
                            : "bg-emerald-50 text-emerald-700"
                        }`}
                      >
                        {settingsError ?? settingsMessage}
                      </div>
                    )}

                    <div className="flex items-center gap-2">
                      <Button
                        type="submit"
                        disabled={isSavingSettings}
                        isLoading={isSavingSettings}
                        size="sm"
                        className="gap-1.5 text-xs"
                      >
                        保存
                      </Button>
                      <Button
                        type="button"
                        variant="ghost"
                        size="sm"
                        onClick={() => {
                          setIsEditingSettings(false);
                          setSettingsMessage(null);
                          setSettingsError(null);
                          if (data) {
                            setEditingTitle(data.title);
                            setEditingContext(data.context);
                            setEditingGoal(data.goal);
                            setEditingVisibility(
                              data.isPublic ? "public" : "private",
                            );
                          }
                        }}
                        className="gap-1.5 text-xs"
                      >
                        キャンセル
                      </Button>
                    </div>
                  </form>
                )}
              </CardContent>
            </Card>

            <Card className="border-none bg-white/80 shadow-sm">
              <CardHeader className="pb-4">
                <CardTitle className="text-lg">進行設定</CardTitle>
                <CardDescription>
                  自動質問生成の制御やセッションの管理を行えます
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-5">
                <button
                  type="button"
                  onClick={canEdit ? handleToggleShouldProceed : undefined}
                  disabled={togglingProceed || !canEdit}
                  aria-pressed={Boolean(threadData?.thread?.shouldProceed)}
                  className={`w-full rounded-2xl border px-4 py-3 text-left transition ${
                    threadData?.thread?.shouldProceed
                      ? "border-emerald-200 bg-emerald-50/70 hover:bg-emerald-50"
                      : "border-amber-200 bg-amber-50/60 hover:bg-amber-50"
                  } ${!canEdit ? "opacity-60 cursor-not-allowed" : ""}`}
                >
                  <div className="flex items-center justify-between gap-4">
                    <div>
                      <p className="text-sm font-medium text-slate-900">
                        新規Statementの自動生成
                      </p>
                      <p className="text-xs text-slate-600">
                        {threadData?.thread?.shouldProceed
                          ? "全員が回答を終えると、新しい質問が生成されます"
                          : "全員が回答を終えても、新しい質問は生成されません"}
                      </p>
                    </div>
                    <div className="flex items-center gap-3">
                      <div
                        aria-hidden="true"
                        className={`flex h-7 w-14 items-center rounded-full border px-1 transition-all duration-150 ${
                          threadData?.thread?.shouldProceed
                            ? "border-emerald-300 bg-emerald-500/90 justify-end"
                            : "border-amber-300 bg-amber-200/90 justify-start"
                        }`}
                      >
                        <div className="flex h-5 w-5 items-center justify-center rounded-full bg-white shadow-sm transition-all duration-150">
                          {threadData?.thread?.shouldProceed ? (
                            <Play className="h-3 w-3 text-emerald-500" />
                          ) : (
                            <Pause className="h-3 w-3 text-amber-500" />
                          )}
                        </div>
                      </div>

                      {togglingProceed && (
                        <Loader2 className="h-4 w-4 animate-spin text-slate-400" />
                      )}
                    </div>
                  </div>
                </button>

                {canEdit && (
                  <div className="rounded-2xl border border-red-200/70 bg-red-50/70 px-4 py-4">
                    <p className="text-sm font-medium text-red-700">
                      セッションを削除
                    </p>
                    <p className="mt-1 text-xs text-red-600">
                      この操作は取り消せません。全てのデータが削除されます。
                    </p>
                    <Button
                      onClick={handleDeleteSession}
                      disabled={deleting}
                      isLoading={deleting}
                      variant="destructive"
                      size="sm"
                      className="mt-3 gap-1.5 text-xs"
                    >
                      <Trash2 className="h-3.5 w-3.5" />
                      セッションを削除
                    </Button>
                  </div>
                )}
              </CardContent>
            </Card>
          </div>
        </div>
      </div>
    </div>
  );
}

interface MonitoringMetricProps {
  label: string;
  value: string;
  subLabel?: string;
  tone?: "default" | "emerald";
}

function MonitoringMetric({
  label,
  value,
  subLabel,
  tone = "default",
}: MonitoringMetricProps) {
  const toneClass =
    tone === "emerald"
      ? "bg-emerald-50/80 border-emerald-100 text-emerald-700"
      : "bg-slate-100/60 border-slate-100 text-slate-700";
  return (
    <div className={`rounded-2xl border px-4 py-4 shadow-sm ${toneClass}`}>
      <p className="text-[11px] uppercase tracking-[0.12em] text-slate-500">
        {label}
      </p>
      <p className="mt-2 text-xl font-semibold">{value}</p>
      {subLabel && <p className="mt-2 text-xs text-slate-500">{subLabel}</p>}
    </div>
  );
}

interface ParticipantProgressRowProps {
  participant: ParticipantProgress;
}

function ParticipantProgressRow({ participant }: ParticipantProgressRowProps) {
  const completionLabel = formatPercentage(participant.completionRate);
  const updatedLabel = formatDateTime(participant.updatedAt);
  const progressRatio =
    participant.totalStatements > 0
      ? Math.min(
          100,
          Math.round(
            (participant.answeredCount / participant.totalStatements) * 100,
          ),
        )
      : 0;

  return (
    <div className="flex h-full flex-col gap-2 rounded-xl border border-slate-200/70 bg-white/70 p-3 shadow-sm">
      <div className="flex flex-wrap items-center gap-x-3 gap-y-1">
        <div className="min-w-0 flex-1">
          <p className="truncate text-sm font-medium text-slate-900">
            {participant.name || "名称未設定"}
          </p>
          <p className="text-[10px] text-slate-400">{updatedLabel}に参加</p>
        </div>
        <div className="text-right">
          <p className="text-sm font-semibold text-slate-900">
            {completionLabel}
          </p>
          <p className="text-[10px] text-slate-500">
            {participant.answeredCount}/{participant.totalStatements}
          </p>
        </div>
      </div>
      <div className="mt-2 h-1 w-full rounded-full bg-slate-200">
        <div
          className="h-full rounded-full bg-indigo-500 transition-all"
          style={{ width: `${progressRatio}%` }}
        />
      </div>
    </div>
  );
}

type HighlightTone = "emerald" | "amber" | "slate";

interface StatementHighlight {
  statement: StatementWithStats;
  positive: number;
  negative: number;
  neutral: number;
  conflict: number;
  responseRate: number;
}

interface StatementHighlightColumnProps {
  title: string;
  tone: HighlightTone;
  items: StatementHighlight[];
}

function StatementHighlightColumn({
  title,
  tone,
  items,
}: StatementHighlightColumnProps) {
  const toneClass =
    tone === "emerald"
      ? "bg-emerald-50 border-emerald-100"
      : tone === "amber"
        ? "bg-amber-50 border-amber-100"
        : "bg-slate-50 border-slate-100";

  const badgeClass =
    tone === "emerald"
      ? "bg-emerald-100 text-emerald-800"
      : tone === "amber"
        ? "bg-amber-100 text-amber-800"
        : "bg-slate-200 text-slate-700";

  return (
    <div className="space-y-3">
      <h3 className="text-sm font-semibold text-slate-900">{title}</h3>
      {items.length === 0 ? (
        <p className="text-xs text-slate-500">
          まだ十分な回答データがありません。
        </p>
      ) : (
        items.map((item, index) => (
          <div
            key={item.statement.id}
            className={`rounded-2xl border px-4 py-4 shadow-sm ${toneClass}`}
          >
            <div className="flex items-start justify-between gap-3">
              <span
                className={`inline-flex items-center justify-center rounded-full px-2 py-0.5 text-[10px] font-semibold ${badgeClass}`}
              >
                #{index + 1}
              </span>
              <div className="text-[11px] text-slate-500">
                回答率 {formatPercentage(item.responseRate)}
              </div>
            </div>
            <p className="mt-3 text-sm text-slate-800 leading-relaxed">
              {item.statement.text}
            </p>
            <div className="mt-4 flex items-center gap-3 text-[11px] text-slate-600">
              <span className="font-medium text-emerald-700">
                Yes {formatPercentage(item.positive)}
              </span>
              <span className="font-medium text-amber-700">
                No {formatPercentage(item.negative)}
              </span>
              <span>わからない {formatPercentage(item.neutral)}</span>
            </div>
            {item.statement.responses.freeTextCount > 0 && (
              <div className="mt-3 rounded-xl border border-white/70 bg-white/70 px-3 py-2 text-[11px] text-slate-700 shadow-inner">
                <div className="flex items-center justify-between gap-2">
                  <span className="font-semibold text-slate-800">
                    自由記述 {item.statement.responses.freeTextCount}件
                  </span>
                </div>
                <ul className="mt-2 space-y-1">
                  {item.statement.responses.freeTextSamples.map(
                    (sample, sampleIndex) => (
                      <li
                        key={`${item.statement.id}-sample-${sampleIndex}`}
                        className="line-clamp-2 text-slate-600"
                      >
                        ・{truncateText(sample.text, 90)}
                      </li>
                    ),
                  )}
                </ul>
              </div>
            )}
          </div>
        ))
      )}
    </div>
  );
}

function ThreadStatusPill({ shouldProceed }: { shouldProceed: boolean }) {
  return (
    <div
      className={`inline-flex items-center gap-1 rounded-full px-3 py-1 text-xs font-medium ${
        shouldProceed
          ? "bg-emerald-50 text-emerald-600 border border-emerald-200"
          : "bg-amber-50 text-amber-600 border border-amber-200"
      }`}
    >
      {shouldProceed ? (
        <>
          <Play className="h-3 w-3" /> 自動生成 ON
        </>
      ) : (
        <>
          <Pause className="h-3 w-3" /> 一時停止中
        </>
      )}
    </div>
  );
}

interface ThreadEventBubbleProps {
  event: TimelineEvent;
  isHostMessage: boolean;
  expanded: boolean;
  onToggle: () => void;
}

function ThreadEventBubble({
  event,
  isHostMessage,
  expanded,
  onToggle,
}: ThreadEventBubbleProps) {
  const markdownProseClass =
    "markdown-body prose prose-sm max-w-none text-slate-800 [&_ol]:list-decimal [&_ul]:list-disc";
  const fadeGradientClass = isHostMessage
    ? "from-indigo-50/95 via-indigo-50/50 to-transparent"
    : "from-white/95 via-white/60 to-transparent";

  const wrapWithFade = (node: ReactElement) => ({
    content: (
      <div className="relative pb-2">
        {node}
        <div
          className={`pointer-events-none absolute inset-x-0 bottom-0 h-10 bg-gradient-to-t ${fadeGradientClass}`}
        />
      </div>
    ),
    hasFade: true,
  });

  const plainContent = (node: ReactElement) => ({
    content: node,
    hasFade: false,
  });

  const meta = EVENT_TYPE_META[event.type] ?? {
    label: event.type,
    accent: "text-slate-500",
    badge: "bg-slate-100 text-slate-600 border-slate-200",
  };

  const markdown =
    typeof event.payload.markdown === "string"
      ? (event.payload.markdown as string)
      : "";

  const showToggle =
    event.type === "survey"
      ? event.statements.length > 3
      : markdown.length > 240;

  const progressValue =
    typeof event.progress === "number"
      ? event.progress
      : Number(event.progress ?? 0);
  const _progressPercent = Math.max(
    0,
    Math.min(100, Math.round(progressValue * 100)),
  );

  const visibleStatements =
    event.type === "survey" && !expanded && event.statements.length > 3
      ? event.statements.slice(0, 3)
      : event.statements;

  const totalSurveyStatements =
    event.type === "survey" ? event.statements.length : 0;

  const statementsList = (
    <div className="space-y-2">
      {event.type === "survey" && totalSurveyStatements > 0 ? (
        <p className="text-base text-slate-900">
          新しく{totalSurveyStatements}
          個の質問を作成しました。皆さんの回答をお待ちしています。
        </p>
      ) : null}
      {visibleStatements.map((statement) => (
        <div
          key={statement.id}
          className="rounded-2xl border border-slate-200/70 bg-white/90 px-3 py-2 text-sm text-slate-700 shadow-sm"
        >
          <span className="mr-2 text-[11px] font-medium text-slate-400">
            #{statement.orderIndex + 1}
          </span>
          {statement.text}
        </div>
      ))}
      {/* {!expanded && event.statements.length > visibleStatements.length && (
        <p className="text-[11px] text-slate-500">
          他{event.statements.length - visibleStatements.length}
          件のステートメントがあります。
        </p>
      )} */}
    </div>
  );

  const showFade = showToggle && !expanded;

  const toggleButton = showToggle ? (
    <button
      type="button"
      onClick={onToggle}
      className="inline-flex items-center gap-1 text-[11px] font-medium text-slate-500 transition-colors hover:text-slate-700"
    >
      {expanded ? (
        <>
          <ChevronUp className="h-3 w-3" />
          閉じる
        </>
      ) : (
        <>
          <ChevronDown className="h-3 w-3" />
          全文を見る
        </>
      )}
    </button>
  ) : null;

  const content = (() => {
    if (event.type === "survey" && visibleStatements.length > 0) {
      return showFade
        ? wrapWithFade(statementsList)
        : plainContent(statementsList);
    }

    if (markdown) {
      const markdownNode = (
        <div className={markdownProseClass}>
          <ReactMarkdown remarkPlugins={[remarkGfm]}>{markdown}</ReactMarkdown>
        </div>
      );

      if (expanded || !showToggle) {
        return plainContent(markdownNode);
      }

      return wrapWithFade(
        <div className={`${markdownProseClass} max-h-48 overflow-hidden`}>
          <ReactMarkdown remarkPlugins={[remarkGfm]}>{markdown}</ReactMarkdown>
        </div>,
      );
    }

    return plainContent(
      <p className="text-sm text-slate-600">内容を準備中です。</p>,
    );
  })();

  return (
    <div
      className={`flex gap-3 ${
        isHostMessage ? "justify-end" : "justify-start"
      }`}
    >
      {!isHostMessage && (
        <div className="mt-2 flex h-8 w-8 flex-shrink-0 items-center justify-center rounded-full bg-slate-900 text-white shadow-sm">
          <Bot className="h-4 w-4" />
        </div>
      )}
      <div
        className={`flex max-w-[min(640px,85%)] flex-col gap-2 ${
          isHostMessage ? "items-end" : "items-start"
        }`}
      >
        <div
          className={`inline-flex items-center gap-2 rounded-full border px-2.5 py-0.5 text-[10px] font-medium ${meta.badge}`}
        >
          <span>{meta.label}</span>
          <span className="text-[10px] text-slate-400">
            #{String(event.orderIndex).padStart(3, "0")}・
            {formatDateTime(event.updatedAt)}
          </span>
        </div>
        <div
          className={`w-full rounded-3xl border px-4 py-3 shadow-sm ${
            isHostMessage
              ? "border-indigo-100 bg-indigo-50/80"
              : "border-slate-200 bg-white/90"
          }`}
        >
          <div className="flex flex-col gap-0">
            {content.content}
            {toggleButton && (
              <div
                className={`flex ${
                  isHostMessage ? "justify-end" : "justify-start"
                } ${content.hasFade ? "-mt-1" : "mt-2"}`}
              >
                {toggleButton}
              </div>
            )}
          </div>
        </div>
        {/* {progressPercent > 0 && progressPercent < 100 && (
          <div className="flex w-full items-center gap-3 text-[11px] text-slate-500">
            <div className="h-1.5 w-full rounded-full bg-slate-200">
              <div
                className={`h-full rounded-full ${
                  isHostMessage ? "bg-indigo-400" : "bg-slate-500"
                }`}
                style={{ width: `${progressPercent}%` }}
              />
            </div>
            <span>{progressPercent}%</span>
          </div>
        )} */}
      </div>
    </div>
  );
}
