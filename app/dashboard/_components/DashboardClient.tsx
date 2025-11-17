"use client";

import axios from "axios";
import {
  Bot,
  Calendar,
  ChevronDown,
  ChevronUp,
  Copy,
  ExternalLink,
  FileText,
  Globe,
  Loader2,
  Lock,
  Maximize2,
  Pencil,
  PanelLeft,
  PanelLeftClose,
  Pause,
  Play,
  Plus,
  Send,
  Trash2,
  Users,
  X,
} from "lucide-react";
import Image from "next/image";
import { usePathname, useRouter, useSearchParams } from "next/navigation";
import {
  type FormEvent,
  type ReactNode,
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";

import { AppHeader } from "@/components/AppHeader";
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

type ThreadEventType = "plan" | "survey" | "survey_analysis" | "user_message";

type TimelineEvent = {
  id: string;
  type: ThreadEventType | string;
  agentId: string | null;
  userId: string | null;
  progress: number | string | null;
  payload: Record<string, unknown>;
  orderIndex: number;
  createdAt: string;
  updatedAt: string;
  statements: TimelineEventStatement[];
};

type EventThreadSummary = {
  id: string;
  shouldProceed: boolean;
  createdAt: string;
  updatedAt: string;
};

type EventThreadAgent = {
  id: string;
  agentType: string;
  state: string;
  statePayload: Record<string, unknown>;
  createdAt: string;
  updatedAt: string;
};

type EventThreadResponse = {
  session: {
    id: string;
    title: string | null;
    context: string | null;
    goal: string | null;
    isPublic: boolean;
  };
  thread: EventThreadSummary | null;
  events: TimelineEvent[];
  agents: EventThreadAgent[];
};

type StatementHighlight = {
  id: string;
  text: string;
  totalResponses: number;
  agreementScore: number;
  positiveShare: number;
  negativeShare: number;
};

type SessionVisibility = "public" | "private";

type AsideSectionKey =
  | "shareLink"
  | "monitoring"
  | "statementHighlights"
  | "progressLog"
  | "sessionInfo";

type SectionCollapsedState = Record<AsideSectionKey, boolean>;

const getDefaultSectionState = (
  hasSession: boolean,
  participantCount: number,
): SectionCollapsedState => ({
  shareLink: !hasSession,
  monitoring: hasSession ? participantCount === 0 : true,
  statementHighlights: true,
  progressLog: true,
  sessionInfo: true,
});

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

const DEFAULT_EVENT_META = {
  label: "イベント",
  accent: "text-slate-600",
  badge: "bg-slate-100 text-slate-600 border-slate-200",
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
      <span className="inline-flex items-center justify-center rounded-full border border-emerald-300 bg-emerald-100 p-1.5">
        <Globe className="h-3 w-3 text-emerald-700" />
      </span>
    );
  }

  return (
    <span className="inline-flex items-center justify-center rounded-full border border-slate-300 bg-slate-100 p-1.5">
      <Lock className="h-3 w-3 text-slate-700" />
    </span>
  );
}

function ThreadStatusPill({ shouldProceed }: { shouldProceed: boolean }) {
  return (
    <div
      className={cn(
        "inline-flex items-center gap-1 rounded-full border px-2 py-0.5 text-[10px] font-semibold",
        shouldProceed
          ? "border-emerald-200 bg-emerald-50 text-emerald-600"
          : "border-amber-200 bg-amber-50 text-amber-600",
      )}
    >
      {shouldProceed ? (
        <>
          <Play className="h-3 w-3" />
          自動進行
        </>
      ) : (
        <>
          <Pause className="h-3 w-3" />
          停止中
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
  const meta =
    event.type in EVENT_TYPE_META
      ? EVENT_TYPE_META[event.type as ThreadEventType]
      : DEFAULT_EVENT_META;
  const payload = (event.payload ?? {}) as Record<string, unknown>;
  const markdown = typeof payload.markdown === "string" ? payload.markdown : "";
  const statements = event.type === "survey" ? (event.statements ?? []) : [];
  const showStatementToggle = event.type === "survey" && statements.length > 3;
  const visibleStatements =
    expanded || !showStatementToggle ? statements : statements.slice(0, 3);

  const statementContent =
    statements.length > 0 ? (
      <div className="space-y-2">
        {visibleStatements.map((statement) => (
          <div
            key={statement.id}
            className="rounded-md border border-border/60 bg-background/90 px-3 py-2 text-xs text-foreground"
          >
            <span className="mr-2 text-[10px] font-semibold text-muted-foreground">
              #{statement.orderIndex + 1}
            </span>
            {statement.text}
          </div>
        ))}
        {!expanded && showStatementToggle && (
          <p className="text-[10px] text-muted-foreground">
            他{statements.length - visibleStatements.length}
            件のステートメントがあります。
          </p>
        )}
      </div>
    ) : null;

  const shouldClampMarkdown = markdown.length > 260 && !expanded;

  const markdownContent = markdown ? (
    <div className="relative">
      <div
        className={cn(
          "prose prose-xs max-w-none text-foreground prose-p:my-1 prose-ol:my-1 prose-ul:my-1",
          shouldClampMarkdown && "max-h-40 overflow-hidden",
        )}
      >
        <ReactMarkdown remarkPlugins={[remarkGfm]}>{markdown}</ReactMarkdown>
      </div>
      {shouldClampMarkdown && (
        <div className="pointer-events-none absolute inset-x-0 bottom-0 h-10 bg-gradient-to-t from-background to-transparent" />
      )}
    </div>
  ) : null;

  const content = statementContent ?? markdownContent ?? (
    <p className="text-xs text-muted-foreground">コンテンツを準備中です。</p>
  );

  const showToggle = showStatementToggle || shouldClampMarkdown;

  const orderNumber = Number(event.orderIndex ?? 0);
  const orderLabel = `#${String(
    Number.isNaN(orderNumber) ? 0 : orderNumber,
  ).padStart(3, "0")}`;

  return (
    <div
      className={cn(
        "flex gap-2 text-xs",
        isHostMessage ? "justify-end" : "justify-start",
      )}
    >
      {!isHostMessage && (
        <div className="mt-1 flex h-7 w-7 flex-shrink-0 items-center justify-center rounded-full bg-primary text-primary-foreground">
          <Bot className="h-3.5 w-3.5" />
        </div>
      )}
      <div
        className={cn(
          "flex max-w-[260px] flex-col gap-2",
          isHostMessage ? "items-end text-right" : "items-start text-left",
        )}
      >
        <div
          className={cn(
            "inline-flex items-center gap-1 rounded-full border px-2 py-0.5 text-[10px] font-semibold",
            meta.badge,
          )}
        >
          <span>{meta.label}</span>
          <span className="text-[9px] text-muted-foreground">
            {orderLabel}・{formatDateTime(event.updatedAt)}
          </span>
        </div>
        <div
          className={cn(
            "w-full rounded-xl border px-3 py-2 shadow-sm",
            isHostMessage
              ? "border-indigo-200 bg-indigo-50/90"
              : "border-border/60 bg-card",
          )}
        >
          {content}
          {showToggle && (
            <button
              type="button"
              onClick={onToggle}
              className={cn(
                "mt-2 inline-flex items-center gap-1 text-[10px] font-semibold text-muted-foreground transition-colors hover:text-foreground",
                isHostMessage ? "ml-auto" : "",
              )}
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
          )}
        </div>
      </div>
    </div>
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
  const [threadData, setThreadData] = useState<EventThreadResponse | null>(
    null,
  );
  const [threadLoading, setThreadLoading] = useState(false);
  const [threadError, setThreadError] = useState<string | null>(null);
  const [messageDraft, setMessageDraft] = useState("");
  const [sendingMessage, setSendingMessage] = useState(false);
  const [togglingThreadProceed, setTogglingThreadProceed] = useState(false);
  const [expandedEvents, setExpandedEvents] = useState<Record<string, boolean>>(
    {},
  );
  const [copyStatus, setCopyStatus] = useState<"idle" | "copied" | "error">(
    "idle",
  );
  const [reportRequestControls, setReportRequestControls] =
    useState<ReactNode | null>(null);
  const [reportHeader, setReportHeader] = useState<ReactNode | null>(null);
  const [isSessionInfoModalOpen, setIsSessionInfoModalOpen] = useState(false);
  const [editingTitle, setEditingTitle] = useState("");
  const [editingGoal, setEditingGoal] = useState("");
  const [editingContext, setEditingContext] = useState("");
  const [editingVisibility, setEditingVisibility] =
    useState<SessionVisibility>("public");
  const [savingSessionInfo, setSavingSessionInfo] = useState(false);
  const [sessionInfoMessage, setSessionInfoMessage] = useState<string | null>(
    null,
  );
  const [sessionInfoError, setSessionInfoError] = useState<string | null>(null);
  const [collapsedSections, setCollapsedSections] =
    useState<SectionCollapsedState>(getDefaultSectionState(false, 0));
  const [collapsedOverrides, setCollapsedOverrides] = useState<
    Partial<Record<AsideSectionKey, boolean>>
  >({});
  const [isShareQrFullscreen, setIsShareQrFullscreen] = useState(false);
  const [isShareQrErrored, setIsShareQrErrored] = useState(false);
  const threadContainerRef = useRef<HTMLDivElement | null>(null);
  const latestThreadKeyRef = useRef<string>("");
  const threadFetchInFlightRef = useRef(false);
  const previousSessionIdRef = useRef<string | null>(null);

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
  const selectedAdminSessionId = selectedAdminSession?.id ?? null;

  const selectedSessionShareLink = selectedAdminSession
    ? `${shareBaseUrl}/sessions/${selectedAdminSession.id}`
    : "";

  const hasSelectedSession = Boolean(
    selectedAdminSession && selectedAdminAccessToken,
  );

  const resetSessionInfoForm = useCallback(() => {
    if (sessionDetail) {
      setEditingTitle(sessionDetail.title ?? "");
      setEditingGoal(sessionDetail.goal ?? "");
      setEditingContext(sessionDetail.context ?? "");
      setEditingVisibility(sessionDetail.isPublic ? "public" : "private");
      return;
    }
    if (selectedAdminSession) {
      setEditingTitle(selectedAdminSession.title ?? "");
      setEditingGoal(selectedAdminSession.goal ?? "");
      setEditingContext(selectedAdminSession.context ?? "");
      setEditingVisibility(
        selectedAdminSession.isPublic ? "public" : "private",
      );
      return;
    }
    setEditingTitle("");
    setEditingGoal("");
    setEditingContext("");
    setEditingVisibility("public");
  }, [selectedAdminSession, sessionDetail]);

  const handleToggleSection = useCallback((key: AsideSectionKey) => {
    setCollapsedSections((previous) => ({
      ...previous,
      [key]: !previous[key],
    }));
    setCollapsedOverrides((previous) => ({
      ...previous,
      [key]: true,
    }));
  }, []);

  const ensureSectionOpen = useCallback((key: AsideSectionKey) => {
    setCollapsedSections((previous) => {
      if (!previous[key]) {
        return previous;
      }
      return {
        ...previous,
        [key]: false,
      };
    });
    setCollapsedOverrides((previous) => ({
      ...previous,
      [key]: true,
    }));
  }, []);

  const participantCount =
    sessionDetail?.totalParticipants ??
    selectedAdminSession?._count.participants ??
    0;

  const initialParticipantCount = useMemo(
    () => selectedAdminSession?._count.participants ?? 0,
    [selectedAdminSession],
  );

  useEffect(() => {
    if (isSessionInfoModalOpen) return;
    resetSessionInfoForm();
  }, [isSessionInfoModalOpen, resetSessionInfoForm]);

  useEffect(() => {
    if (previousSessionIdRef.current === selectedAdminSessionId) {
      return;
    }
    previousSessionIdRef.current = selectedAdminSessionId;
    const defaults = getDefaultSectionState(
      hasSelectedSession,
      initialParticipantCount,
    );
    setCollapsedSections(defaults);
    setCollapsedOverrides({});
    setIsSessionInfoModalOpen(false);
    setSessionInfoMessage(null);
    setSessionInfoError(null);
  }, [hasSelectedSession, initialParticipantCount, selectedAdminSessionId]);

  useEffect(() => {
    if (!hasSelectedSession) return;
    setCollapsedSections((previous) => {
      let shouldUpdate = false;
      const next = { ...previous };
      if (!collapsedOverrides.shareLink && next.shareLink) {
        next.shareLink = false;
        shouldUpdate = true;
      }
      if (!collapsedOverrides.monitoring) {
        const desired = participantCount === 0;
        if (next.monitoring !== desired) {
          next.monitoring = desired;
          shouldUpdate = true;
        }
      }
      return shouldUpdate ? next : previous;
    });
  }, [collapsedOverrides, hasSelectedSession, participantCount]);

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

  const handleSaveSessionInfo = useCallback(
    async (event: FormEvent<HTMLFormElement>) => {
      event.preventDefault();
      if (
        !selectedAdminSessionId ||
        !selectedAdminAccessToken ||
        !userId ||
        savingSessionInfo
      ) {
        return;
      }
      setSavingSessionInfo(true);
      setSessionInfoMessage(null);
      setSessionInfoError(null);
      try {
        const response = await axios.patch(
          `/api/sessions/${selectedAdminSessionId}/${selectedAdminAccessToken}`,
          {
            title: editingTitle,
            context: editingContext,
            goal: editingGoal,
            isPublic: editingVisibility === "public",
          },
          {
            headers: createAuthorizationHeader(userId),
          },
        );
        const updated = response.data.data as {
          title: string;
          context: string | null;
          goal: string | null;
          isPublic: boolean;
        };
        setSessionDetail((previous) =>
          previous
            ? {
                ...previous,
                title: updated.title ?? "",
                context: updated.context ?? "",
                goal: updated.goal ?? "",
                isPublic: updated.isPublic,
              }
            : previous,
        );
        setSessions((previous) =>
          previous.map((session) =>
            session.id === selectedAdminSessionId
              ? {
                  ...session,
                  title: updated.title ?? "",
                  context: updated.context ?? "",
                  goal: updated.goal ?? "",
                  isPublic: updated.isPublic,
                }
              : session,
          ),
        );
        setSessionInfoMessage("セッション情報を更新しました。");
        setIsSessionInfoModalOpen(false);
      } catch (err) {
        console.error("Failed to update session info:", err);
        setSessionInfoError("セッション情報の更新に失敗しました。");
      } finally {
        setSavingSessionInfo(false);
      }
    },
    [
      editingContext,
      editingGoal,
      editingTitle,
      editingVisibility,
      savingSessionInfo,
      selectedAdminAccessToken,
      selectedAdminSessionId,
      userId,
    ],
  );

  const handleCancelSessionInfoEdit = useCallback(() => {
    setSessionInfoError(null);
    setIsSessionInfoModalOpen(false);
    resetSessionInfoForm();
  }, [resetSessionInfoForm]);

  useEffect(() => {
    setCopyStatus("idle");
    setIsShareQrFullscreen(false);

    if (!selectedAdminSessionId || !selectedAdminAccessToken || !userId) {
      setSessionDetail(null);
      setDetailError(null);
      setThreadData(null);
      setThreadError(null);
      setThreadLoading(false);
      setMessageDraft("");
      setExpandedEvents({});
      setReportRequestControls(null);
      setReportHeader(null);
      setIsShareQrErrored(false);
      latestThreadKeyRef.current = "";
      return;
    }

    let cancelled = false;
    setReportRequestControls(null);
    setReportHeader(null);
    setDetailLoading(true);
    setDetailError(null);
    setThreadData(null);
    setThreadError(null);
    setThreadLoading(true);

    const fetchDetail = async () => {
      try {
        const response = await axios.get(
          `/api/sessions/${selectedAdminSessionId}/${selectedAdminAccessToken}`,
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

    void fetchDetail();
    setIsShareQrErrored(false);

    return () => {
      cancelled = true;
    };
  }, [selectedAdminAccessToken, selectedAdminSessionId, userId]);

  const fetchEventThread = useCallback(
    async ({ showSpinner = false }: { showSpinner?: boolean } = {}) => {
      if (!selectedAdminSessionId || !selectedAdminAccessToken || !userId) {
        return;
      }
      if (threadFetchInFlightRef.current && !showSpinner) {
        return;
      }
      const requestKey = `${selectedAdminSessionId}:${selectedAdminAccessToken}`;
      latestThreadKeyRef.current = requestKey;
      threadFetchInFlightRef.current = true;
      if (showSpinner) {
        setThreadLoading(true);
        setThreadError(null);
      }
      try {
        const response = await axios.get(
          `/api/sessions/${selectedAdminSessionId}/${selectedAdminAccessToken}/event-thread`,
          {
            headers: createAuthorizationHeader(userId),
          },
        );
        if (latestThreadKeyRef.current !== requestKey) {
          return;
        }
        const responseData = response.data as EventThreadResponse;
        setThreadData(responseData);
        setThreadError(null);
      } catch (err) {
        if (latestThreadKeyRef.current !== requestKey) {
          return;
        }
        console.error("Failed to fetch event thread:", err);
        setThreadError("進行ログの取得に失敗しました。");
      } finally {
        threadFetchInFlightRef.current = false;
        if (showSpinner) {
          setThreadLoading(false);
        }
      }
    },
    [selectedAdminAccessToken, selectedAdminSessionId, userId],
  );

  useEffect(() => {
    if (!selectedAdminSessionId || !selectedAdminAccessToken || !userId) {
      return;
    }
    void fetchEventThread({ showSpinner: true });
    const intervalId = window.setInterval(() => {
      void fetchEventThread();
    }, 6000);
    return () => {
      window.clearInterval(intervalId);
    };
  }, [
    fetchEventThread,
    selectedAdminAccessToken,
    selectedAdminSessionId,
    userId,
  ]);

  useEffect(() => {
    setExpandedEvents({});
    setMessageDraft("");
  }, [selectedAdminSessionId]);

  useEffect(() => {
    if (!threadData?.events?.length) return;
    const container = threadContainerRef.current;
    if (!container) return;
    container.scrollTop = container.scrollHeight;
  }, [threadData?.events]);

  useEffect(() => {
    if (!threadData?.events) return;
    const ids = new Set(threadData.events.map((event) => event.id));
    setExpandedEvents((previous) => {
      let changed = false;
      const next = { ...previous };
      Object.keys(next).forEach((id) => {
        if (!ids.has(id)) {
          delete next[id];
          changed = true;
        }
      });
      return changed ? next : previous;
    });
  }, [threadData?.events]);

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

  const handleSendThreadMessage = useCallback(async () => {
    if (
      !selectedAdminSessionId ||
      !userId ||
      messageDraft.trim().length === 0
    ) {
      return;
    }
    setSendingMessage(true);
    try {
      await axios.post(
        `/api/sessions/${selectedAdminSessionId}/event-thread/events/user-message`,
        { markdown: messageDraft.trim() },
        {
          headers: createAuthorizationHeader(userId),
        },
      );
      setMessageDraft("");
      await fetchEventThread({ showSpinner: true });
    } catch (err) {
      console.error("Failed to send message:", err);
      window.alert("メッセージの送信に失敗しました。");
    } finally {
      setSendingMessage(false);
    }
  }, [fetchEventThread, messageDraft, selectedAdminSessionId, userId]);

  const handleToggleThreadProceed = useCallback(async () => {
    if (!selectedAdminSessionId || !threadData?.thread || !userId) {
      return;
    }
    setTogglingThreadProceed(true);
    try {
      const response = await axios.patch(
        `/api/sessions/${selectedAdminSessionId}/event-thread/should-proceed`,
        { shouldProceed: !threadData.thread.shouldProceed },
        {
          headers: createAuthorizationHeader(userId),
        },
      );
      const updatedThread = response.data.thread as EventThreadSummary;
      setThreadData((previous) =>
        previous
          ? {
              ...previous,
              thread: updatedThread,
            }
          : previous,
      );
    } catch (err) {
      console.error("Failed to toggle shouldProceed:", err);
      window.alert("自動進行の切り替えに失敗しました。");
    } finally {
      setTogglingThreadProceed(false);
    }
  }, [selectedAdminSessionId, threadData, userId]);

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

  const threadEvents = threadData?.events ?? [];
  const hasThread = Boolean(threadData?.thread);
  const threadShouldProceed = threadData?.thread?.shouldProceed ?? false;
  const canManageThread = Boolean(selectedAdminSession?.isHost);
  const canEditSessionInfo = Boolean(selectedAdminSession?.isHost);

  const openSessionInfoModal = useCallback(() => {
    if (!canEditSessionInfo || !hasSelectedSession) return;
    ensureSectionOpen("sessionInfo");
    setSessionInfoMessage(null);
    setSessionInfoError(null);
    resetSessionInfoForm();
    setIsSessionInfoModalOpen(true);
  }, [
    canEditSessionInfo,
    ensureSectionOpen,
    hasSelectedSession,
    resetSessionInfoForm,
  ]);

  if (userLoading || loading) {
    return (
      <div className="flex min-h-screen items-center justify-center">
        <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
      </div>
    );
  }

  return (
    <>
      <div className="flex h-screen flex-col bg-gradient-to-b from-slate-50 to-white">
        <AppHeader />

        <div className="flex flex-1 overflow-hidden">
          <aside
            className={cn(
              "flex flex-col border-r border-slate-200 bg-white transition-all duration-300",
              isSidebarCollapsed ? "w-0" : "w-80",
            )}
          >
            {!isSidebarCollapsed && (
              <>
                <div className="flex h-16 items-center gap-2 border-b border-slate-200 px-4">
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
                                ? "border-blue-300 bg-blue-50 shadow-sm"
                                : "border-slate-200 bg-white hover:border-blue-200 hover:shadow-sm",
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
                    <div className="flex min-h-[200px] items-center justify-center rounded-xl border border-dashed border-slate-300 bg-slate-50 px-4 text-center text-xs text-slate-600">
                      管理中のセッションがありません。新規作成してチームの対話を始めましょう。
                    </div>
                  )}
                </div>
              </>
            )}
          </aside>

          <main className="flex flex-1 flex-col overflow-hidden">
            <div className="flex h-16 items-center justify-between border-b border-slate-200 bg-white px-6">
              <div className="flex items-center gap-4">
                <Button
                  variant="ghost"
                  size="icon"
                  className="h-9 w-9"
                  onClick={() => setIsSidebarCollapsed((value) => !value)}
                >
                  {isSidebarCollapsed ? (
                    <PanelLeft className="h-4 w-4" />
                  ) : (
                    <PanelLeftClose className="h-4 w-4" />
                  )}
                </Button>
                <div className="space-y-1">
                  <div className="flex items-center gap-2">
                    <h2 className="text-lg font-semibold text-card-foreground">
                      {selectedAdminSession
                        ? selectedAdminSession.title || "名称未設定"
                        : sessions.length === 0
                          ? "まだセッションがありません"
                          : "管理可能なセッションを選択してください"}
                    </h2>
                    {canEditSessionInfo && hasSelectedSession && (
                      <Button
                        type="button"
                        variant="ghost"
                        size="icon"
                        className="h-7 w-7 rounded-full text-muted-foreground hover:text-foreground"
                        onClick={openSessionInfoModal}
                        aria-label="セッション情報を編集"
                      >
                        <Pencil className="h-4 w-4" />
                      </Button>
                    )}
                  </div>
                  <div className="flex items-start gap-1 text-xs text-muted-foreground">
                    <p className="flex-1">
                      {selectedAdminSession
                        ? selectedAdminSession.goal ||
                          selectedAdminSession.context ||
                          "セッションの目的や背景は未設定です。"
                        : managedSessions.length > 0
                          ? "左側の一覧からセッションを選ぶとレポートが表示されます。"
                          : "まずは新しいセッションを作成して、対話を設計しましょう。"}
                    </p>
                    {canEditSessionInfo && hasSelectedSession && (
                      <button
                        type="button"
                        onClick={openSessionInfoModal}
                        className="mt-0.5 rounded-full p-1 text-muted-foreground transition hover:bg-slate-100 hover:text-foreground"
                        aria-label="セッション概要を編集"
                      >
                        <Pencil className="h-3.5 w-3.5" />
                      </button>
                    )}
                  </div>
                </div>
              </div>
              {headerStats.length > 0 && (
                <div className="hidden items-center gap-3 md:flex">
                  {canEditSessionInfo && selectedAdminSession && (
                    <Button
                      variant="destructive"
                      size="sm"
                      className="gap-1 text-xs"
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
                  )}
                  {headerStats.map((stat) => {
                    const Icon = stat.icon;
                    return (
                      <div
                        key={stat.label}
                        className="flex items-center gap-2 rounded-full bg-slate-100 px-3 py-1 text-xs text-slate-600"
                      >
                        <Icon className="h-3.5 w-3.5 text-slate-500" />
                        <span className="font-semibold text-slate-900">
                          {stat.value}
                        </span>
                        <span>{stat.label}</span>
                      </div>
                    );
                  })}
                </div>
              )}
            </div>

            <div className="flex flex-1 overflow-hidden">
              <div className="flex flex-1 flex-col overflow-hidden border-r border-slate-200">
                {reportHeader && (
                  <div className="border-b border-slate-200 bg-white px-6 py-4">
                    {reportHeader}
                  </div>
                )}

                <div className="flex h-full flex-1 flex-col overflow-hidden bg-white">
                  {error && (
                    <div className="mx-6 mt-6 rounded-xl border border-rose-200 bg-rose-50 px-6 py-3">
                      <p className="text-sm font-semibold text-rose-700">
                        データの取得に失敗しました
                      </p>
                      <p className="text-xs text-rose-600">{error}</p>
                    </div>
                  )}
                  <div className="flex-1 overflow-hidden">
                    {detailLoading && hasSelectedSession ? (
                      <div className="flex h-full items-center justify-center">
                        <Loader2 className="h-6 w-6 animate-spin text-slate-400" />
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
                      <div className="flex h-full flex-col items-center justify-center gap-2 text-sm text-slate-500">
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
                      <div className="mx-6 mb-6 rounded-xl border border-amber-300 bg-amber-50 px-6 py-3">
                        <p className="text-sm text-amber-700">{detailError}</p>
                      </div>
                    )}
                  </div>
                  {reportRequestControls && (
                    <div className="px-6 pb-6">
                      {reportRequestControls}
                    </div>
                  )}
                </div>
              </div>

              <aside className="w-80 bg-slate-50">
                <div className="flex h-full flex-col overflow-y-auto px-6 py-6">
                  <div className="space-y-4">
                    <Card>
                      <CardHeader className="pb-2">
                        <div className="flex items-start justify-between gap-2">
                          <div>
                            <CardTitle className="text-sm font-semibold">
                              参加用リンク
                            </CardTitle>
                            <CardDescription className="text-xs">
                              招待URLとQRコードをまとめて共有できます。
                            </CardDescription>
                          </div>
                          <button
                            type="button"
                            onClick={() => handleToggleSection("shareLink")}
                            className="text-muted-foreground transition hover:text-foreground"
                            aria-expanded={!collapsedSections.shareLink}
                            aria-controls="aside-shareLink"
                          >
                            {collapsedSections.shareLink ? (
                              <ChevronDown className="h-4 w-4" />
                            ) : (
                              <ChevronUp className="h-4 w-4" />
                            )}
                            <span className="sr-only">
                              セクションを切り替え
                            </span>
                          </button>
                        </div>
                      </CardHeader>
                      {!collapsedSections.shareLink && (
                        <CardContent id="aside-shareLink" className="space-y-3">
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
                      )}
                    </Card>

                    <Card>
                      <CardHeader className="pb-2">
                        <div className="flex items-start justify-between gap-2">
                          <div>
                            <CardTitle className="text-sm font-semibold">
                              モニタリング
                            </CardTitle>
                            <CardDescription className="text-xs">
                              参加者ごとの回答率をトラッキングできます。
                            </CardDescription>
                          </div>
                          <button
                            type="button"
                            onClick={() => handleToggleSection("monitoring")}
                            className="text-muted-foreground transition hover:text-foreground"
                            aria-expanded={!collapsedSections.monitoring}
                            aria-controls="aside-monitoring"
                          >
                            {collapsedSections.monitoring ? (
                              <ChevronDown className="h-4 w-4" />
                            ) : (
                              <ChevronUp className="h-4 w-4" />
                            )}
                            <span className="sr-only">
                              セクションを切り替え
                            </span>
                          </button>
                        </div>
                      </CardHeader>
                      {!collapsedSections.monitoring && (
                        <CardContent
                          id="aside-monitoring"
                          className="space-y-3"
                        >
                          {detailLoading && hasSelectedSession ? (
                            <div className="flex items-center justify-center py-6">
                              <Loader2 className="h-5 w-5 animate-spin text-muted-foreground" />
                            </div>
                          ) : participantProgress.length > 0 ? (
                            participantProgress
                              .slice(0, 5)
                              .map((participant) => (
                                <div
                                  key={participant.userId}
                                  className="rounded-md border border-border/50 bg-background px-3 py-3 text-xs"
                                >
                                  <div className="flex items-center justify-between">
                                    <span className="font-semibold text-foreground">
                                      {participant.name || "匿名ユーザー"}
                                    </span>
                                    <span className="rounded-full bg-muted px-2 py-0.5 text-[10px] font-semibold text-muted-foreground">
                                      {formatPercent(
                                        participant.completionRate,
                                      )}
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
                      )}
                    </Card>

                    {/* <Card>
                      <CardHeader className="pb-2">
                        <div className="flex items-start justify-between gap-2">
                          <div>
                            <CardTitle className="text-sm font-semibold">
                              ステートメントのハイライト
                            </CardTitle>
                            <CardDescription className="text-xs">
                              回答が集まっているテーマを把握し、次の打ち手を検討します。
                            </CardDescription>
                          </div>
                          <button
                            type="button"
                            onClick={() =>
                              handleToggleSection("statementHighlights")
                            }
                            className="text-muted-foreground transition hover:text-foreground"
                            aria-expanded={
                              !collapsedSections.statementHighlights
                            }
                            aria-controls="aside-statementHighlights"
                          >
                            {collapsedSections.statementHighlights ? (
                              <ChevronDown className="h-4 w-4" />
                            ) : (
                              <ChevronUp className="h-4 w-4" />
                            )}
                            <span className="sr-only">
                              セクションを切り替え
                            </span>
                          </button>
                        </div>
                      </CardHeader>
                      {!collapsedSections.statementHighlights && (
                        <CardContent
                          id="aside-statementHighlights"
                          className="space-y-3"
                        >
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
                      )}
                    </Card> */}

                    {/* <Card>
                      <CardHeader className="pb-2">
                        <div className="flex items-start justify-between gap-2">
                          <div>
                            <CardTitle className="text-sm font-semibold">
                              進行ログ
                            </CardTitle>
                            <CardDescription className="text-xs">
                              Cartographer エージェントの動作履歴を確認します。
                            </CardDescription>
                          </div>
                          <div className="flex items-center gap-2">
                            {hasSelectedSession && hasThread && (
                              <ThreadStatusPill
                                shouldProceed={threadShouldProceed}
                              />
                            )}
                            <button
                              type="button"
                              onClick={() => handleToggleSection("progressLog")}
                              className="text-muted-foreground transition hover:text-foreground"
                              aria-expanded={!collapsedSections.progressLog}
                              aria-controls="aside-progressLog"
                            >
                              {collapsedSections.progressLog ? (
                                <ChevronDown className="h-4 w-4" />
                              ) : (
                                <ChevronUp className="h-4 w-4" />
                              )}
                              <span className="sr-only">
                                セクションを切り替え
                              </span>
                            </button>
                          </div>
                        </div>
                      </CardHeader>
                      {!collapsedSections.progressLog && (
                        <CardContent
                          id="aside-progressLog"
                          className="space-y-3"
                        >
                          {!hasSelectedSession ? (
                            <p className="text-xs text-muted-foreground">
                              管理権限のあるセッションを選択すると、進行ログが表示されます。
                            </p>
                          ) : threadLoading ? (
                            <div className="flex items-center justify-center py-6">
                              <Loader2 className="h-5 w-5 animate-spin text-muted-foreground" />
                            </div>
                          ) : threadError ? (
                            <div className="rounded-md border border-amber-300 bg-amber-50 px-3 py-3 text-xs text-amber-700">
                              {threadError}
                            </div>
                          ) : (
                            <>
                              {canManageThread && hasThread && (
                                <Button
                                  variant={
                                    threadShouldProceed ? "ghost" : "outline"
                                  }
                                  size="sm"
                                  className="w-full justify-center gap-1 text-xs"
                                  onClick={() => {
                                    void handleToggleThreadProceed();
                                  }}
                                  disabled={togglingThreadProceed}
                                >
                                  {togglingThreadProceed ? (
                                    <Loader2 className="h-3.5 w-3.5 animate-spin" />
                                  ) : threadShouldProceed ? (
                                    <Pause className="h-3.5 w-3.5" />
                                  ) : (
                                    <Play className="h-3.5 w-3.5" />
                                  )}
                                  {threadShouldProceed
                                    ? "自動生成を一時停止"
                                    : "自動生成を再開"}
                                </Button>
                              )}
                              {threadEvents.length > 0 ? (
                                <div
                                  ref={threadContainerRef}
                                  className="max-h-80 space-y-3 overflow-y-auto pr-1"
                                >
                                  {threadEvents.map((event) => {
                                    const isHostMessage =
                                      event.type === "user_message";
                                    const expanded = Boolean(
                                      expandedEvents[event.id],
                                    );
                                    return (
                                      <ThreadEventBubble
                                        key={event.id}
                                        event={event}
                                        isHostMessage={isHostMessage}
                                        expanded={expanded}
                                        onToggle={() =>
                                          setExpandedEvents((previous) => ({
                                            ...previous,
                                            [event.id]: !previous[event.id],
                                          }))
                                        }
                                      />
                                    );
                                  })}
                                </div>
                              ) : (
                                <p className="text-xs text-muted-foreground">
                                  まだ記録されたイベントはありません。
                                </p>
                              )}
                            </>
                          )}
                          {hasSelectedSession && canManageThread && (
                            <div className="space-y-2 border-t border-dashed border-border/60 pt-3">
                              <label
                                htmlFor="threadMessage"
                                className="text-[11px] font-semibold text-muted-foreground"
                              >
                                ファシリテーターAIへのメッセージ
                              </label>
                              <textarea
                                id="threadMessage"
                                value={messageDraft}
                                onChange={(event) =>
                                  setMessageDraft(event.target.value)
                                }
                                rows={3}
                                className="w-full rounded-md border border-border/60 bg-background px-2.5 py-2 text-xs text-foreground placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-primary/30"
                                placeholder="進行の補足情報や指示があれば、ここに入力してください。"
                              />
                              <div className="flex justify-end">
                                <Button
                                  type="button"
                                  size="sm"
                                  onClick={() => {
                                    void handleSendThreadMessage();
                                  }}
                                  disabled={
                                    sendingMessage ||
                                    messageDraft.trim().length === 0
                                  }
                                  isLoading={sendingMessage}
                                  className="gap-1 text-xs"
                                >
                                  <Send className="h-3.5 w-3.5" />
                                  送信
                                </Button>
                              </div>
                            </div>
                          )}
                        </CardContent>
                      )}
                    </Card> */}

                    {/* <Card>
                      <CardHeader className="pb-2">
                        <div className="flex items-start justify-between gap-2">
                          <div>
                            <CardTitle className="text-sm font-semibold">
                              セッション情報
                            </CardTitle>
                            <CardDescription className="text-xs">
                              {canEditSessionInfo
                                ? "目的や公開設定を確認・編集できます。"
                                : "目的や公開設定を確認できます。"}
                            </CardDescription>
                          </div>
                            <div className="flex items-center gap-2">
                              {hasSelectedSession && canEditSessionInfo && (
                                <Button
                                  variant="ghost"
                                  size="sm"
                                  className="gap-1 text-xs"
                                  onClick={openSessionInfoModal}
                                >
                                  編集
                                </Button>
                              )}
                            <button
                              type="button"
                              onClick={() => handleToggleSection("sessionInfo")}
                              className="text-muted-foreground transition hover:text-foreground"
                              aria-expanded={!collapsedSections.sessionInfo}
                              aria-controls="aside-sessionInfo"
                            >
                              {collapsedSections.sessionInfo ? (
                                <ChevronDown className="h-4 w-4" />
                              ) : (
                                <ChevronUp className="h-4 w-4" />
                              )}
                              <span className="sr-only">
                                セクションを切り替え
                              </span>
                            </button>
                          </div>
                        </div>
                      </CardHeader>
                      {!collapsedSections.sessionInfo && (
                        <CardContent
                          id="aside-sessionInfo"
                          className="space-y-4 text-xs text-muted-foreground"
                        >
                          {!hasSelectedSession ? (
                            <p>
                              管理セッションを選択すると情報が表示されます。
                            </p>
                          ) : detailLoading ? (
                            <div className="flex items-center justify-center py-6">
                              <Loader2 className="h-5 w-5 animate-spin text-muted-foreground" />
                            </div>
                          ) : (
                            <>
                              {sessionInfoMessage && (
                                <div className="rounded-md border border-emerald-200 bg-emerald-50 px-3 py-2 text-[11px] text-emerald-700">
                                  {sessionInfoMessage}
                                </div>
                              )}
                              <div className="space-y-3 text-xs text-muted-foreground">
                                <div className="flex items-center justify-between">
                                  <p className="text-[11px] font-semibold text-muted-foreground">
                                    公開状態
                                  </p>
                                  <VisibilityBadge
                                    isPublic={
                                      sessionDetail?.isPublic ??
                                      selectedAdminSession?.isPublic ??
                                      false
                                    }
                                  />
                                </div>
                                <div>
                                  <p className="text-[11px] font-semibold text-muted-foreground">
                                    ゴール
                                  </p>
                                  <p className="mt-1 whitespace-pre-wrap text-sm text-foreground">
                                    {sessionDetail?.goal ||
                                    selectedAdminSession?.goal
                                      ? (sessionDetail?.goal ??
                                        selectedAdminSession?.goal)
                                      : "未設定です。"}
                                  </p>
                                </div>
                                <div>
                                  <p className="text-[11px] font-semibold text-muted-foreground">
                                    コンテキスト
                                  </p>
                                  <p className="mt-1 whitespace-pre-wrap text-sm text-foreground">
                                    {sessionDetail?.context ||
                                    selectedAdminSession?.context
                                      ? (sessionDetail?.context ??
                                        selectedAdminSession?.context)
                                      : "未設定です。"}
                                  </p>
                                </div>
                              </div>
                            </>
                          )}
                        </CardContent>
                      )}
                    </Card> */}

                    <Card>
                      <CardHeader className="pb-2">
                        <CardTitle className="text-sm font-semibold">
                          進行設定
                        </CardTitle>
                        <CardDescription className="text-xs">
                          新規ステートメント生成とセッション管理を行えます。
                        </CardDescription>
                      </CardHeader>
                      <CardContent className="space-y-4 text-xs text-muted-foreground">
                        {selectedAdminSession ? (
                          <>
                            <button
                              type="button"
                              onClick={
                                canManageThread
                                  ? () => {
                                      void handleToggleThreadProceed();
                                    }
                                  : undefined
                              }
                              disabled={
                                !canManageThread || togglingThreadProceed
                              }
                              aria-pressed={threadShouldProceed}
                              className={cn(
                                "w-full rounded-lg border px-4 py-3 text-left transition focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-primary/40",
                                threadShouldProceed
                                  ? "border-emerald-200 bg-emerald-50/80 hover:bg-emerald-50"
                                  : "border-amber-200 bg-amber-50/70 hover:bg-amber-50",
                                !canManageThread || togglingThreadProceed
                                  ? "cursor-not-allowed opacity-75"
                                  : "",
                              )}
                            >
                              <div className="flex items-center justify-between gap-4">
                                <div className="space-y-1">
                                  <p className="text-sm font-semibold text-foreground">
                                    新規ステートメントの自動生成
                                  </p>
                                  <p className="text-[11px] text-muted-foreground">
                                    {threadShouldProceed
                                      ? "全員の回答が揃うと、次のステートメントを自動で提示します。"
                                      : "回答が揃っても、新しいステートメントは提示されません。"}
                                  </p>
                                </div>
                                <div className="flex items-center gap-3">
                                  <div
                                    aria-hidden="true"
                                    className={cn(
                                      "flex h-7 w-14 items-center rounded-full border px-1 transition-all duration-150",
                                      threadShouldProceed
                                        ? "justify-end border-emerald-300 bg-emerald-500/90"
                                        : "justify-start border-amber-300 bg-amber-200/80",
                                    )}
                                  >
                                    <div className="flex h-5 w-5 items-center justify-center rounded-full bg-white shadow-sm transition-all duration-150">
                                      {threadShouldProceed ? (
                                        <Play className="h-3 w-3 text-emerald-500" />
                                      ) : (
                                        <Pause className="h-3 w-3 text-amber-500" />
                                      )}
                                    </div>
                                  </div>
                                  {togglingThreadProceed && (
                                    <Loader2 className="h-4 w-4 animate-spin text-muted-foreground" />
                                  )}
                                </div>
                              </div>
                            </button>
                            {!canManageThread && (
                              <p className="text-[11px]">
                                セッションホストのみが自動生成設定を変更できます。
                              </p>
                            )}
                            <div className="rounded-lg border border-rose-200/70 bg-rose-50/70 px-4 py-3">
                              <p className="text-sm font-semibold text-rose-700">
                                セッションを削除
                              </p>
                              <p className="mt-1 text-[11px] leading-relaxed text-rose-600">
                                この操作は取り消せません。セッションと関連するデータが全て削除されます。
                              </p>
                              <Button
                                variant="destructive"
                                size="sm"
                                className="mt-3 gap-1 text-xs"
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

      {isSessionInfoModalOpen && (
        <div className="fixed inset-0 z-50">
          <button
            type="button"
            aria-label="セッション情報の編集を閉じる"
            tabIndex={-1}
            className="absolute inset-0 h-full w-full bg-slate-950/60"
            onClick={handleCancelSessionInfoEdit}
          />
          <div className="relative z-10 flex h-full w-full items-center justify-center px-4 py-8">
            <div
              className="w-full max-w-2xl rounded-3xl border border-slate-200 bg-white shadow-2xl"
              role="dialog"
              aria-modal="true"
            >
              <div className="flex items-center justify-between border-b border-slate-100 px-6 py-4">
                <div>
                  <p className="text-xs font-semibold uppercase tracking-wide text-slate-500">
                    セッション情報
                  </p>
                  <h2 className="text-xl font-bold text-slate-900">
                    セッションを編集
                  </h2>
                </div>
                <button
                  type="button"
                  onClick={handleCancelSessionInfoEdit}
                  aria-label="閉じる"
                  className="rounded-full border border-slate-200 p-2 text-slate-500 transition-colors hover:border-slate-300 hover:text-slate-900"
                >
                  <X className="h-4 w-4" />
                </button>
              </div>
              <form
                onSubmit={handleSaveSessionInfo}
                className="max-h-[70vh] overflow-y-auto px-6 py-6 space-y-4 text-xs text-muted-foreground"
              >
                <div className="space-y-1.5">
                  <label
                    htmlFor="sessionInfoModalTitle"
                    className="text-[11px] font-semibold text-muted-foreground"
                  >
                    タイトル
                  </label>
                  <Input
                    id="sessionInfoModalTitle"
                    value={editingTitle}
                    onChange={(event) => setEditingTitle(event.target.value)}
                    required
                    className="text-sm"
                  />
                </div>

                <div className="space-y-1.5">
                  <span className="text-[11px] font-semibold text-muted-foreground">
                    公開設定
                  </span>
                  <div className="grid grid-cols-2 gap-2">
                    <label className="flex items-center gap-2 rounded-md border border-border/60 bg-muted/40 px-3 py-2 text-xs font-semibold text-foreground">
                      <input
                        type="radio"
                        name="sessionInfoModalVisibility"
                        value="public"
                        checked={editingVisibility === "public"}
                        onChange={() => setEditingVisibility("public")}
                      />
                      <span>公開</span>
                    </label>
                    <label className="flex items-center gap-2 rounded-md border border-border/60 bg-muted/40 px-3 py-2 text-xs font-semibold text-foreground">
                      <input
                        type="radio"
                        name="sessionInfoModalVisibility"
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
                    htmlFor="sessionInfoModalGoal"
                    className="text-[11px] font-semibold text-muted-foreground"
                  >
                    ゴール
                  </label>
                  <textarea
                    id="sessionInfoModalGoal"
                    value={editingGoal}
                    onChange={(event) => setEditingGoal(event.target.value)}
                    rows={4}
                    className="w-full rounded-md border border-border/70 bg-background px-3 py-2 text-sm text-foreground placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-primary/30"
                  />
                </div>

                <div className="space-y-1.5">
                  <label
                    htmlFor="sessionInfoModalContext"
                    className="text-[11px] font-semibold text-muted-foreground"
                  >
                    コンテキスト
                  </label>
                  <textarea
                    id="sessionInfoModalContext"
                    value={editingContext}
                    onChange={(event) => setEditingContext(event.target.value)}
                    rows={4}
                    className="w-full rounded-md border border-border/70 bg-background px-3 py-2 text-sm text-foreground placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-primary/30"
                  />
                </div>

                {sessionInfoError && (
                  <div className="rounded-md border border-rose-200 bg-rose-50 px-3 py-2 text-[11px] text-rose-700">
                    {sessionInfoError}
                  </div>
                )}

                <div className="flex items-center justify-end gap-2">
                  <Button
                    type="button"
                    variant="ghost"
                    size="sm"
                    className="gap-1 text-xs"
                    onClick={handleCancelSessionInfoEdit}
                  >
                    キャンセル
                  </Button>
                  <Button
                    type="submit"
                    size="sm"
                    className="gap-1 text-xs"
                    disabled={savingSessionInfo}
                    isLoading={savingSessionInfo}
                  >
                    保存
                  </Button>
                </div>
              </form>
            </div>
          </div>
        </div>
      )}

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
