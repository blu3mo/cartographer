"use client";

import axios from "axios";
import {
  Bot,
  Brain,
  Check,
  ChevronDown,
  ChevronUp,
  Copy,
  Download,
  FileText,
  Heart,
  Info,
  Loader2,
  Maximize2,
  Send,
  Settings,
  SquareArrowOutUpRight,
  Trash2,
  X,
  Zap,
} from "lucide-react";
import Image from "next/image";
import {
  type ReactElement,
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";
import { toast } from "sonner";

import { ReportTasteSelect } from "@/components/report/ReportTasteSelect";
import { Button } from "@/components/ui/Button";
import {
  Card,
  CardContent,
  CardDescription,
  CardFooter,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { Textarea } from "@/components/ui/textarea";
import { Progress } from "@/components/ui/progress";
import {
  AlertDialog,
  AlertDialogAction,
  AlertDialogCancel,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogHeader,
  AlertDialogTitle,
  AlertDialogTrigger,
} from "@/components/ui/alert-dialog";
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import { ThemeToggle } from "@/components/ThemeToggle";
import { useUserId } from "@/lib/useUserId";
import {
  createResponseLogColumns,
  type ResponseLogRow,
} from "./response-logs-columns";
import { ResponseLogsDataTable } from "./response-logs-data-table";

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

interface ResponseLogStatement {
  id: string;
  text: string;
  orderIndex: number;
}

interface ResponseLogItem {
  statementId: string;
  statementText: string;
  orderIndex: number;
  responseType: "scale" | "free_text" | null;
  value: number | null;
  textResponse: string | null;
  answeredAt: string | null;
}

interface ResponseLogParticipant {
  userId: string;
  name: string;
  joinedAt: string;
  responses: ResponseLogItem[];
}

interface ResponseLogsData {
  sessionId: string;
  participants: ResponseLogParticipant[];
  statements: ResponseLogStatement[];
  totalParticipants: number;
  totalStatements: number;
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
    dot: "bg-status-pending",
    text: "text-status-pending-foreground",
  },
  generating: {
    label: "生成中",
    dot: "bg-status-generating",
    text: "text-status-generating-foreground",
  },
  completed: {
    label: "完了",
    dot: "bg-status-completed",
    text: "text-status-completed-foreground",
  },
  failed: {
    label: "失敗",
    dot: "bg-status-failed",
    text: "text-status-failed-foreground",
  },
};

type ReportTemplateType = "empathy" | "logical" | "psychopath" | "freeform";
type ReportModeType = "auto" | "custom";

interface ReportTemplate {
  id: ReportTemplateType;
  name: string;
  icon: typeof FileText;
  description: string;
  prompt: string;
  samplePreview: string;
  color: string;
}

const REPORT_TEMPLATES: ReportTemplate[] = [
  {
    id: "empathy",
    name: "共感重視",
    icon: Heart,
    description: "感情的ケアを重視した表現",
    prompt:
      "参加者の感情や心理的な側面に配慮し、共感的で温かみのある表現を用いてレポートをまとめてください。対立よりも相互理解を促す視点で記述し、前向きな雰囲気を作ることを意識してください。",
    samplePreview:
      "こんにちは、皆さん。\n\n今回のセッションでは、それぞれの視点や思いを共有していただき、本当にありがとうございました。お一人お一人の意見には、大切な想いや背景があることがよく伝わってきました。\n\n特に印象的だったのは、異なる立場にいる方々が、互いの状況を理解しようと努めていた点です。完全な合意には至らなかったテーマもありますが、それぞれの価値観を尊重し合う姿勢が見られたことは、とても前向きな一歩だと感じます。",
    color: "bg-pink-100 border-pink-200 hover:bg-pink-200 text-pink-900 dark:bg-pink-950/30 dark:border-pink-800 dark:hover:bg-pink-950/50 dark:text-pink-300",
  },
  {
    id: "logical",
    name: "論理重視",
    icon: Brain,
    description: "ビジネス・意思決定向けの分析",
    prompt:
      "論理的で明確な構成を重視し、意思決定に必要な情報を簡潔にまとめてください。データや数値、合意・対立のポイントを明確に示し、次のアクションにつながる提言を含めてください。",
    samplePreview:
      "## エグゼクティブサマリー\n\n本セッションでは、参加者間で以下の3つの重要な対立軸が確認されました：\n\n1. **施策Aの優先順位**：賛成67%、反対33%\n2. **予算配分の方針**：意見が二分（賛成48%、反対52%）\n3. **実施時期**：即時実施派と段階的実施派が対立\n\n## 推奨アクション\n\n1. 施策Aについては支持が過半数を超えているため、早急に実行計画を策定すべき\n2. 予算配分については追加の議論が必要。次回セッションで数値根拠を提示し、再検討を推奨",
    color: "bg-blue-100 border-blue-200 hover:bg-blue-200 text-blue-900 dark:bg-blue-950/30 dark:border-blue-800 dark:hover:bg-blue-950/50 dark:text-blue-300",
  },
  {
    id: "psychopath",
    name: "サイコパスモード",
    icon: Zap,
    description: "論理重視・矛盾を鋭く指摘",
    prompt:
      "感情的な共感や寄り添いは一切廃してください。論理、事実、真実のみでレポートを構成してください。参加者が矛盾した意見を持っている場合は、個人名を明示しながら鋭くその点を指摘してください。曖昧な表現は避け、明確で容赦のない分析を行ってください。",
    samplePreview:
      "## 分析結果\n\n本セッションにおける参加者の回答には、複数の論理的矛盾が検出された。\n\n### 矛盾の指摘\n\n**田中太郎氏**は設問3で「予算削減が必要」と回答しているにもかかわらず、設問7では「全部署への予算増額」に賛成している。この2つの立場は論理的に両立しない。\n\n**佐藤花子氏**は「迅速な意思決定が重要」と主張する一方で、「全員の合意形成を最優先すべき」とも述べている。迅速性と全員合意は多くの場合トレードオフの関係にあり、両方を同時に達成することは現実的ではない。\n\n### 結論\n\n参加者の多くは自身の意見の一貫性を保てていない。感情的な反応に基づく場当たり的な回答が目立つ。",
    color: "bg-purple-100 border-purple-200 hover:bg-purple-200 text-purple-900 dark:bg-purple-950/30 dark:border-purple-800 dark:hover:bg-purple-950/50 dark:text-purple-300",
  },
  {
    id: "freeform",
    name: "自由記述",
    icon: Settings,
    description: "自由に指示を記載",
    prompt: "",
    samplePreview: "",
    color: "bg-muted border-border hover:bg-secondary text-foreground",
  },
];

const REPORT_STYLE_OPTIONS = [
  {
    value: "auto",
    label: "スタンダード",
    icon: <FileText className="h-4 w-4" />,
  },
  {
    value: "empathy",
    label: "共感重視",
    icon: <Heart className="h-4 w-4 text-pink-600 dark:text-pink-500" />,
  },
  {
    value: "logical",
    label: "論理重視",
    icon: <Brain className="h-4 w-4 text-blue-600 dark:text-blue-500" />,
  },
  {
    value: "psychopath",
    label: "サイコパスモード",
    icon: <Zap className="h-4 w-4 text-purple-600 dark:text-purple-500" />,
  },
  {
    value: "freeform",
    label: "自由記述",
    icon: <Settings className="h-4 w-4" />,
  },
];

const EVENT_TYPE_META: Record<
  ThreadEventType,
  { label: string; accent: string; badge: string }
> = {
  plan: {
    label: "Plan",
    accent: "text-sky-700 dark:text-primary",
    badge: "bg-sky-50 text-sky-700 border-sky-200 dark:bg-muted dark:text-foreground dark:border-border",
  },
  survey: {
    label: "Survey",
    accent: "text-emerald-700 dark:text-primary",
    badge: "bg-emerald-50 text-emerald-700 border-emerald-200 dark:bg-muted dark:text-foreground dark:border-border",
  },
  survey_analysis: {
    label: "Analysis",
    accent: "text-purple-700 dark:text-primary",
    badge: "bg-purple-50 text-purple-700 border-purple-200 dark:bg-muted dark:text-foreground dark:border-border",
  },
  user_message: {
    label: "You",
    accent: "text-slate-700 dark:text-primary",
    badge: "bg-slate-50 text-slate-700 border-slate-200 dark:bg-muted dark:text-foreground dark:border-border",
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

const convertResponseLogsToCSV = (logsData: ResponseLogsData): string => {
  // Header row
  const headers = [
    "参加者ID",
    "参加者名",
    "参加日時",
    "設問番号",
    "設問内容",
    "回答タイプ",
    "評価値",
    "自由記述",
    "回答日時",
  ];

  const rows: string[][] = [headers];

  // Data rows
  logsData.participants.forEach((participant) => {
    participant.responses.forEach((response) => {
      const row = [
        participant.userId,
        participant.name,
        participant.joinedAt,
        (response.orderIndex + 1).toString(),
        response.statementText,
        response.responseType ?? "未回答",
        response.value !== null ? response.value.toString() : "",
        response.textResponse ?? "",
        response.answeredAt ?? "",
      ];
      rows.push(row);
    });
  });

  // Convert to CSV format
  return rows
    .map((row) =>
      row
        .map((cell) => {
          // Escape double quotes and wrap in quotes if contains comma, newline, or quote
          const escaped = cell.replace(/"/g, '""');
          if (
            escaped.includes(",") ||
            escaped.includes("\n") ||
            escaped.includes('"')
          ) {
            return `"${escaped}"`;
          }
          return escaped;
        })
        .join(","),
    )
    .join("\n");
};

const downloadCSV = (csvContent: string, filename: string) => {
  // Add BOM for UTF-8 to ensure proper encoding in Excel
  const bom = "\uFEFF";
  const blob = new Blob([bom + csvContent], {
    type: "text/csv;charset=utf-8;",
  });
  const link = document.createElement("a");
  const url = URL.createObjectURL(blob);
  link.setAttribute("href", url);
  link.setAttribute("download", filename);
  link.style.visibility = "hidden";
  document.body.appendChild(link);
  link.click();
  document.body.removeChild(link);
  URL.revokeObjectURL(url);
};

export default function AdminPage({
  sessionId,
  accessToken,
}: {
  sessionId: string;
  accessToken: string;
}) {
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
  const [creatingReport, setCreatingReport] = useState(false);
  const [reportCopyStatus, setReportCopyStatus] = useState<
    "idle" | "copied" | "error"
  >("idle");
  const [showReportModal, setShowReportModal] = useState(false);
  const [, setReportMode] = useState<ReportModeType | null>(null);
  const [selectedTemplate, setSelectedTemplate] =
    useState<ReportTemplateType | null>(null);
  const [customPrompt, setCustomPrompt] = useState("");
  const [selectedReportStyle, setSelectedReportStyle] = useState<
    "auto" | ReportTemplateType
  >("auto");

  const [isGeneratingQuestion, setIsGeneratingQuestion] = useState(false);
  const [showGenerateQuestionDialog, setShowGenerateQuestionDialog] =
    useState(false);
  const [generatedQuestions, setGeneratedQuestions] = useState<string[]>([]);
  const [generateQuestionError, setGenerateQuestionError] = useState<
    string | null
  >(null);
  const [showResponseLog, setShowResponseLog] = useState(false);
  const [responseLogsData, setResponseLogsData] =
    useState<ResponseLogsData | null>(null);
  const [responseLogsLoading, setResponseLogsLoading] = useState(false);
  const [responseLogsError, setResponseLogsError] = useState<string | null>(
    null,
  );

  const fetchAdminData = useCallback(async (withSpinner = true) => {
    if (!userId) return;

    try {
      if (withSpinner) {
        setLoading(true);
      }
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
      if (withSpinner) {
        setLoading(false);
      }
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

  const fetchResponseLogs = useCallback(async () => {
    if (!userId) return;
    setResponseLogsLoading(true);
    try {
      const response = await axios.get(
        `/api/sessions/${sessionId}/${accessToken}/response-logs`,
        {
          headers: { Authorization: `Bearer ${userId}` },
        },
      );
      setResponseLogsData(response.data.data as ResponseLogsData);
      setResponseLogsError(null);
    } catch (err) {
      console.error("Failed to fetch response logs:", err);
      if (axios.isAxiosError(err) && err.response?.status === 403) {
        setResponseLogsError(
          "回答ログの閲覧権限がありません。セッションのホストのみが閲覧できます。",
        );
      } else {
        setResponseLogsError("回答ログの取得に失敗しました。");
      }
    } finally {
      setResponseLogsLoading(false);
    }
  }, [sessionId, accessToken, userId]);

  useEffect(() => {
    if (isUserIdLoading) return;
    void fetchAdminData(true);
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

  // Track report completion and show toast
  const prevReportsRef = useRef<SessionReport[]>([]);
  useEffect(() => {
    const prevReports = prevReportsRef.current;
    prevReportsRef.current = reports;

    // Check if any report just transitioned to "completed"
    reports.forEach((report) => {
      const prevReport = prevReports.find((r) => r.id === report.id);
      if (
        prevReport &&
        (prevReport.status === "pending" || prevReport.status === "generating") &&
        report.status === "completed"
      ) {
        toast.success("レポート生成が完了しました", {
          description: `バージョン ${report.version} のレポートが生成されました`,
          duration: 5000,
        });
      } else if (
        prevReport &&
        (prevReport.status === "pending" || prevReport.status === "generating") &&
        report.status === "failed"
      ) {
        toast.error("レポート生成に失敗しました", {
          description: "もう一度お試しください",
          duration: 5000,
        });
      }
    });
  }, [reports]);

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
        },
        {
          headers: { Authorization: `Bearer ${userId}` },
        },
      );

      const updated = response.data.data as {
        title: string;
        context: string;
        goal: string;
      };

      setData((prev) =>
        prev
          ? {
              ...prev,
              title: updated.title,
              context: updated.context,
              goal: updated.goal,
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

    try {
      setDeleting(true);
      await axios.delete(`/api/sessions/${sessionId}/${accessToken}`, {
        headers: { Authorization: `Bearer ${userId}` },
      });
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

  const handleCloseReportModal = () => {
    setShowReportModal(false);
    setReportMode(null);
    setSelectedTemplate(null);
    setCustomPrompt("");
  };

  const handleSelectTemplate = (templateId: ReportTemplateType) => {
    // 選択状態のみ更新、ピッカーには反映しない
    setSelectedTemplate(templateId);
  };

  const handleConfirmTemplate = () => {
    // 「決定」ボタンを押した時にピッカーに反映
    if (selectedTemplate) {
      setSelectedReportStyle(selectedTemplate);
    } else {
      // selectedTemplate が null の場合はスタンダード
      setSelectedReportStyle("auto");
    }
    handleCloseReportModal();
  };

  const handleGenerateReport = async (
    templateId: ReportTemplateType | null,
  ) => {
    if (!userId) return;
    if (creatingReport) return;

    let finalPrompt = "";

    if (templateId === null) {
      // スタンダード（標準レポート）
      finalPrompt = "";
    } else if (templateId === "freeform") {
      // 自由記述
      finalPrompt = customPrompt;
    } else {
      // テンプレート指定
      const template = REPORT_TEMPLATES.find((t) => t.id === templateId);
      if (!template) return;
      finalPrompt = template.prompt;
    }

    try {
      setCreatingReport(true);
      const response = await axios.post(
        `/api/sessions/${sessionId}/${accessToken}/reports`,
        {
          requestMarkdown: finalPrompt,
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
      setReportsError(null);
      handleCloseReportModal();
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
      setGenerateQuestionError(null);
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
        await fetchAdminData(false);
        setGeneratedQuestions(
          newStatements
            .map((statement: { text?: string }) => statement.text ?? "")
            .filter((text: string) => text.length > 0),
        );
        toast.success("質問生成が完了しました", {
          description: `${newStatements.length}問の質問を追加しました。参加用リンクの画面を再読み込みすると追加の質問に回答できます。`,
          duration: 5000,
        });
      } else {
        setGenerateQuestionError("質問を生成できませんでした。");
      }
    } catch (err) {
      console.error("Failed to generate questions:", err);
      setGenerateQuestionError("質問の生成に失敗しました。");
      toast.error("質問の生成に失敗しました", {
        description: "もう一度お試しください",
        duration: 5000,
      });
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
  const participantNameMap = useMemo(() => {
    const map: Record<string, string> = {};
    participants.forEach((participant) => {
      if (participant.userId) {
        map[participant.userId] = participant.name || "匿名";
      }
    });
    return map;
  }, [participants]);

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
      <div className="min-h-screen flex items-center justify-center bg-background">
        <Loader2 className="h-8 w-8 animate-spin text-muted-foreground dark:text-muted-foreground" />
      </div>
    );
  }

  if (error) {
    return (
      <div className="min-h-screen bg-background">
        <div className="max-w-6xl mx-auto px-6 py-16">
          <Card className="border-red-200/70 bg-red-50/80 dark:border-red-800/70 dark:bg-red-950/50">
            <CardContent className="pt-6">
              <p className="text-red-700 dark:text-red-400">{error}</p>
            </CardContent>
          </Card>
        </div>
      </div>
    );
  }

  if (!data) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-background">
        <p className="text-muted-foreground">セッションが見つかりません。</p>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-background">
      <div className="max-w-[90rem] mx-auto px-6 py-10 space-y-10">
        <header className="space-y-4">
          <div className="flex flex-wrap items-start justify-between gap-4">
            <div className="space-y-2">
              <h1 className="text-3xl font-semibold text-foreground">
                {data.title}
              </h1>
            </div>
            <ThemeToggle />
          </div>
        </header>

        <div className="grid gap-8 lg:grid-cols-[minmax(0,2fr)_minmax(320px,1fr)]">
          <div className="space-y-8">
            <Card>
              <CardHeader>
                <CardTitle>参加者状況の把握</CardTitle>
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
                  <p className="text-sm text-muted-foreground">
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
              <CardFooter className="flex flex-wrap justify-end gap-3 border-t pt-6">
                {canEdit && (
                  <AlertDialog
                    open={showGenerateQuestionDialog}
                    onOpenChange={(open) => {
                      setShowGenerateQuestionDialog(open);
                      if (!open) {
                        setGeneratedQuestions([]);
                        setGenerateQuestionError(null);
                      }
                    }}
                  >
                    <AlertDialogTrigger asChild>
                      <Button
                        variant="outline"
                        size="sm"
                        disabled={isGeneratingQuestion}
                        className="gap-2"
                      >
                        <Bot className="h-4 w-4" />
                        追加質問を生成
                      </Button>
                    </AlertDialogTrigger>
                    <AlertDialogContent>
                      <AlertDialogHeader>
                        <AlertDialogTitle>
                          {generatedQuestions.length > 0
                            ? "追加の質問を生成しました"
                            : "追加の質問を生成しますか？"}
                        </AlertDialogTitle>
                        <AlertDialogDescription>
                          {generatedQuestions.length > 0
                            ? "以下の質問がセッションに追加されました。"
                            : "これまでの回答をもとに、追加で15問の質問を生成します。生成された質問はすぐにセッションへ追加されます。"}
                        </AlertDialogDescription>
                      </AlertDialogHeader>
                      <div className="space-y-3">
                        {generateQuestionError && (
                          <p className="text-xs text-rose-600 dark:text-rose-400">
                            {generateQuestionError}
                          </p>
                        )}
                        {generatedQuestions.length > 0 ? (
                          <>
                            <p className="text-xs font-medium text-muted-foreground">
                              追加された質問のプレビュー（{generatedQuestions.length}問）
                            </p>
                            <div className="max-h-60 space-y-2 overflow-y-auto">
                              {generatedQuestions.map((question, index) => (
                                <div
                                  key={`${question}-${index}`}
                                  className="rounded-lg border border-border/80 bg-background/80 p-3 text-sm text-foreground shadow-sm"
                                >
                                  <p className="text-sm leading-relaxed">
                                    {question}
                                  </p>
                                </div>
                              ))}
                            </div>
                            <p className="text-[11px] text-muted-foreground">
                              参加用リンクの画面を再読み込みすると追加の質問に回答できます。
                            </p>
                          </>
                        ) : (
                          <p className="text-xs text-muted-foreground">
                            生成が完了すると、追加された質問のプレビューがここに表示されます。
                          </p>
                        )}
                      </div>
                      <AlertDialogFooter>
                        {generatedQuestions.length > 0 ? (
                          <AlertDialogCancel>閉じる</AlertDialogCancel>
                        ) : (
                          <>
                            <AlertDialogCancel>キャンセル</AlertDialogCancel>
                            <AlertDialogAction
                              onClick={(event) => {
                                event.preventDefault();
                                void handleGenerateQuestion();
                              }}
                              disabled={isGeneratingQuestion}
                            >
                              {isGeneratingQuestion ? (
                                <>
                                  <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                                  生成中...
                                </>
                              ) : (
                                "生成する"
                              )}
                            </AlertDialogAction>
                          </>
                        )}
                      </AlertDialogFooter>
                    </AlertDialogContent>
                  </AlertDialog>
                )}
                <Button
                  variant="outline"
                  size="sm"
                  onClick={() => {
                    setShowResponseLog(true);
                    void fetchResponseLogs();
                  }}
                  disabled={participants.length === 0}
                  className="gap-2"
                >
                  <FileText className="h-4 w-4" />
                  解答ログを表示
                </Button>
              </CardFooter>
            </Card>

            <Card>
              <CardHeader>
                <div className="flex flex-wrap items-start justify-between gap-4">
                  <div className="space-y-2">
                    <CardTitle>レポート</CardTitle>
                    <CardDescription>
                      参加者の回答をもとに洞察レポートを生成します
                    </CardDescription>
                  </div>
                </div>
              </CardHeader>
              <CardContent className="space-y-6">
                {canEdit ? (
                  <div className="space-y-3 rounded-xl border border-border/50 bg-muted/30 p-4">
                    <ReportTasteSelect
                      value={selectedReportStyle}
                      options={REPORT_STYLE_OPTIONS}
                      onClick={() => {
                        setShowReportModal(true);
                        setReportMode("custom");
                      }}
                      disabled={creatingReport}
                    />

                    <Button
                      type="button"
                      size="lg"
                      onClick={() => {
                        if (selectedReportStyle === "auto") {
                          handleGenerateReport(null);
                        } else if (selectedReportStyle === "freeform") {
                          // 自由記述の場合はモーダルを開く
                          setShowReportModal(true);
                          setReportMode("custom");
                          setSelectedTemplate("freeform");
                        } else {
                          handleGenerateReport(selectedReportStyle);
                        }
                      }}
                      disabled={creatingReport}
                      isLoading={creatingReport}
                      className="h-12 w-full justify-center gap-3 rounded-lg text-base font-semibold shadow-sm transition-all hover:shadow-md"
                    >
                      <FileText className="h-5 w-5" />
                      <span>レポートを生成</span>
                    </Button>
                  </div>
                ) : (
                  <div className="rounded-3xl border border-border/70 bg-muted/80 px-4 py-3 text-xs text-muted-foreground">
                    レポート生成はセッションのホストのみ利用できます。
                  </div>
                )}

                <Dialog open={showReportModal} onOpenChange={setShowReportModal}>
                  <DialogContent className="max-w-4xl max-h-[90vh] overflow-y-auto">
                    <DialogHeader>
                      <DialogTitle className="text-2xl">レポートスタイルを選択</DialogTitle>
                      <DialogDescription>
                        目的に合わせてレポートのスタイルを選択できます
                      </DialogDescription>
                    </DialogHeader>

                      <div className="space-y-6">

                        <div className="flex flex-col gap-6 lg:flex-row">
                          {/* 左側: テンプレートリスト */}
                          <div className="flex-1 space-y-2">
                            {/* スタンダード */}
                            <Card
                              className={`cursor-pointer transition-all hover:shadow-md ${
                                selectedTemplate === null
                                  ? "border-primary bg-primary/5 ring-2 ring-primary ring-offset-2"
                                  : "hover:border-primary/50"
                              } ${creatingReport ? "pointer-events-none opacity-50" : ""}`}
                              onClick={() => !creatingReport && setSelectedTemplate(null)}
                            >
                              <CardContent className="flex items-start gap-3 p-4">
                                <div className="rounded-lg bg-muted p-2">
                                  <FileText className="h-5 w-5 text-muted-foreground" />
                                </div>
                                <div className="flex-1">
                                  <h3 className="font-semibold text-foreground">
                                    スタンダード
                                  </h3>
                                  <p className="mt-0.5 text-xs text-muted-foreground">
                                    標準的な分析レポート
                                  </p>
                                </div>
                              </CardContent>
                            </Card>

                            {REPORT_TEMPLATES.filter(
                              (t) => t.id !== "freeform",
                            ).map((template) => {
                              const Icon = template.icon;
                              const isSelected =
                                selectedTemplate === template.id;
                              return (
                                <Card
                                  key={template.id}
                                  className={`cursor-pointer transition-all hover:shadow-md ${
                                    isSelected
                                      ? "border-primary bg-primary/5 ring-2 ring-primary ring-offset-2"
                                      : "hover:border-primary/50"
                                  } ${creatingReport ? "pointer-events-none opacity-50" : ""}`}
                                  onClick={() => !creatingReport && handleSelectTemplate(template.id)}
                                >
                                  <CardContent className="flex items-start gap-3 p-4">
                                    <div className="rounded-lg bg-muted p-2">
                                      <Icon className="h-5 w-5 text-muted-foreground" />
                                    </div>
                                    <div className="flex-1">
                                      <h3 className="font-semibold text-foreground">
                                        {template.name}
                                      </h3>
                                      <p className="mt-0.5 text-xs text-muted-foreground">
                                        {template.description}
                                      </p>
                                    </div>
                                  </CardContent>
                                </Card>
                              );
                            })}

                            {/* 自由記述は区切り線の下に表示 */}
                            <div className="border-t border-border pt-2 mt-3">
                              <Card
                                className={`cursor-pointer border-dashed transition-all hover:shadow-md ${
                                  selectedTemplate === "freeform"
                                    ? "border-primary bg-primary/5 ring-2 ring-primary ring-offset-2"
                                    : "hover:border-primary/50"
                                } ${creatingReport ? "pointer-events-none opacity-50" : ""}`}
                                onClick={() => !creatingReport && handleSelectTemplate("freeform")}
                              >
                                <CardContent className="flex items-start gap-3 p-4">
                                  <div className="rounded-lg bg-muted p-2">
                                    <Settings className="h-5 w-5 text-muted-foreground" />
                                  </div>
                                  <div className="flex-1">
                                    <h3 className="font-semibold text-foreground">
                                      自由記述
                                    </h3>
                                    <p className="mt-0.5 text-xs text-muted-foreground">
                                      自由に指示を記載
                                    </p>
                                  </div>
                                </CardContent>
                              </Card>
                            </div>
                          </div>

                          {/* 右側: レポートプレビュー */}
                          <div className="flex-1">
                            <Card className="sticky top-0">
                              <CardHeader className="pb-3">
                                <div className="flex items-center justify-between">
                                  <CardTitle className="text-base">
                                    {selectedTemplate === "freeform"
                                      ? "プロンプト"
                                      : "レポートプレビュー"}
                                  </CardTitle>
                                  {selectedTemplate !== "freeform" &&
                                    selectedTemplate !== null && (
                                      <div className="group relative">
                                        <Info className="h-4 w-4 cursor-help text-muted-foreground transition hover:text-foreground" />
                                        <div className="invisible absolute right-0 top-6 z-10 w-80 rounded-lg border border-border bg-popover p-3 text-xs leading-relaxed text-popover-foreground shadow-lg opacity-0 transition-all group-hover:visible group-hover:opacity-100">
                                          <div className="mb-1 font-semibold">
                                            AIへの指示プロンプト:
                                          </div>
                                          {
                                            REPORT_TEMPLATES.find(
                                              (t) => t.id === selectedTemplate,
                                            )?.prompt
                                          }
                                        </div>
                                      </div>
                                    )}
                                </div>
                              </CardHeader>
                              <CardContent className="space-y-3">
                                {selectedTemplate === "freeform" ? (
                                  <>
                                    <Textarea
                                      id="templatePrompt"
                                      value={customPrompt}
                                      onChange={(e) =>
                                        setCustomPrompt(e.target.value)
                                      }
                                      rows={10}
                                      maxLength={5000}
                                      placeholder="例:「共有している価値観について重点的に分析してほしい」「易しい言葉を使った分かりやすいレポートを出力してほしい」"
                                      className="resize-none"
                                    />
                                    <div className="flex items-center justify-between">
                                      <span className="text-xs text-muted-foreground">
                                        {customPrompt.length} / 5000
                                      </span>
                                      <Button
                                        type="button"
                                        onClick={handleConfirmTemplate}
                                        disabled={
                                          creatingReport || !customPrompt.trim()
                                        }
                                        size="sm"
                                      >
                                        決定
                                      </Button>
                                    </div>
                                  </>
                                ) : selectedTemplate ? (
                                  <>
                                    <div className="min-h-[240px] max-h-[400px] overflow-y-auto whitespace-pre-wrap rounded-lg border border-border bg-muted/50 px-4 py-3 text-sm leading-relaxed text-muted-foreground">
                                      {
                                        REPORT_TEMPLATES.find(
                                          (t) => t.id === selectedTemplate,
                                        )?.samplePreview
                                      }
                                    </div>
                                    <div className="flex justify-end">
                                      <Button
                                        type="button"
                                        onClick={handleConfirmTemplate}
                                        disabled={creatingReport}
                                        size="sm"
                                      >
                                        決定
                                      </Button>
                                    </div>
                                  </>
                                ) : (
                                  <>
                                    <div className="min-h-[240px] max-h-[400px] overflow-y-auto whitespace-pre-wrap rounded-lg border border-border bg-muted/50 px-4 py-3 text-sm leading-relaxed text-muted-foreground">
                                      {`## セッション分析レポート

本セッションでは、参加者の回答データに基づいて以下の分析を行いました。

### 主要な発見

参加者の回答傾向を分析した結果、いくつかの重要なパターンが確認されました。合意度の高い項目と対立の見られる項目の両方が存在し、多様な意見が表明されています。

### 次のステップ

今後の議論では、対立点についてさらなる対話を深めることで、相互理解を促進することが推奨されます。`}
                                    </div>
                                    <div className="flex justify-end">
                                      <Button
                                        type="button"
                                        onClick={handleConfirmTemplate}
                                        disabled={creatingReport}
                                        size="sm"
                                      >
                                        決定
                                      </Button>
                                    </div>
                                  </>
                                )}
                              </CardContent>
                            </Card>
                          </div>
                        </div>
                      </div>
                  </DialogContent>
                </Dialog>

                {reportsError && (
                  <div className="rounded-2xl border border-amber-200 bg-amber-50 px-4 py-3 text-sm text-amber-700">
                    {reportsError}
                  </div>
                )}

                {reportsLoading ? (
                  <div className="space-y-4">
                    <div className="h-12 animate-pulse rounded-2xl bg-muted/80" />
                    <div className="h-[420px] animate-pulse rounded-3xl bg-muted/80" />
                  </div>
                ) : reports.length === 0 ? (
                  <div className="rounded-2xl border border-dashed border-border bg-muted px-4 py-6 text-center text-sm text-muted-foreground">
                    まだレポートはありません。ボタンをクリックして作成。
                  </div>
                ) : selectedReport ? (
                  <div className="space-y-6">
                    <div className="flex flex-col gap-4 lg:flex-row lg:items-center lg:justify-between">
                      <div className="space-y-2">
                        <label
                          htmlFor="reportVersionSelect"
                          className="text-xs font-semibold uppercase tracking-[0.18em] text-muted-foreground"
                        >
                          表示するレポートのバージョン
                        </label>
                        <div className="flex flex-wrap items-center gap-3">
                          <select
                            id="reportVersionSelect"
                            value={selectedReportId ?? ""}
                            onChange={(event) =>
                              setSelectedReportId(event.target.value)
                            }
                            className="max-w-xs rounded-2xl border border-border bg-background px-3 py-2 text-sm font-medium text-foreground shadow-sm focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring dark:bg-card"
                          >
                            {reports.map((report) => {
                              return (
                                <option key={report.id} value={report.id}>
                                  v{String(report.version).padStart(2, "0")}
                                </option>
                              );
                            })}
                          </select>
                        </div>
                      </div>
                    </div>

                    {selectedReport.requestMarkdown ? (
                      <div className="rounded-2xl border border-border bg-muted/70 p-4 text-sm text-foreground">
                        <p className="text-[11px] font-semibold uppercase tracking-[0.18em] text-muted-foreground">
                          レポート生成のリクエスト
                        </p>
                        <p className="mt-1 whitespace-pre-wrap leading-relaxed">
                          {selectedReport.requestMarkdown}
                        </p>
                      </div>
                    ) : null}

                    <div className="min-h-[360px] rounded-3xl border border-border bg-card/80 p-6 shadow-inner dark:bg-card/50">
                      <div className="flex h-full flex-col gap-4">
                        <div className="space-y-3">
                          <div className="flex flex-wrap gap-4 text-[11px] uppercase tracking-[0.2em] text-muted-foreground">
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
                              variant="outline"
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
                              <SquareArrowOutUpRight className="h-3.5 w-3.5" />
                              レポート詳細ページを開く
                            </Button>
                          </div>
                        </div>

                        <div className="flex-1 overflow-y-auto rounded-2xl border border-border bg-background/90 p-4 dark:bg-card/90">
                          {selectedReport.status === "completed" &&
                          selectedReport.contentMarkdown ? (
                            <div className="markdown-body prose prose-slate dark:prose-invert max-w-none text-sm leading-relaxed">
                              <ReactMarkdown remarkPlugins={[remarkGfm]}>
                                {selectedReport.contentMarkdown}
                              </ReactMarkdown>
                            </div>
                          ) : selectedReport.status === "failed" ? (
                            <div className="text-sm text-rose-600 dark:text-rose-400">
                              レポート生成に失敗しました。
                              <br />
                              {selectedReport.errorMessage ??
                                "詳細はログを確認してください。"}
                            </div>
                          ) : (
                            <div className="flex h-full flex-col items-center justify-center gap-3 text-sm text-muted-foreground">
                              <Loader2 className="h-5 w-5 animate-spin text-muted-foreground" />
                              <p>レポートを生成しています…</p>
                              <p className="text-[11px] text-muted-foreground">
                                完了まで1~2分ほどかかる場合があります。
                              </p>
                            </div>
                          )}
                        </div>
                      </div>
                    </div>
                  </div>
                ) : (
                  <div className="rounded-2xl border border-dashed border-border/80 bg-white/70 px-4 py-6 text-center text-sm text-muted-foreground">
                    レポートを選択するとここに表示されます。
                  </div>
                )}
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <div className="space-y-2">
                  <CardTitle>回答意見の可視化</CardTitle>
                  <CardDescription>
                    参加者の回答をもとに、合意できている点、対立している点、みんなが分からない点を把握できます
                  </CardDescription>
                </div>
              </CardHeader>

              <CardContent className="space-y-6">
                <div className="grid gap-6 lg:grid-cols-3">
                  <StatementHighlightColumn
                    title="合意度トップ3"
                    tone="emerald"
                    items={statementHighlights.agreement}
                    participantNameMap={participantNameMap}
                  />
                  <StatementHighlightColumn
                    title="対立度トップ3"
                    tone="amber"
                    items={statementHighlights.conflict}
                    participantNameMap={participantNameMap}
                  />
                  <StatementHighlightColumn
                    title="わからない・自信がない度トップ3"
                    tone="slate"
                    items={statementHighlights.dontKnow}
                    participantNameMap={participantNameMap}
                  />
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <div className="flex flex-wrap items-start justify-between gap-4">
                  <div className="space-y-2">
                    <CardTitle>進行ログ</CardTitle>
                    <CardDescription>
                      ファシリテーターAIの進行状況をここから確認できます
                    </CardDescription>
                  </div>
                  <div className="flex flex-col items-end gap-1 text-right">
                    <ThreadStatusToggle
                      shouldProceed={threadData?.thread?.shouldProceed ?? false}
                      onToggle={handleToggleShouldProceed}
                      disabled={!canEdit || !threadData?.thread}
                      isLoading={togglingProceed}
                    />
                    <span className="text-[11px] text-muted-foreground">
                      {threadData?.thread?.shouldProceed
                        ? "全員が回答したら質問を自動生成する"
                        : "全員が回答しても質問を自動生成しない"}
                    </span>
                  </div>
                </div>
              </CardHeader>
              <CardContent className="space-y-6">
                <div className="relative overflow-hidden rounded-3xl border border-border bg-card/70 shadow-inner">
                  {threadLoading && (
                    <div className="absolute inset-x-0 top-0 z-10 bg-gradient-to-b from-background/90 to-background/30 py-2 text-center text-xs text-muted-foreground">
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
                      <p className="text-sm text-muted-foreground">
                        まだイベントはありません。Agentとの会話はここに表示されます。
                      </p>
                    )}
                  </div>
                </div>

                {canEdit && (
                  <div className="space-y-3 rounded-lg border bg-card p-4 shadow-sm">
                    <div className="space-y-1.5">
                      <Label htmlFor="adminMessage" className="text-sm font-medium">
                        ファシリテーターAIへのメッセージ
                      </Label>
                      <p className="text-xs text-muted-foreground">
                        AIに伝えたい情報や指示を入力できます
                      </p>
                    </div>
                    <div className="space-y-2">
                      <Textarea
                        id="adminMessage"
                        value={messageDraft}
                        onChange={(event) => setMessageDraft(event.target.value)}
                        rows={3}
                        className="resize-none"
                        placeholder="例: 次の質問では環境問題に焦点を当ててください"
                      />
                      <div className="flex items-center justify-end gap-2">
                        <Button
                          type="button"
                          onClick={handleSendMessage}
                          disabled={
                            sendingMessage || messageDraft.trim().length === 0
                          }
                          isLoading={sendingMessage}
                          size="sm"
                        >
                          <Send className="h-4 w-4" />
                          送信
                        </Button>
                      </div>
                    </div>
                  </div>
                )}
              </CardContent>
            </Card>
          </div>

          <div className="space-y-8">
            <Card>
              <CardHeader>
                <CardTitle>参加用リンク</CardTitle>
                <CardDescription>
                  共有リンクやQRコードから参加者を招待できます
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-5">
                <div className="space-y-2">
                  <label
                    htmlFor="shareLink"
                    className="text-xs font-medium text-foreground"
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
                <div className="relative rounded-2xl border border-border bg-gradient-to-br from-slate-50 to-white px-6 py-6 text-center shadow-inner dark:border-border dark:from-slate-900/40 dark:to-slate-950/40">
                  {shareQrUrl && (
                    <Button
                      type="button"
                      variant="ghost"
                      size="sm"
                      onClick={() => setIsShareQrFullscreen(true)}
                      className="absolute right-4 top-4 gap-1.5 rounded-full border border-border bg-card px-3 text-xs text-muted-foreground shadow-sm hover:bg-accent"
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
                      unoptimized
                      className="mx-auto h-[176px] w-[176px] rounded-xl border border-border bg-white object-contain p-2 shadow-sm dark:border-border dark:bg-white"
                    />
                  ) : (
                    <div className="mx-auto flex h-[176px] w-[176px] items-center justify-center rounded-xl border border-dashed border-border bg-card text-xs text-muted-foreground">
                      QRコードを生成できませんでした
                    </div>
                  )}
                </div>
              </CardContent>
            </Card>
            <Dialog
              open={isShareQrFullscreen}
              onOpenChange={setIsShareQrFullscreen}
            >
              <DialogContent className="max-w-5xl border-white/10 bg-black/85 p-8 backdrop-blur-sm">
                <DialogHeader>
                  <DialogTitle className="text-3xl font-semibold text-white text-center">
                    QRコードを携帯でスキャン
                  </DialogTitle>
                </DialogHeader>
                {fullscreenQrUrl && (
                  <div className="flex justify-center">
                    <div className="rounded-3xl border border-white/10 bg-white/5 p-4 shadow-2xl backdrop-blur">
                      <Image
                        src={fullscreenQrUrl}
                        alt="参加用QRコード"
                        width={FULLSCREEN_QR_SIZE}
                        height={FULLSCREEN_QR_SIZE}
                        unoptimized
                        className="h-auto w-full max-w-[min(95vw,880px)] rounded-2xl border border-white bg-white p-6 shadow-lg"
                      />
                    </div>
                  </div>
                )}
              </DialogContent>
            </Dialog>

            <Card>
              <CardHeader>
                <div className="flex items-start justify-between gap-4">
                  <div className="space-y-2">
                    <CardTitle>セッション概要の編集</CardTitle>
                    <CardDescription>
                      {canEdit
                        ? "基本情報を編集してアップデートできます"
                        : "セッションの基本情報"}
                    </CardDescription>
                  </div>
                  {canEdit && (
                    <Button
                      variant="outline"
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
                <div className="space-y-4 text-sm text-muted-foreground">
                  <div>
                    <p className="text-xs font-medium text-muted-foreground uppercase tracking-[0.12em] dark:text-muted-foreground">
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
                    <p className="text-xs font-medium text-muted-foreground uppercase tracking-[0.12em] dark:text-muted-foreground">
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
              </CardContent>
            </Card>

            <Dialog open={isEditingSettings} onOpenChange={setIsEditingSettings}>
              <DialogContent className="max-w-2xl max-h-[90vh] overflow-y-auto">
                <DialogHeader>
                  <DialogTitle>セッション情報を編集</DialogTitle>
                  <DialogDescription>
                    セッションの基本情報を更新できます
                  </DialogDescription>
                </DialogHeader>
                <form onSubmit={handleSaveSettings} className="space-y-4">
                  <div className="space-y-1.5">
                    <label
                      htmlFor="sessionTitle"
                      className="text-xs font-medium text-foreground"
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
                    <label
                      htmlFor="sessionGoal"
                      className="text-xs font-medium text-foreground"
                    >
                      ゴール
                    </label>
                    <Textarea
                      id="sessionGoal"
                      value={editingGoal}
                      onChange={(event) => setEditingGoal(event.target.value)}
                      required
                      rows={5}
                      className="rounded-xl resize-none"
                    />
                  </div>

                  <div className="space-y-1.5">
                    <label
                      htmlFor="sessionContext"
                      className="text-xs font-medium text-foreground"
                    >
                      背景情報
                    </label>
                    <Textarea
                      id="sessionContext"
                      value={editingContext}
                      onChange={(event) =>
                        setEditingContext(event.target.value)
                      }
                      rows={5}
                      className="rounded-xl resize-none"
                    />
                  </div>

                  {(settingsMessage || settingsError) && (
                    <div
                      className={`rounded-xl px-3 py-2 text-xs ${
                        settingsError
                          ? "bg-red-50 text-red-600 dark:bg-red-900/20 dark:text-red-400"
                          : "bg-emerald-50 text-emerald-700 dark:bg-emerald-900/20 dark:text-emerald-400"
                      }`}
                    >
                      {settingsError ?? settingsMessage}
                    </div>
                  )}

                  <DialogFooter>
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
                        }
                      }}
                      className="gap-1.5 text-xs"
                    >
                      キャンセル
                    </Button>
                    <Button
                      type="submit"
                      disabled={isSavingSettings}
                      isLoading={isSavingSettings}
                      size="sm"
                      className="gap-1.5 text-xs"
                    >
                      保存
                    </Button>
                  </DialogFooter>
                </form>
              </DialogContent>
            </Dialog>

            <Card>
              <CardHeader>
                <CardTitle>セッション管理</CardTitle>
              </CardHeader>
              <CardContent className="space-y-6">
                {canEdit && (
                  <AlertDialog>
                    <AlertDialogTrigger asChild>
                      <Button
                        disabled={deleting}
                        variant="destructive"
                        size="sm"
                        className="gap-1.5"
                      >
                        <Trash2 className="h-4 w-4" />
                        セッションを削除
                      </Button>
                    </AlertDialogTrigger>
                    <AlertDialogContent>
                      <AlertDialogHeader>
                        <AlertDialogTitle>セッションを削除しますか？</AlertDialogTitle>
                        <AlertDialogDescription>
                          この操作は取り消せません。全てのデータが完全に削除されます。
                        </AlertDialogDescription>
                      </AlertDialogHeader>
                      <AlertDialogFooter>
                        <AlertDialogCancel>キャンセル</AlertDialogCancel>
                        <AlertDialogAction
                          onClick={handleDeleteSession}
                          disabled={deleting}
                          className="bg-destructive text-destructive-foreground hover:bg-destructive/90"
                        >
                          {deleting ? (
                            <>
                              <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                              削除中...
                            </>
                          ) : (
                            "削除する"
                          )}
                        </AlertDialogAction>
                      </AlertDialogFooter>
                    </AlertDialogContent>
                  </AlertDialog>
                )}
              </CardContent>
            </Card>
          </div>
        </div>

        {/* Response Log Modal */}
        {showResponseLog && (
          <div className="fixed inset-0 z-50 m-0 flex items-center justify-center bg-black/85 p-4 backdrop-blur-sm">
            <button
              type="button"
              aria-label="モーダルを閉じる"
              className="absolute inset-0 z-0 h-full w-full cursor-pointer bg-transparent focus:outline-none"
              onClick={() => setShowResponseLog(false)}
              onKeyDown={(event) => {
                if (event.key === "Escape") {
                  event.preventDefault();
                  setShowResponseLog(false);
                }
              }}
            />
            <div
              className="relative z-10 w-full max-w-7xl max-h-[90vh] overflow-hidden rounded-2xl border border-border bg-background shadow-2xl"
              role="dialog"
              aria-modal="true"
              aria-label="回答ログ"
            >
              <div className="flex items-center justify-between border-b border-border px-6 py-4">
                <div>
                  <h2 className="text-xl font-semibold text-foreground">
                    解答ログ
                  </h2>
                  <p className="mt-1 text-sm text-muted-foreground">
                    全参加者の詳細な回答データを確認・エクスポートできます
                  </p>
                </div>
                <div className="flex items-center gap-2">
                  {responseLogsData && (
                    <Button
                      variant="outline"
                      size="sm"
                      onClick={() => {
                        const csvContent =
                          convertResponseLogsToCSV(responseLogsData);
                        const timestamp = new Date()
                          .toISOString()
                          .slice(0, 19)
                          .replace(/:/g, "-");
                        downloadCSV(
                          csvContent,
                          `response-logs-${sessionId}-${timestamp}.csv`,
                        );
                      }}
                      className="gap-2"
                    >
                      <Download className="h-4 w-4" />
                      CSVエクスポート
                    </Button>
                  )}
                  <Button
                    variant="ghost"
                    size="icon"
                    onClick={() => setShowResponseLog(false)}
                    className="text-muted-foreground hover:bg-muted hover:text-muted-foreground"
                  >
                    <X className="h-5 w-5" />
                  </Button>
                </div>
              </div>
              <div
                className="overflow-y-auto px-6 py-6"
                style={{ maxHeight: "calc(90vh - 80px)" }}
              >
                {responseLogsLoading ? (
                  <div className="flex flex-col items-center justify-center py-12">
                    <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
                    <p className="mt-3 text-sm text-muted-foreground">読み込み中...</p>
                  </div>
                ) : responseLogsError ? (
                  <div className="flex flex-col items-center justify-center py-12">
                    <Info className="h-8 w-8 text-rose-400" />
                    <p className="mt-3 text-sm text-rose-600">
                      {responseLogsError}
                    </p>
                  </div>
                ) : responseLogsData ? (
                  <div className="space-y-6">
                    <div className="grid gap-4 sm:grid-cols-3">
                      <div className="rounded-xl border border-border bg-card px-4 py-3">
                        <p className="text-xs font-medium text-muted-foreground uppercase tracking-wider">
                          参加者数
                        </p>
                        <p className="mt-1 text-2xl font-semibold text-card-foreground">
                          {responseLogsData.totalParticipants}人
                        </p>
                      </div>
                      <div className="rounded-xl border border-border bg-card px-4 py-3">
                        <p className="text-xs font-medium text-muted-foreground uppercase tracking-wider">
                          設問数
                        </p>
                        <p className="mt-1 text-2xl font-semibold text-card-foreground">
                          {responseLogsData.totalStatements}問
                        </p>
                      </div>
                      <div className="rounded-xl border border-border bg-card px-4 py-3">
                        <p className="text-xs font-medium text-muted-foreground uppercase tracking-wider">
                          総回答数
                        </p>
                        <p className="mt-1 text-2xl font-semibold text-card-foreground">
                          {responseLogsData.participants.reduce(
                            (sum, p) =>
                              sum +
                              p.responses.filter((r) => r.responseType !== null)
                                .length,
                            0,
                          )}
                          件
                        </p>
                      </div>
                    </div>

                    <ResponseLogsDataTable
                      columns={createResponseLogColumns(
                        responseLogsData.statements,
                      )}
                      data={responseLogsData.participants.map(
                        (participant): ResponseLogRow => {
                          const row: ResponseLogRow = {
                            participantUserId: participant.userId,
                            participantName: participant.name,
                            joinedAt: participant.joinedAt,
                          };
                          participant.responses.forEach((response) => {
                            row[`statement_${response.statementId}`] = response;
                          });
                          return row;
                        },
                      )}
                    />
                  </div>
                ) : null}
              </div>
            </div>
          </div>
        )}
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
  const toneClass = "bg-card border-border text-foreground";
  return (
    <div className={`rounded-2xl border px-4 py-4 shadow-sm ${toneClass}`}>
      <p className="text-[11px] uppercase tracking-[0.12em] text-muted-foreground">
        {label}
      </p>
      <p className="mt-2 text-xl font-semibold">{value}</p>
      {subLabel && <p className="mt-2 text-xs text-muted-foreground">{subLabel}</p>}
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
    <div className="flex h-full flex-col gap-2 rounded-xl border border-border bg-card p-3 shadow-sm">
      <div className="flex flex-wrap items-center gap-x-3 gap-y-1">
        <div className="min-w-0 flex-1">
          <p className="truncate text-sm font-medium text-foreground">
            {participant.name || "名称未設定"}
          </p>
          <p className="text-[10px] text-muted-foreground dark:text-muted-foreground">{updatedLabel}に参加</p>
        </div>
        <div className="text-right">
          <p className="text-sm font-semibold text-foreground">
            {completionLabel}
          </p>
          <p className="text-[10px] text-muted-foreground">
            {participant.answeredCount}/{participant.totalStatements}
          </p>
        </div>
      </div>
      <Progress value={progressRatio} className="mt-2 h-1" />
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
  participantNameMap: Record<string, string>;
}

function StatementHighlightColumn({
  title,
  tone,
  items,
  participantNameMap,
}: StatementHighlightColumnProps) {
  const toneClass =
    tone === "emerald"
      ? "bg-blue-50 border-blue-200 dark:bg-blue-950/20 dark:border-blue-800/50"
      : tone === "amber"
        ? "bg-purple-50 border-purple-200 dark:bg-purple-950/20 dark:border-purple-800/50"
        : "bg-amber-50 border-amber-200 dark:bg-amber-950/20 dark:border-amber-800/50";

  const badgeClass =
    tone === "emerald"
      ? "bg-blue-100 text-blue-700 dark:bg-blue-900/50 dark:text-blue-300"
      : tone === "amber"
        ? "bg-purple-100 text-purple-700 dark:bg-purple-900/50 dark:text-purple-300"
        : "bg-amber-100 text-amber-700 dark:bg-amber-900/50 dark:text-amber-300";

  return (
    <div className="space-y-3">
      <h3 className="text-sm font-semibold text-foreground">{title}</h3>
      {items.length === 0 ? (
        <p className="text-xs text-muted-foreground">
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
              <div className="text-[11px] text-muted-foreground">
                回答率 {formatPercentage(item.responseRate)}
              </div>
            </div>
            <p className="mt-3 text-sm text-foreground leading-relaxed">
              {item.statement.text}
            </p>
            <div className="mt-4 flex items-center gap-3 text-[11px] text-muted-foreground">
              <span className="font-medium text-blue-700 dark:text-blue-400">
                Yes {formatPercentage(item.positive)}
              </span>
              <span className="font-medium text-purple-700 dark:text-purple-400">
                No {formatPercentage(item.negative)}
              </span>
              <span className="font-medium text-amber-700 dark:text-amber-400">わからない・自信がない {formatPercentage(item.neutral)}</span>
            </div>
            {item.statement.responses.freeTextCount > 0 && (
              <div className="mt-3 rounded-xl border border-border bg-muted/70 px-3 py-2 text-[11px] text-muted-foreground shadow-inner">
                <div className="flex items-center justify-between gap-2">
                  <span className="font-semibold text-foreground">
                    自由記述 {item.statement.responses.freeTextCount}件
                  </span>
                </div>
                <ul className="mt-2 space-y-1">
                  {item.statement.responses.freeTextSamples.map(
                    (sample, sampleIndex) => (
                      <li
                        key={`${item.statement.id}-sample-${sampleIndex}`}
                        className="text-muted-foreground whitespace-pre-wrap break-words"
                      >
                        ・{sample.text}
                        <span className="ml-2 text-[10px] text-muted-foreground dark:text-muted-foreground">
                          —{" "}
                          {sample.participantUserId
                            ? (participantNameMap[sample.participantUserId] ??
                              "送信者不明")
                            : "送信者不明"}
                        </span>
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

function ThreadStatusToggle({
  shouldProceed,
  onToggle,
  disabled,
  isLoading,
}: {
  shouldProceed: boolean;
  onToggle: () => void;
  disabled: boolean;
  isLoading: boolean;
}) {
  const statusDescription = shouldProceed
    ? "新規Statementの自動生成: 全員が回答を終えると、新しい質問が生成されます"
    : "新規Statementの自動生成: 全員が回答を終えても、新しい質問は生成されません";
  const isDisabled = disabled || isLoading;

  return (
    <div className="flex items-center gap-2">
      <button
        type="button"
        role="switch"
        aria-checked={shouldProceed}
        data-state={shouldProceed ? "checked" : "unchecked"}
        onClick={onToggle}
        disabled={isDisabled}
        aria-label="新規Statementの自動生成を切り替え"
        title={statusDescription}
        className="peer inline-flex h-5 w-9 shrink-0 cursor-pointer items-center rounded-full border-2 border-transparent shadow-sm transition-colors focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2 focus-visible:ring-offset-background disabled:cursor-not-allowed disabled:opacity-50 data-[state=checked]:bg-primary data-[state=unchecked]:bg-input"
      >
        <span
          data-state={shouldProceed ? "checked" : "unchecked"}
          className="pointer-events-none block h-4 w-4 rounded-full bg-background shadow-lg ring-0 transition-transform data-[state=checked]:translate-x-4 data-[state=unchecked]:translate-x-0"
        />
      </button>
      {isLoading && (
        <Loader2 className="h-3.5 w-3.5 animate-spin text-muted-foreground" />
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
    "markdown-body prose prose-sm dark:prose-invert max-w-none text-foreground [&_ol]:list-decimal [&_ul]:list-disc";
  const fadeGradientClass = isHostMessage
    ? "from-primary/5 via-primary/3 to-transparent dark:from-primary/5 dark:via-primary/3"
    : "from-card/95 via-card/60 to-transparent dark:from-card/95 dark:via-card/60";

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
    accent: "text-muted-foreground",
    badge: "bg-muted text-muted-foreground border-border",
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
        <p className="text-base text-foreground">
          新しく{totalSurveyStatements}
          個の質問を作成しました。皆さんの回答をお待ちしています。
        </p>
      ) : null}
      {visibleStatements.map((statement) => (
        <div
          key={statement.id}
          className="rounded-2xl border border-border bg-card/90 px-3 py-2 text-sm text-foreground shadow-sm"
        >
          <span className="mr-2 text-[11px] font-medium text-muted-foreground">
            #{statement.orderIndex + 1}
          </span>
          {statement.text}
        </div>
      ))}
      {/* {!expanded && event.statements.length > visibleStatements.length && (
        <p className="text-[11px] text-muted-foreground">
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
      className="inline-flex items-center gap-1 text-[11px] font-medium text-muted-foreground transition-colors hover:text-foreground"
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
      <p className="text-sm text-muted-foreground">内容を準備中です。</p>,
    );
  })();

  return (
    <div
      className={`flex gap-3 ${
        isHostMessage ? "justify-end" : "justify-start"
      }`}
    >
      {!isHostMessage && (
        <div className="mt-2 flex h-8 w-8 flex-shrink-0 items-center justify-center rounded-full bg-primary text-primary-foreground shadow-sm">
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
          <span className="text-[10px] text-muted-foreground">
            #{String(event.orderIndex).padStart(3, "0")}・
            {formatDateTime(event.updatedAt)}
          </span>
        </div>
        <div
          className={`w-full rounded-3xl border px-4 py-3 shadow-sm ${
            isHostMessage
              ? "border-border bg-muted/80"
              : "border-border bg-card/90"
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
          <div className="flex w-full items-center gap-3 text-[11px] text-muted-foreground">
            <div className="h-1.5 w-full rounded-full bg-secondary">
              <div
                className={`h-full rounded-full ${
                  isHostMessage ? "bg-indigo-400" : "bg-muted0"
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
