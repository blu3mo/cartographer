"use client";

import axios from "axios";
import {
  ChevronDown,
  Copy,
  Loader2,
  MessageCircle,
  Pause,
  Play,
  QrCode,
  RefreshCcw,
  Trash2,
  Users,
} from "lucide-react";
import QRCodeStyling from "qr-code-styling";
import { use, useCallback, useEffect, useRef, useState } from "react";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";

import { Button } from "@/components/ui/Button";
import { Card, CardContent } from "@/components/ui/card";
import { useUserId } from "@/lib/useUserId";

interface ResponseStats {
  strongYes: number;
  yes: number;
  dontKnow: number;
  no: number;
  strongNo: number;
  totalCount: number;
}

interface StatementWithStats {
  id: string;
  sessionId: string;
  text: string;
  orderIndex: number;
  responses: ResponseStats;
  agreementScore: number;
}

interface ParticipantWithProgress {
  userId: string;
  name: string;
  responseCount: number;
  totalStatements: number;
  progressPercent: number;
  createdAt: string;
}

interface SessionAdminData {
  id: string;
  title: string;
  context: string;
  goal: string;
  isPublic: boolean;
  createdAt: string;
  statements: StatementWithStats[];
  participants: ParticipantWithProgress[];
}

type ThreadEventType = "plan" | "survey" | "survey_analysis" | "user_message";

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
  progress: number;
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

const EVENT_TYPE_META: Record<
  ThreadEventType,
  { label: string; icon: string; color: string }
> = {
  plan: {
    label: "Plan",
    icon: "📋",
    color: "text-blue-600 bg-blue-50",
  },
  survey: {
    label: "Survey",
    icon: "📊",
    color: "text-emerald-600 bg-emerald-50",
  },
  survey_analysis: {
    label: "Analysis",
    icon: "🔍",
    color: "text-purple-600 bg-purple-50",
  },
  user_message: {
    label: "Message",
    icon: "💬",
    color: "text-amber-600 bg-amber-50",
  },
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

export default function AdminPage({
  params,
}: {
  params: Promise<{ sessionId: string }>;
}) {
  const { sessionId } = use(params);
  const { userId, isLoading: isUserIdLoading } = useUserId();
  const [data, setData] = useState<SessionAdminData | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [deleting, setDeleting] = useState(false);

  const [threadData, setThreadData] = useState<EventThreadResponse | null>(
    null,
  );
  const [threadLoading, setThreadLoading] = useState(true);
  const [threadError, setThreadError] = useState<string | null>(null);
  const [messageDraft, setMessageDraft] = useState("");
  const [sendingMessage, setSendingMessage] = useState(false);
  const [togglingProceed, setTogglingProceed] = useState(false);
  const [expandedEvents, setExpandedEvents] = useState<Record<string, boolean>>(
    {},
  );

  const [showQRCode, setShowQRCode] = useState(false);
  const [copiedLink, setCopiedLink] = useState(false);
  const qrCodeRef = useRef<HTMLDivElement>(null);

  const sessionUrl =
    typeof window !== "undefined"
      ? `${window.location.origin}/sessions/${sessionId}`
      : "";

  const fetchAdminData = useCallback(async () => {
    if (!userId) {
      return;
    }

    try {
      setLoading(true);
      const response = await axios.get(`/api/sessions/${sessionId}/admin`, {
        headers: {
          Authorization: `Bearer ${userId}`,
        },
      });
      const responseData = response.data.data;
      setData({
        id: responseData.id,
        title: responseData.title,
        context: responseData.context,
        goal: responseData.goal ?? "",
        isPublic: responseData.isPublic,
        createdAt: responseData.createdAt,
        statements: responseData.statements ?? [],
        participants: responseData.participants ?? [],
      });
      setError(null);
    } catch (err: unknown) {
      console.error("Failed to fetch admin data:", err);
      if (axios.isAxiosError(err) && err.response?.status === 403) {
        setError("このセッションの管理権限がありません。");
      } else {
        setError("データの取得に失敗しました。");
      }
    } finally {
      setLoading(false);
    }
  }, [sessionId, userId]);

  const fetchEventThread = useCallback(
    async (withSpinner = false) => {
      if (!userId) {
        return;
      }
      if (withSpinner) {
        setThreadLoading(true);
      }
      try {
        const response = await axios.get(
          `/api/sessions/${sessionId}/event-thread`,
          {
            headers: {
              Authorization: `Bearer ${userId}`,
            },
          },
        );
        setThreadData(response.data);
        setThreadError(null);
      } catch (err: unknown) {
        console.error("Failed to fetch event thread:", err);
        setThreadError("Event Threadの取得に失敗しました。");
      } finally {
        setThreadLoading(false);
      }
    },
    [sessionId, userId],
  );

  useEffect(() => {
    if (isUserIdLoading) {
      return;
    }
    void fetchAdminData();
  }, [fetchAdminData, isUserIdLoading]);

  useEffect(() => {
    if (isUserIdLoading || !userId) {
      return;
    }
    void fetchEventThread(true);
    const intervalId = window.setInterval(() => {
      void fetchEventThread();
    }, 5000);
    return () => window.clearInterval(intervalId);
  }, [fetchEventThread, isUserIdLoading, userId]);

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
    if (data?.title) {
      document.title = `${data.title} - 管理画面 - Cartographer`;
    }
    return () => {
      document.title = "Cartographer";
    };
  }, [data]);

  useEffect(() => {
    if (showQRCode && qrCodeRef.current && sessionUrl) {
      qrCodeRef.current.innerHTML = "";
      const qrCode = new QRCodeStyling({
        width: 200,
        height: 200,
        data: sessionUrl,
        margin: 10,
        qrOptions: {
          typeNumber: 0,
          mode: "Byte",
          errorCorrectionLevel: "Q",
        },
        imageOptions: {
          hideBackgroundDots: true,
          imageSize: 0.4,
          margin: 0,
        },
        dotsOptions: {
          color: "#000000",
          type: "rounded",
        },
        backgroundOptions: {
          color: "#ffffff",
        },
        cornersSquareOptions: {
          color: "#000000",
          type: "extra-rounded",
        },
        cornersDotOptions: {
          color: "#000000",
          type: "dot",
        },
      });
      qrCode.append(qrCodeRef.current);
    }
  }, [showQRCode, sessionUrl]);

  const handleCopyLink = async () => {
    try {
      await navigator.clipboard.writeText(sessionUrl);
      setCopiedLink(true);
      setTimeout(() => setCopiedLink(false), 2000);
    } catch (err) {
      console.error("Failed to copy link:", err);
    }
  };

  const handleSendMessage = async () => {
    if (!userId || messageDraft.trim().length === 0) {
      return;
    }
    setSendingMessage(true);
    try {
      await axios.post(
        `/api/sessions/${sessionId}/event-thread/events/user-message`,
        { markdown: messageDraft },
        {
          headers: {
            Authorization: `Bearer ${userId}`,
          },
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
    if (!userId || !threadData?.thread) {
      return;
    }

    setTogglingProceed(true);
    try {
      const response = await axios.patch(
        `/api/sessions/${sessionId}/event-thread/should-proceed`,
        {
          shouldProceed: !threadData.thread.shouldProceed,
        },
        {
          headers: {
            Authorization: `Bearer ${userId}`,
          },
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
      alert("shouldProceedの更新に失敗しました。");
    } finally {
      setTogglingProceed(false);
    }
  };

  const handleDeleteSession = async () => {
    if (
      !confirm(
        "このセッションを完全に削除しますか？\n\nこの操作は取り消せません。すべてのデータが永久に失われます。",
      )
    ) {
      return;
    }

    try {
      setDeleting(true);
      await axios.delete(`/api/sessions/${sessionId}/admin`, {
        headers: {
          Authorization: `Bearer ${userId}`,
        },
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

  const getTopStatements = (
    statements: StatementWithStats[],
    type: "agreement" | "conflict" | "unknown",
  ): StatementWithStats[] => {
    const sorted = [...statements].sort((a, b) => {
      if (type === "agreement") {
        const aAgree = a.responses.strongYes + a.responses.yes;
        const bAgree = b.responses.strongYes + b.responses.yes;
        return bAgree - aAgree;
      } else if (type === "conflict") {
        return b.agreementScore - a.agreementScore;
      } else {
        return b.responses.dontKnow - a.responses.dontKnow;
      }
    });
    return sorted.slice(0, 3);
  };

  if (isUserIdLoading || loading) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-[#fafafa]">
        <Loader2 className="h-8 w-8 animate-spin text-gray-400" />
      </div>
    );
  }

  if (error) {
    return (
      <div className="min-h-screen bg-[#fafafa]">
        <div className="max-w-5xl mx-auto px-6 py-12">
          <Card className="border-red-200 bg-red-50">
            <CardContent className="pt-6">
              <p className="text-red-700 text-sm">{error}</p>
            </CardContent>
          </Card>
        </div>
      </div>
    );
  }

  if (!data) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-[#fafafa]">
        <p className="text-gray-500 text-sm">セッションが見つかりません。</p>
      </div>
    );
  }

  const topAgreement = getTopStatements(data.statements, "agreement");
  const topConflict = getTopStatements(data.statements, "conflict");
  const topUnknown = getTopStatements(data.statements, "unknown");

  return (
    <div className="min-h-screen bg-[#fafafa]">
      <div className="max-w-5xl mx-auto px-6 py-8 space-y-6">
        <div className="space-y-2">
          <h1 className="text-3xl font-semibold text-gray-900 tracking-tight">
            {data.title}
          </h1>
          <p className="text-sm text-gray-500">管理画面</p>
        </div>

        <div className="bg-white rounded-lg border border-gray-200 shadow-sm p-6 space-y-4">
          <div className="flex items-center justify-between">
            <h2 className="text-base font-semibold text-gray-900">
              セッションリンク
            </h2>
            <Button
              variant="ghost"
              size="sm"
              onClick={() => setShowQRCode(!showQRCode)}
              className="text-xs h-8 gap-1.5"
            >
              <QrCode className="h-3.5 w-3.5" />
              {showQRCode ? "QRコードを隠す" : "QRコードを表示"}
            </Button>
          </div>

          <div className="flex items-center gap-3">
            <div className="flex-1 bg-gray-50 rounded-md px-4 py-2.5 border border-gray-200">
              <code className="text-sm text-gray-700 break-all">
                {sessionUrl}
              </code>
            </div>
            <Button
              variant="outline"
              size="sm"
              onClick={handleCopyLink}
              className="gap-1.5 h-9"
            >
              <Copy className="h-3.5 w-3.5" />
              {copiedLink ? "コピーしました！" : "コピー"}
            </Button>
          </div>

          {showQRCode && (
            <div className="flex justify-center pt-2 pb-2">
              <div
                ref={qrCodeRef}
                className="bg-white p-4 rounded-lg border border-gray-200"
              />
            </div>
          )}
        </div>

        <div className="bg-white rounded-lg border border-gray-200 shadow-sm p-6 space-y-4">
          <div className="flex items-center gap-2">
            <Users className="h-4 w-4 text-gray-600" />
            <h2 className="text-base font-semibold text-gray-900">
              参加者モニタリング
            </h2>
          </div>

          <div className="grid grid-cols-1 gap-3">
            <div className="flex items-center justify-between py-2 px-3 bg-gray-50 rounded-md">
              <span className="text-sm text-gray-600">参加者数</span>
              <span className="text-lg font-semibold text-gray-900">
                {data.participants.length}人
              </span>
            </div>

            {data.participants.length > 0 ? (
              <div className="space-y-2">
                {data.participants.map((participant) => (
                  <div
                    key={participant.userId}
                    className="flex items-center justify-between py-3 px-4 bg-gray-50 rounded-md border border-gray-100"
                  >
                    <div className="flex-1 min-w-0">
                      <p className="text-sm font-medium text-gray-900 truncate">
                        {participant.name}
                      </p>
                      <p className="text-xs text-gray-500 mt-0.5">
                        {participant.responseCount} /{" "}
                        {participant.totalStatements} 回答済み
                      </p>
                    </div>
                    <div className="flex items-center gap-3 ml-4">
                      <div className="w-32 h-2 bg-gray-200 rounded-full overflow-hidden">
                        <div
                          className="h-full bg-emerald-500 transition-all duration-300"
                          style={{ width: `${participant.progressPercent}%` }}
                        />
                      </div>
                      <span className="text-sm font-medium text-gray-700 w-12 text-right">
                        {Math.round(participant.progressPercent)}%
                      </span>
                    </div>
                  </div>
                ))}
              </div>
            ) : (
              <div className="py-8 text-center">
                <p className="text-sm text-gray-500">まだ参加者がいません</p>
              </div>
            )}
          </div>
        </div>

        <div className="bg-white rounded-lg border border-gray-200 shadow-sm p-6 space-y-4">
          <h2 className="text-base font-semibold text-gray-900">
            Statement概要
          </h2>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <div className="space-y-2">
              <div className="flex items-center gap-2">
                <div className="w-2 h-2 rounded-full bg-emerald-500" />
                <h3 className="text-sm font-medium text-gray-700">
                  合意度トップ3
                </h3>
              </div>
              <div className="space-y-2">
                {topAgreement.map((statement, index) => (
                  <div
                    key={statement.id}
                    className="p-3 bg-emerald-50 rounded-md border border-emerald-100"
                  >
                    <div className="flex items-start gap-2">
                      <span className="text-xs font-semibold text-emerald-700 mt-0.5">
                        #{index + 1}
                      </span>
                      <p className="text-xs text-gray-700 leading-relaxed flex-1">
                        {statement.text}
                      </p>
                    </div>
                    <div className="mt-2 text-xs text-emerald-600 font-medium">
                      {Math.round(
                        statement.responses.strongYes + statement.responses.yes,
                      )}
                      % 賛成
                    </div>
                  </div>
                ))}
              </div>
            </div>

            <div className="space-y-2">
              <div className="flex items-center gap-2">
                <div className="w-2 h-2 rounded-full bg-red-500" />
                <h3 className="text-sm font-medium text-gray-700">
                  対立度トップ3
                </h3>
              </div>
              <div className="space-y-2">
                {topConflict.map((statement, index) => (
                  <div
                    key={statement.id}
                    className="p-3 bg-red-50 rounded-md border border-red-100"
                  >
                    <div className="flex items-start gap-2">
                      <span className="text-xs font-semibold text-red-700 mt-0.5">
                        #{index + 1}
                      </span>
                      <p className="text-xs text-gray-700 leading-relaxed flex-1">
                        {statement.text}
                      </p>
                    </div>
                    <div className="mt-2 text-xs text-red-600 font-medium">
                      対立スコア: {statement.agreementScore}
                    </div>
                  </div>
                ))}
              </div>
            </div>

            <div className="space-y-2">
              <div className="flex items-center gap-2">
                <div className="w-2 h-2 rounded-full bg-amber-500" />
                <h3 className="text-sm font-medium text-gray-700">
                  わからない度トップ3
                </h3>
              </div>
              <div className="space-y-2">
                {topUnknown.map((statement, index) => (
                  <div
                    key={statement.id}
                    className="p-3 bg-amber-50 rounded-md border border-amber-100"
                  >
                    <div className="flex items-start gap-2">
                      <span className="text-xs font-semibold text-amber-700 mt-0.5">
                        #{index + 1}
                      </span>
                      <p className="text-xs text-gray-700 leading-relaxed flex-1">
                        {statement.text}
                      </p>
                    </div>
                    <div className="mt-2 text-xs text-amber-600 font-medium">
                      {Math.round(statement.responses.dontKnow)}% わからない
                    </div>
                  </div>
                ))}
              </div>
            </div>
          </div>

          <div className="pt-2">
            <Button
              variant="outline"
              size="sm"
              disabled
              className="text-xs h-8 gap-1.5 text-gray-400 cursor-not-allowed"
            >
              Statement一覧ページへ（未実装）
            </Button>
          </div>
        </div>

        <div className="bg-white rounded-lg border border-gray-200 shadow-sm overflow-hidden">
          <div className="p-6 border-b border-gray-200">
            <div className="flex items-center justify-between">
              <div className="flex items-center gap-2">
                <MessageCircle className="h-4 w-4 text-gray-600" />
                <h2 className="text-base font-semibold text-gray-900">
                  チャット
                </h2>
              </div>
              <Button
                variant="ghost"
                size="sm"
                onClick={() => fetchEventThread(true)}
                disabled={threadLoading}
                className="text-xs h-8 gap-1.5"
              >
                <RefreshCcw
                  className={`h-3.5 w-3.5 ${threadLoading ? "animate-spin" : ""}`}
                />
                更新
              </Button>
            </div>
          </div>

          <div className="h-[500px] overflow-y-auto p-6 bg-gray-50">
            {threadLoading && !threadData ? (
              <div className="flex items-center justify-center h-full">
                <Loader2 className="h-6 w-6 animate-spin text-gray-400" />
              </div>
            ) : threadError ? (
              <div className="flex items-center justify-center h-full">
                <p className="text-sm text-red-600">{threadError}</p>
              </div>
            ) : threadData?.events.length ? (
              <div className="space-y-4">
                {threadData.events.map((event) => (
                  <ChatMessage
                    key={event.id}
                    event={event}
                    expanded={Boolean(expandedEvents[event.id])}
                    onToggle={() =>
                      setExpandedEvents((prev) => ({
                        ...prev,
                        [event.id]: !prev[event.id],
                      }))
                    }
                  />
                ))}
              </div>
            ) : (
              <div className="flex items-center justify-center h-full">
                <p className="text-sm text-gray-500">
                  まだメッセージはありません
                </p>
              </div>
            )}
          </div>

          <div className="p-4 border-t border-gray-200 bg-white">
            <div className="flex gap-2">
              <textarea
                value={messageDraft}
                onChange={(event) => setMessageDraft(event.target.value)}
                rows={2}
                className="flex-1 rounded-md border border-gray-300 bg-white px-3 py-2 text-sm placeholder:text-gray-400 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent resize-none"
                placeholder="Agentへメッセージを送信..."
                onKeyDown={(e) => {
                  if (e.key === "Enter" && (e.metaKey || e.ctrlKey)) {
                    void handleSendMessage();
                  }
                }}
              />
              <Button
                onClick={handleSendMessage}
                disabled={sendingMessage || messageDraft.trim().length === 0}
                isLoading={sendingMessage}
                size="sm"
                className="h-auto px-4"
              >
                送信
              </Button>
            </div>
            <p className="text-xs text-gray-500 mt-2">⌘ + Enter で送信</p>
          </div>
        </div>

        <div className="bg-white rounded-lg border border-gray-200 shadow-sm p-6 space-y-4">
          <h2 className="text-base font-semibold text-gray-900">設定</h2>

          <div className="space-y-3">
            <div className="flex items-center justify-between py-3 px-4 bg-gray-50 rounded-md border border-gray-100">
              <div className="flex-1">
                <p className="text-sm font-medium text-gray-900">
                  質問の自動生成
                </p>
                <p className="text-xs text-gray-500 mt-0.5">
                  Agentが自動的に新しい質問を生成します
                </p>
              </div>
              <Button
                variant={
                  threadData?.thread?.shouldProceed ? "default" : "outline"
                }
                size="sm"
                disabled={togglingProceed}
                isLoading={togglingProceed}
                onClick={handleToggleShouldProceed}
                className="gap-1.5 h-9 min-w-[100px]"
              >
                {threadData?.thread?.shouldProceed ? (
                  <>
                    <Pause className="h-3.5 w-3.5" />
                    ON
                  </>
                ) : (
                  <>
                    <Play className="h-3.5 w-3.5" />
                    OFF
                  </>
                )}
              </Button>
            </div>

            <div className="pt-2 border-t border-gray-200">
              <div className="flex items-center justify-between py-3 px-4 bg-red-50 rounded-md border border-red-200">
                <div className="flex-1">
                  <p className="text-sm font-medium text-red-900">
                    セッションを削除
                  </p>
                  <p className="text-xs text-red-600 mt-0.5">
                    この操作は取り消せません
                  </p>
                </div>
                <Button
                  onClick={handleDeleteSession}
                  disabled={deleting}
                  isLoading={deleting}
                  variant="destructive"
                  size="sm"
                  className="gap-1.5 h-9"
                >
                  <Trash2 className="h-3.5 w-3.5" />
                  削除
                </Button>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}

function ChatMessage({
  event,
  expanded,
  onToggle,
}: {
  event: TimelineEvent;
  expanded: boolean;
  onToggle: () => void;
}) {
  const meta = EVENT_TYPE_META[event.type] ?? {
    label: event.type,
    icon: "📄",
    color: "text-gray-600 bg-gray-50",
  };

  const isUserMessage = event.type === "user_message";
  const markdown =
    typeof event.payload.markdown === "string"
      ? (event.payload.markdown as string)
      : "";

  const progressPercent = Math.round((event.progress ?? 0) * 100);

  const getPreviewContent = () => {
    if (event.type === "survey" && event.statements.length > 0) {
      return `${event.statements.length}件のステートメントを生成`;
    }
    if (markdown) {
      const preview = markdown.replace(/[#*_`]/g, "").trim();
      return preview.length > 100 ? `${preview.substring(0, 100)}...` : preview;
    }
    return "処理中...";
  };

  return (
    <div
      className={`flex gap-3 ${isUserMessage ? "flex-row-reverse" : "flex-row"}`}
    >
      {!isUserMessage && (
        <div className="flex-shrink-0 w-8 h-8 rounded-full bg-gradient-to-br from-blue-500 to-purple-600 flex items-center justify-center text-white text-sm font-medium shadow-sm">
          🤖
        </div>
      )}

      <div
        className={`flex-1 max-w-[85%] ${isUserMessage ? "items-end" : "items-start"}`}
      >
        <div
          className={`rounded-lg shadow-sm ${
            isUserMessage
              ? "bg-blue-600 text-white"
              : "bg-white border border-gray-200"
          }`}
        >
          <button
            type="button"
            onClick={onToggle}
            className={`w-full text-left px-4 py-3 ${
              isUserMessage ? "hover:bg-blue-700" : "hover:bg-gray-50"
            } transition-colors rounded-lg`}
          >
            <div className="flex items-start justify-between gap-3">
              <div className="flex-1 min-w-0">
                <div className="flex items-center gap-2 mb-1">
                  {!isUserMessage && (
                    <span
                      className={`text-xs px-2 py-0.5 rounded-full ${meta.color}`}
                    >
                      {meta.icon} {meta.label}
                    </span>
                  )}
                  <span
                    className={`text-xs ${
                      isUserMessage ? "text-blue-100" : "text-gray-500"
                    }`}
                  >
                    {formatDateTime(event.updatedAt)}
                  </span>
                </div>

                {!expanded && (
                  <p
                    className={`text-sm ${
                      isUserMessage ? "text-white" : "text-gray-700"
                    } line-clamp-2`}
                  >
                    {getPreviewContent()}
                  </p>
                )}

                {!expanded && progressPercent < 100 && !isUserMessage && (
                  <div className="mt-2">
                    <div className="h-1.5 w-full rounded-full bg-gray-200">
                      <div
                        className="h-full rounded-full bg-blue-500 transition-all"
                        style={{ width: `${progressPercent}%` }}
                      />
                    </div>
                  </div>
                )}
              </div>

              <ChevronDown
                className={`h-4 w-4 flex-shrink-0 transition-transform ${
                  expanded ? "rotate-180" : ""
                } ${isUserMessage ? "text-blue-100" : "text-gray-400"}`}
              />
            </div>
          </button>

          {expanded && (
            <div className="px-4 pb-4 pt-1 space-y-3 border-t border-gray-100">
              {progressPercent < 100 && !isUserMessage && (
                <div>
                  <div className="flex items-center justify-between text-xs text-gray-500 mb-1">
                    <span>進捗</span>
                    <span>{progressPercent}%</span>
                  </div>
                  <div className="h-1.5 w-full rounded-full bg-gray-200">
                    <div
                      className="h-full rounded-full bg-blue-500 transition-all"
                      style={{ width: `${progressPercent}%` }}
                    />
                  </div>
                </div>
              )}

              {event.type === "survey" && event.statements.length > 0 && (
                <div>
                  <p className="text-xs font-medium text-gray-600 mb-2">
                    ステートメント（{event.statements.length}件）
                  </p>
                  <ul className="space-y-1.5">
                    {event.statements.map((statement) => (
                      <li
                        key={statement.id}
                        className="text-xs bg-gray-50 rounded px-3 py-2 border border-gray-100"
                      >
                        <span className="text-gray-500 mr-2">
                          #{statement.orderIndex + 1}
                        </span>
                        <span className="text-gray-700">{statement.text}</span>
                      </li>
                    ))}
                  </ul>
                </div>
              )}

              {markdown && (
                <div
                  className={`text-sm prose prose-sm max-w-none ${
                    isUserMessage ? "prose-invert text-white" : "text-gray-700"
                  }`}
                >
                  <ReactMarkdown remarkPlugins={[remarkGfm]}>
                    {markdown}
                  </ReactMarkdown>
                </div>
              )}
            </div>
          )}
        </div>
      </div>

      {isUserMessage && (
        <div className="flex-shrink-0 w-8 h-8 rounded-full bg-gray-300 flex items-center justify-center text-gray-600 text-sm font-medium">
          👤
        </div>
      )}
    </div>
  );
}
