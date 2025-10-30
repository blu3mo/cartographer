"use client";

import axios from "axios";
import {
  ChevronDown,
  Edit3,
  Loader2,
  MessageCircle,
  Pause,
  Play,
  RefreshCcw,
  Save,
  Trash2,
  X,
} from "lucide-react";
import { use, useCallback, useEffect, useState } from "react";
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

interface SessionAdminData {
  id: string;
  title: string;
  context: string;
  goal: string;
  isPublic: boolean;
  createdAt: string;
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
  { label: string; badgeClass: string; color: string }
> = {
  plan: {
    label: "Plan",
    badgeClass: "bg-sky-50 text-sky-700 border-sky-200",
    color: "bg-sky-500",
  },
  survey: {
    label: "Survey",
    badgeClass: "bg-emerald-50 text-emerald-700 border-emerald-200",
    color: "bg-emerald-500",
  },
  survey_analysis: {
    label: "Analysis",
    badgeClass: "bg-purple-50 text-purple-700 border-purple-200",
    color: "bg-purple-500",
  },
  user_message: {
    label: "Message",
    badgeClass: "bg-amber-50 text-amber-700 border-amber-200",
    color: "bg-amber-500",
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

const truncateText = (text: string, maxLength: number) => {
  if (text.length <= maxLength) return text;
  return text.substring(0, maxLength) + "...";
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

  // Session settings edit mode
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
    if (data?.title) {
      document.title = `${data.title} - 管理画面 - Cartographer`;
    }
    return () => {
      document.title = "Cartographer";
    };
  }, [data]);

  const handleSaveSettings = async (event: React.FormEvent) => {
    event.preventDefault();
    if (!userId) return;

    setIsSavingSettings(true);
    setSettingsMessage(null);
    setSettingsError(null);

    try {
      const response = await axios.patch(
        `/api/sessions/${sessionId}/admin`,
        {
          title: editingTitle,
          context: editingContext,
          goal: editingGoal,
          isPublic: editingVisibility === "public",
        },
        {
          headers: {
            Authorization: `Bearer ${userId}`,
          },
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
      !confirm("このセッションを完全に削除しますか？この操作は取り消せません。")
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

  if (isUserIdLoading || loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
      </div>
    );
  }

  if (error) {
    return (
      <div className="min-h-screen bg-background">
        <div className="max-w-4xl mx-auto px-4 py-12 sm:px-6 lg:px-8">
          <Card className="border-destructive">
            <CardContent className="pt-6">
              <p className="text-destructive">{error}</p>
            </CardContent>
          </Card>
        </div>
      </div>
    );
  }

  if (!data) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <p className="text-muted-foreground">セッションが見つかりません。</p>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-background">
      <div className="max-w-6xl mx-auto px-4 py-8 sm:px-6 lg:px-8 space-y-6">
        {/* Header */}
        <div className="flex items-start justify-between">
          <div>
            <h1 className="text-2xl font-bold tracking-tight">
              {data.title}
            </h1>
            <p className="text-sm text-muted-foreground mt-1">管理画面</p>
          </div>
          <Button
            variant="outline"
            size="sm"
            onClick={() => window.open(`/sessions/${sessionId}/admin/extra`, '_blank')}
            className="text-xs"
          >
            追加情報
          </Button>
        </div>

        {/* Session Settings - Compact View */}
        <Card className="border-muted">
          <CardHeader className="pb-3">
            <div className="flex items-center justify-between">
              <CardTitle className="text-base">セッション設定</CardTitle>
              {!isEditingSettings && (
                <Button
                  variant="ghost"
                  size="sm"
                  onClick={() => setIsEditingSettings(true)}
                  className="gap-1.5 text-xs h-8"
                >
                  <Edit3 className="h-3.5 w-3.5" />
                  編集
                </Button>
              )}
            </div>
          </CardHeader>
          <CardContent className="space-y-3">
            {!isEditingSettings ? (
              <>
                <div className="grid grid-cols-[100px_1fr] gap-x-4 gap-y-2 text-xs">
                  <span className="text-muted-foreground">公開設定</span>
                  <span className="font-medium">
                    {data.isPublic ? "公開" : "非公開"}
                  </span>

                  <span className="text-muted-foreground">ゴール</span>
                  <span className="text-foreground/80">
                    {truncateText(data.goal, 120)}
                  </span>

                  <span className="text-muted-foreground">背景情報</span>
                  <span className="text-foreground/80">
                    {data.context ? truncateText(data.context, 120) : "未設定"}
                  </span>
                </div>
              </>
            ) : (
              <form onSubmit={handleSaveSettings} className="space-y-4">
                <div className="space-y-1.5">
                  <label htmlFor="sessionTitle" className="text-xs font-medium">
                    タイトル
                  </label>
                  <Input
                    id="sessionTitle"
                    type="text"
                    value={editingTitle}
                    onChange={(event) => setEditingTitle(event.target.value)}
                    required
                    className="text-sm h-9"
                  />
                </div>

                <div className="space-y-1.5">
                  <span className="text-xs font-medium">公開設定</span>
                  <div className="flex gap-2">
                    <label className="flex flex-1 items-center gap-2 rounded-md border border-input bg-muted/30 px-3 py-2 text-xs cursor-pointer hover:border-primary/60">
                      <input
                        type="radio"
                        name="sessionVisibility"
                        value="public"
                        checked={editingVisibility === "public"}
                        onChange={() => setEditingVisibility("public")}
                        className="text-xs"
                      />
                      <span>公開</span>
                    </label>
                    <label className="flex flex-1 items-center gap-2 rounded-md border border-input bg-muted/30 px-3 py-2 text-xs cursor-pointer hover:border-primary/60">
                      <input
                        type="radio"
                        name="sessionVisibility"
                        value="private"
                        checked={editingVisibility === "private"}
                        onChange={() => setEditingVisibility("private")}
                        className="text-xs"
                      />
                      <span>非公開</span>
                    </label>
                  </div>
                </div>

                <div className="space-y-1.5">
                  <label htmlFor="sessionGoal" className="text-xs font-medium">
                    ゴール
                  </label>
                  <textarea
                    id="sessionGoal"
                    value={editingGoal}
                    onChange={(event) => setEditingGoal(event.target.value)}
                    required
                    rows={6}
                    className="flex w-full rounded-md border border-input bg-transparent px-3 py-2 text-xs shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring resize-none"
                  />
                </div>

                <div className="space-y-1.5">
                  <label htmlFor="sessionContext" className="text-xs font-medium">
                    背景情報
                  </label>
                  <textarea
                    id="sessionContext"
                    value={editingContext}
                    onChange={(event) => setEditingContext(event.target.value)}
                    rows={5}
                    className="flex w-full rounded-md border border-input bg-transparent px-3 py-2 text-xs shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring resize-none"
                  />
                </div>

                {(settingsMessage || settingsError) && (
                  <div
                    className={`text-xs px-3 py-2 rounded-md ${
                      settingsError
                        ? "bg-destructive/10 text-destructive"
                        : "bg-emerald-50 text-emerald-700"
                    }`}
                  >
                    {settingsError ?? settingsMessage}
                  </div>
                )}

                <div className="flex gap-2">
                  <Button
                    type="submit"
                    disabled={isSavingSettings}
                    isLoading={isSavingSettings}
                    size="sm"
                    className="gap-1.5 text-xs h-8"
                  >
                    <Save className="h-3.5 w-3.5" />
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
                        setEditingVisibility(data.isPublic ? "public" : "private");
                      }
                    }}
                    className="gap-1.5 text-xs h-8"
                  >
                    <X className="h-3.5 w-3.5" />
                    キャンセル
                  </Button>
                </div>
              </form>
            )}
          </CardContent>
        </Card>

        {/* Event Thread Timeline */}
        <Card>
          <CardHeader className="pb-3">
            <div className="flex items-center justify-between">
              <div>
                <CardTitle className="text-base">Event Timeline</CardTitle>
                <CardDescription className="text-xs mt-1">
                  Plan / Survey / Analysis / Message
                </CardDescription>
              </div>
              <div className="flex items-center gap-2">
                <div className="flex items-center gap-2 text-xs">
                  <span className="text-muted-foreground">自動進行:</span>
                  <span
                    className={`font-medium ${
                      threadData?.thread?.shouldProceed
                        ? "text-emerald-600"
                        : "text-amber-600"
                    }`}
                  >
                    {threadData?.thread?.shouldProceed ? "ON" : "PAUSED"}
                  </span>
                </div>
                <Button
                  type="button"
                  variant="outline"
                  size="sm"
                  onClick={() => fetchEventThread(true)}
                  disabled={threadLoading}
                  className="gap-1.5 text-xs h-8"
                >
                  <RefreshCcw
                    className={`h-3.5 w-3.5 ${threadLoading ? "animate-spin" : ""}`}
                  />
                  更新
                </Button>
              </div>
            </div>
          </CardHeader>
          <CardContent className="space-y-4">
            {/* Timeline Container with fixed height and scroll */}
            <div className="relative border rounded-lg bg-muted/20 overflow-hidden">
              <div className="h-[700px] overflow-y-auto p-4">
                {threadLoading && !threadData ? (
                  <div className="flex items-center justify-center h-full">
                    <Loader2 className="h-6 w-6 animate-spin text-muted-foreground" />
                  </div>
                ) : threadError ? (
                  <div className="flex items-center justify-center h-full">
                    <p className="text-xs text-destructive">{threadError}</p>
                  </div>
                ) : threadData?.events.length ? (
                  <div className="space-y-3">
                    {threadData.events.map((event, index) => (
                      <TimelineEventCard
                        key={event.id}
                        event={event}
                        isFirst={index === 0}
                        isLast={index === threadData.events.length - 1}
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
                    <p className="text-xs text-muted-foreground">
                      まだイベントはありません
                    </p>
                  </div>
                )}
              </div>
            </div>

            {/* Controls - Compact horizontal layout */}
            <div className="flex flex-col sm:flex-row gap-3">
              <div className="flex-1 rounded-lg border bg-card p-3 space-y-2">
                <div className="flex items-center gap-2">
                  <MessageCircle className="h-3.5 w-3.5 text-muted-foreground" />
                  <span className="text-xs font-medium">メッセージ送信</span>
                </div>
                <textarea
                  value={messageDraft}
                  onChange={(event) => setMessageDraft(event.target.value)}
                  rows={2}
                  className="w-full rounded-md border border-input bg-background px-2.5 py-1.5 text-xs placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring resize-none"
                  placeholder="Agentへの指示やメモ"
                />
                <Button
                  type="button"
                  onClick={handleSendMessage}
                  disabled={sendingMessage || messageDraft.trim().length === 0}
                  isLoading={sendingMessage}
                  size="sm"
                  className="w-full gap-1.5 text-xs h-7"
                >
                  <MessageCircle className="h-3 w-3" />
                  送信
                </Button>
              </div>

              <div className="sm:w-48 rounded-lg border bg-card p-3 space-y-2">
                <span className="text-xs font-medium block">進行制御</span>
                <p className="text-[10px] text-muted-foreground leading-tight">
                  Agent の WAITING 状態を制御
                </p>
                <Button
                  type="button"
                  variant={
                    threadData?.thread?.shouldProceed ? "secondary" : "default"
                  }
                  className="w-full gap-1.5 text-xs h-7"
                  disabled={togglingProceed}
                  isLoading={togglingProceed}
                  onClick={handleToggleShouldProceed}
                  size="sm"
                >
                  {threadData?.thread?.shouldProceed ? (
                    <>
                      <Pause className="h-3 w-3" />
                      一時停止
                    </>
                  ) : (
                    <>
                      <Play className="h-3 w-3" />
                      再開
                    </>
                  )}
                </Button>
              </div>
            </div>
          </CardContent>
        </Card>

        {/* Danger Zone */}
        <Card className="border-destructive/50">
          <CardHeader className="pb-3">
            <CardTitle className="text-base text-destructive">危険な操作</CardTitle>
            <CardDescription className="text-xs">
              このセッションを完全に削除します。この操作は取り消せません。
            </CardDescription>
          </CardHeader>
          <CardContent>
            <Button
              onClick={handleDeleteSession}
              disabled={deleting}
              isLoading={deleting}
              variant="destructive"
              size="sm"
              className="gap-1.5 text-xs h-8"
            >
              <Trash2 className="h-3.5 w-3.5" />
              セッションを削除
            </Button>
          </CardContent>
        </Card>
      </div>
    </div>
  );
}

function TimelineEventCard({
  event,
  isFirst,
  isLast,
  expanded,
  onToggle,
}: {
  event: TimelineEvent;
  isFirst: boolean;
  isLast: boolean;
  expanded: boolean;
  onToggle: () => void;
}) {
  const meta = EVENT_TYPE_META[event.type] ?? {
    label: event.type,
    badgeClass: "bg-muted text-foreground border-border",
    color: "bg-muted-foreground",
  };

  const progressPercent = Math.round((event.progress ?? 0) * 100);
  const markdown =
    typeof event.payload.markdown === "string"
      ? (event.payload.markdown as string)
      : "";

  // Preview content
  const getPreviewContent = () => {
    if (event.type === "survey" && event.statements.length > 0) {
      return `${event.statements.length}件のステートメント`;
    }
    if (markdown) {
      const preview = markdown.replace(/[#*_`]/g, "").trim();
      return truncateText(preview, 100);
    }
    return "準備中...";
  };

  return (
    <div className="relative pl-6">
      {/* Timeline connector */}
      <div
        className={`absolute left-[7px] top-0 w-0.5 bg-border ${
          isFirst ? "top-3" : ""
        } ${isLast ? "h-3" : "h-full"}`}
      />

      {/* Timeline dot */}
      <div
        className={`absolute left-0 top-3 h-4 w-4 rounded-full border-2 border-background ${meta.color}`}
      />

      {/* Event card */}
      <div className="rounded-lg border bg-card shadow-sm">
        <button
          type="button"
          onClick={onToggle}
          className="flex w-full items-start justify-between gap-3 px-3 py-2.5 text-left hover:bg-muted/50 transition-colors"
        >
          <div className="flex-1 min-w-0">
            <div className="flex items-center gap-2 mb-1">
              <span
                className={`inline-flex items-center rounded-full border px-2 py-0.5 text-[10px] font-medium ${meta.badgeClass}`}
              >
                {meta.label}
              </span>
              <span className="text-[10px] text-muted-foreground">
                {formatDateTime(event.updatedAt)}
              </span>
              <span className="text-[10px] text-muted-foreground font-mono">
                #{String(event.orderIndex).padStart(3, "0")}
              </span>
            </div>

            {!expanded && (
              <p className="text-xs text-foreground/80 line-clamp-2">
                {getPreviewContent()}
              </p>
            )}

            {!expanded && progressPercent < 100 && (
              <div className="mt-2">
                <div className="h-1 w-full rounded-full bg-muted">
                  <div
                    className={`h-full rounded-full ${meta.color}`}
                    style={{ width: `${progressPercent}%` }}
                  />
                </div>
              </div>
            )}
          </div>

          <ChevronDown
            className={`h-4 w-4 text-muted-foreground flex-shrink-0 transition-transform ${
              expanded ? "rotate-180" : ""
            }`}
          />
        </button>

        {expanded && (
          <div className="px-3 pb-3 pt-1 space-y-3 border-t">
            {progressPercent < 100 && (
              <div>
                <div className="flex items-center justify-between text-[10px] text-muted-foreground mb-1">
                  <span>進捗</span>
                  <span>{progressPercent}%</span>
                </div>
                <div className="h-1.5 w-full rounded-full bg-muted">
                  <div
                    className={`h-full rounded-full ${meta.color} transition-all`}
                    style={{ width: `${progressPercent}%` }}
                  />
                </div>
              </div>
            )}

            {event.type === "survey" && event.statements.length > 0 && (
              <div>
                <p className="text-[10px] font-medium text-muted-foreground mb-2">
                  ステートメント（{event.statements.length}件）
                </p>
                <ul className="space-y-1.5 text-xs">
                  {event.statements.map((statement) => (
                    <li
                      key={statement.id}
                      className="rounded border border-dashed bg-muted/30 px-2.5 py-1.5"
                    >
                      <span className="text-[10px] text-muted-foreground mr-2">
                        #{statement.orderIndex + 1}
                      </span>
                      {statement.text}
                    </li>
                  ))}
                </ul>
              </div>
            )}

            {(event.type === "survey_analysis" ||
              event.type === "plan" ||
              event.type === "user_message") &&
              markdown && (
                <div className="rounded-md border bg-background px-3 py-2 text-sm markdown-body prose prose-sm">
                  <ReactMarkdown remarkPlugins={[remarkGfm]}>
                    {markdown}
                  </ReactMarkdown>
                </div>
              )}
          </div>
        )}
      </div>
    </div>
  );
}
