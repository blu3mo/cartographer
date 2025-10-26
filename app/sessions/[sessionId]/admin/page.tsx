"use client";

import axios from "axios";
import {
  Bot,
  ChevronDown,
  ChevronRight,
  Loader2,
  MessageCircle,
  Pause,
  Play,
  RefreshCcw,
  Trash2,
} from "lucide-react";
import { use, useCallback, useEffect, useState } from "react";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";

import UserMap from "@/components/UserMap";
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

interface AgentInstanceSummary {
  id: string;
  agentType: string;
  state: string;
  statePayload: Record<string, unknown>;
  createdAt: string;
  updatedAt: string;
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
    isPublic: boolean;
  };
  thread: EventThreadSummary;
  events: TimelineEvent[];
  agents: AgentInstanceSummary[];
}

const EVENT_TYPE_META: Record<
  ThreadEventType,
  { label: string; badgeClass: string }
> = {
  plan: {
    label: "Plan",
    badgeClass: "bg-sky-100 text-sky-700 border-sky-200",
  },
  survey: {
    label: "Survey",
    badgeClass: "bg-emerald-100 text-emerald-700 border-emerald-200",
  },
  survey_analysis: {
    label: "Survey Analysis",
    badgeClass: "bg-purple-100 text-purple-700 border-purple-200",
  },
  user_message: {
    label: "User Message",
    badgeClass: "bg-amber-100 text-amber-700 border-amber-200",
  },
};

const formatDateTime = (value: string) =>
  new Date(value).toLocaleString("ja-JP", { hour12: false });

const formatCompactId = (value?: string | null) => {
  if (!value) return "-";
  if (value.length <= 10) return value;
  return `${value.slice(0, 4)}…${value.slice(-4)}`;
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
  const [editingTitle, setEditingTitle] = useState("");
  const [editingContext, setEditingContext] = useState("");
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
        isPublic: boolean;
      };

      setData((prev) =>
        prev
          ? {
              ...prev,
              title: updated.title,
              context: updated.context,
              isPublic: updated.isPublic,
            }
          : prev,
      );
      setSettingsMessage("セッション情報を更新しました。");
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
      <div className="max-w-5xl mx-auto px-4 py-12 sm:px-6 lg:px-8 space-y-8">
        <div>
          <h1 className="text-3xl font-bold tracking-tight mb-2">
            {editingTitle || data.title}
          </h1>
          <p className="text-muted-foreground">管理画面</p>
        </div>

        <Card>
          <CardHeader>
            <CardTitle>セッション設定</CardTitle>
            <CardDescription>
              タイトル、公開設定、コンテキストを編集できます
            </CardDescription>
          </CardHeader>
          <CardContent>
            <form onSubmit={handleSaveSettings} className="space-y-6">
              <div className="space-y-2">
                <label htmlFor="sessionTitle" className="text-sm font-medium">
                  セッションのタイトル
                </label>
                <Input
                  id="sessionTitle"
                  type="text"
                  value={editingTitle}
                  onChange={(event) => setEditingTitle(event.target.value)}
                  required
                />
                <p className="text-xs text-muted-foreground">
                  参加者にとってわかりやすいタイトルを設定しましょう。
                </p>
              </div>

              <div className="space-y-3">
                <span className="text-sm font-medium">公開設定</span>
                <div className="flex flex-col gap-3 sm:flex-row">
                  <label className="flex flex-1 items-start gap-3 rounded-lg border border-input bg-muted px-4 py-3 text-sm shadow-sm transition hover:border-primary/60">
                    <input
                      type="radio"
                      name="sessionVisibility"
                      value="public"
                      checked={editingVisibility === "public"}
                      onChange={() => setEditingVisibility("public")}
                      className="mt-0.5"
                    />
                    <span>
                      <span className="font-medium">公開セッション</span>
                      <br />
                      <span className="text-xs text-muted-foreground">
                        Cartographerのトップページで参加者を募集できます。
                      </span>
                    </span>
                  </label>
                  <label className="flex flex-1 items-start gap-3 rounded-lg border border-input bg-muted px-4 py-3 text-sm shadow-sm transition hover:border-primary/60">
                    <input
                      type="radio"
                      name="sessionVisibility"
                      value="private"
                      checked={editingVisibility === "private"}
                      onChange={() => setEditingVisibility("private")}
                      className="mt-0.5"
                    />
                    <span>
                      <span className="font-medium">非公開セッション</span>
                      <br />
                      <span className="text-xs text-muted-foreground">
                        直接URLを共有したメンバーだけがアクセスできます。
                      </span>
                    </span>
                  </label>
                </div>
              </div>

              <div className="space-y-2">
                <label htmlFor="sessionContext" className="text-sm font-medium">
                  コンテキスト
                </label>
                <textarea
                  id="sessionContext"
                  value={editingContext}
                  onChange={(event) => setEditingContext(event.target.value)}
                  required
                  rows={12}
                  className="flex w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50 resize-none"
                  placeholder="セッションの目的や背景など、AIに共有したい情報を記入してください。"
                />
              </div>

              {(settingsMessage || settingsError) && (
                <Card
                  className={
                    settingsError ? "border-destructive" : "border-emerald-500"
                  }
                >
                  <CardContent className="pt-6">
                    <p
                      className={`text-sm ${settingsError ? "text-destructive" : "text-emerald-600"}`}
                    >
                      {settingsError ?? settingsMessage}
                    </p>
                  </CardContent>
                </Card>
              )}

              <Button
                type="submit"
                disabled={isSavingSettings}
                isLoading={isSavingSettings}
                className="w-full sm:w-auto"
              >
                セッション情報を保存
              </Button>
            </form>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="gap-3 lg:flex-row lg:items-center lg:justify-between">
            <div>
              <CardTitle>Event Thread</CardTitle>
              <CardDescription>
                Plan / Survey / Survey Analysis / User Message の履歴
              </CardDescription>
            </div>
            <div className="flex flex-wrap gap-3">
              <Button
                type="button"
                variant="outline"
                onClick={() => fetchEventThread(true)}
                disabled={threadLoading}
                className="gap-2"
              >
                <RefreshCcw
                  className={`h-4 w-4 ${threadLoading ? "animate-spin" : ""}`}
                />
                更新
              </Button>
            </div>
          </CardHeader>
          <CardContent className="space-y-6">
            {threadData?.thread && (
              <div className="flex flex-wrap gap-4 text-xs text-muted-foreground border rounded-md px-4 py-3 bg-muted/30">
                <span>
                  Thread ID:{" "}
                  <span className="font-mono text-[11px]">
                    {formatCompactId(threadData.thread.id)}
                  </span>
                </span>
                <span>作成: {formatDateTime(threadData.thread.createdAt)}</span>
                <span>
                  最終更新: {formatDateTime(threadData.thread.updatedAt)}
                </span>
                <span>
                  shouldProceed:{" "}
                  <span
                    className={
                      threadData.thread.shouldProceed
                        ? "text-emerald-600 font-medium"
                        : "text-amber-600 font-medium"
                    }
                  >
                    {threadData.thread.shouldProceed ? "ON" : "PAUSED"}
                  </span>
                </span>
              </div>
            )}

            {threadLoading ? (
              <div className="flex justify-center py-12">
                <Loader2 className="h-6 w-6 animate-spin text-muted-foreground" />
              </div>
            ) : threadError ? (
              <div className="rounded-md border border-destructive/50 bg-destructive/5 px-4 py-3 text-sm text-destructive">
                {threadError}
              </div>
            ) : threadData?.events.length ? (
              <div className="space-y-3">
                {threadData.events.map((event) => (
                  <TimelineEventCard
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
              <p className="text-sm text-muted-foreground">
                まだイベントはありません。
              </p>
            )}

            <div className="grid gap-6 lg:grid-cols-5">
              <div className="rounded-lg border bg-muted/30 p-4 lg:col-span-2">
                <p className="text-sm font-medium text-muted-foreground mb-2">
                  shouldProceed 制御
                </p>
                <p className="text-xs text-muted-foreground mb-4">
                  AgentのWAITING状態を進行させるかを切り替えます。
                </p>
                <Button
                  type="button"
                  variant={
                    threadData?.thread?.shouldProceed ? "secondary" : "default"
                  }
                  className="w-full gap-2"
                  disabled={togglingProceed}
                  isLoading={togglingProceed}
                  onClick={handleToggleShouldProceed}
                >
                  {threadData?.thread?.shouldProceed ? (
                    <>
                      <Pause className="h-4 w-4" />
                      一時停止する
                    </>
                  ) : (
                    <>
                      <Play className="h-4 w-4" />
                      再開する
                    </>
                  )}
                </Button>
              </div>

              <div className="rounded-lg border p-4 space-y-3 lg:col-span-3">
                <div className="flex items-center gap-2">
                  <MessageCircle className="h-4 w-4 text-muted-foreground" />
                  <p className="text-sm font-medium">User Message を送信</p>
                </div>
                <textarea
                  value={messageDraft}
                  onChange={(event) => setMessageDraft(event.target.value)}
                  rows={4}
                  className="w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
                  placeholder="Agentに伝えたいメモや指示を記入します。"
                />
                <div className="flex justify-end">
                  <Button
                    type="button"
                    onClick={handleSendMessage}
                    disabled={
                      sendingMessage || messageDraft.trim().length === 0
                    }
                    isLoading={sendingMessage}
                    className="gap-2"
                  >
                    <MessageCircle className="h-4 w-4" />
                    送信
                  </Button>
                </div>
              </div>
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle>Agent Monitor</CardTitle>
            <CardDescription>
              Ptolemyなど、Event Threadに紐づくエージェントの状態
            </CardDescription>
          </CardHeader>
          <CardContent>
            {threadLoading ? (
              <div className="flex justify-center py-6">
                <Loader2 className="h-5 w-5 animate-spin text-muted-foreground" />
              </div>
            ) : threadData?.agents.length ? (
              <div className="space-y-3">
                {threadData.agents.map((agent) => (
                  <div
                    key={agent.id}
                    className="rounded-lg border border-border/80 bg-muted/30 p-4"
                  >
                    <div className="flex flex-wrap items-center justify-between gap-2">
                      <div className="flex items-center gap-2 text-sm font-semibold">
                        <Bot className="h-4 w-4 text-muted-foreground" />
                        {agent.agentType} · {formatCompactId(agent.id)}
                      </div>
                      <span className="text-xs text-muted-foreground">
                        最終更新: {formatDateTime(agent.updatedAt)}
                      </span>
                    </div>
                    <p className="text-sm mt-2">
                      状態:{" "}
                      <span className="font-semibold text-foreground">
                        {agent.state}
                      </span>
                    </p>
                    {Object.keys(agent.statePayload ?? {}).length > 0 && (
                      <pre className="mt-3 text-xs bg-background rounded-md border px-3 py-2 overflow-x-auto">
                        {JSON.stringify(agent.statePayload, null, 2)}
                      </pre>
                    )}
                  </div>
                ))}
              </div>
            ) : threadError ? (
              <p className="text-sm text-destructive">{threadError}</p>
            ) : (
              <p className="text-sm text-muted-foreground">
                このThreadに紐づくAgentはまだありません。
              </p>
            )}
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle>ユーザーマップ（PCA分析）</CardTitle>
            <CardDescription>
              参加者の回答パターンを2次元マップで可視化
            </CardDescription>
          </CardHeader>
          <CardContent>
            {userId && <UserMap sessionId={sessionId} userId={userId} />}
          </CardContent>
        </Card>

        <Card className="border-destructive">
          <CardHeader>
            <CardTitle className="text-destructive">危険な操作</CardTitle>
            <CardDescription>
              このセッションを完全に削除します。この操作は取り消せません。
            </CardDescription>
          </CardHeader>
          <CardContent>
            <Button
              onClick={handleDeleteSession}
              disabled={deleting}
              isLoading={deleting}
              variant="destructive"
              className="w-full sm:w-auto"
            >
              <Trash2 className="h-4 w-4" />
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
  expanded,
  onToggle,
}: {
  event: TimelineEvent;
  expanded: boolean;
  onToggle: () => void;
}) {
  const meta = EVENT_TYPE_META[event.type] ?? {
    label: event.type,
    badgeClass: "bg-muted text-foreground border-border",
  };

  const progressPercent = Math.round((event.progress ?? 0) * 100);
  const markdown =
    typeof event.payload.markdown === "string"
      ? (event.payload.markdown as string)
      : "";

  return (
    <div className="rounded-lg border border-border bg-card">
      <button
        type="button"
        onClick={onToggle}
        className="flex w-full items-center justify-between gap-3 px-4 py-3 text-left"
      >
        <div className="flex items-center gap-3">
          <span
            className={`inline-flex items-center rounded-full border px-3 py-1 text-xs font-medium ${meta.badgeClass}`}
          >
            {meta.label}
          </span>
          <div className="flex flex-col text-xs text-muted-foreground">
            <span>更新: {formatDateTime(event.updatedAt)}</span>
            <span className="font-mono text-[11px]">
              #{String(event.orderIndex).padStart(4, "0")}
            </span>
          </div>
        </div>
        {expanded ? (
          <ChevronDown className="h-4 w-4 text-muted-foreground" />
        ) : (
          <ChevronRight className="h-4 w-4 text-muted-foreground" />
        )}
      </button>

      <div className="px-4 pb-4">
        <div className="mb-3">
          <div className="flex items-center justify-between text-xs text-muted-foreground mb-1">
            <span>進捗</span>
            <span>{progressPercent}%</span>
          </div>
          <div className="h-1.5 w-full rounded-full bg-muted">
            <div
              className="h-full rounded-full bg-primary transition-all"
              style={{ width: `${progressPercent}%` }}
            />
          </div>
        </div>

        <div className="flex flex-wrap gap-3 text-[11px] text-muted-foreground">
          {event.agentId && (
            <span>Agent: {formatCompactId(event.agentId)}</span>
          )}
          {event.userId && <span>User: {formatCompactId(event.userId)}</span>}
          <span>作成: {formatDateTime(event.createdAt)}</span>
          <span>更新: {formatDateTime(event.updatedAt)}</span>
        </div>

        {expanded && (
          <div className="mt-4 space-y-3 border-t pt-4">
            {event.type === "survey" && (
              <div>
                <p className="text-xs font-medium text-muted-foreground mb-2">
                  ステートメント（{event.statements.length}件）
                </p>
                {event.statements.length ? (
                  <ul className="space-y-2 text-sm leading-relaxed">
                    {event.statements.map((statement) => (
                      <li
                        key={statement.id}
                        className="rounded-md border border-dashed border-border/60 bg-muted/30 px-3 py-2"
                      >
                        <span className="text-xs text-muted-foreground mr-2">
                          #{statement.orderIndex + 1}
                        </span>
                        {statement.text}
                      </li>
                    ))}
                  </ul>
                ) : (
                  <p className="text-xs text-muted-foreground">
                    ステートメントを準備中です…
                  </p>
                )}
              </div>
            )}

            {event.type === "survey_analysis" && (
              <div className="space-y-3">
                <p className="text-xs text-muted-foreground">
                  対象ステートメント: {event.statements.length}件
                </p>
                {markdown ? (
                  <div className="rounded-md border bg-background px-3 py-2 text-sm markdown-body prose prose-sm">
                    <ReactMarkdown remarkPlugins={[remarkGfm]}>
                      {markdown}
                    </ReactMarkdown>
                  </div>
                ) : (
                  <p className="text-xs text-muted-foreground">
                    分析文章を生成中…
                  </p>
                )}
              </div>
            )}

            {event.type === "plan" && (
              <div>
                {markdown ? (
                  <div className="rounded-md border bg-background px-3 py-2 text-sm markdown-body prose prose-sm">
                    <ReactMarkdown remarkPlugins={[remarkGfm]}>
                      {markdown}
                    </ReactMarkdown>
                  </div>
                ) : (
                  <p className="text-xs text-muted-foreground">
                    プランを生成中…
                  </p>
                )}
              </div>
            )}

            {event.type === "user_message" && (
              <div className="rounded-md border bg-background px-3 py-2 text-sm markdown-body prose prose-sm">
                {markdown ? (
                  <ReactMarkdown remarkPlugins={[remarkGfm]}>
                    {markdown}
                  </ReactMarkdown>
                ) : (
                  <span className="text-xs text-muted-foreground">
                    メッセージが空です。
                  </span>
                )}
              </div>
            )}
          </div>
        )}
      </div>
    </div>
  );
}
