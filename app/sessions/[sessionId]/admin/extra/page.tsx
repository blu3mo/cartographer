"use client";

import axios from "axios";
import { Bot, Loader2, RefreshCcw } from "lucide-react";
import { use, useCallback, useEffect, useState } from "react";

import UserMap from "@/components/UserMap";
import { Button } from "@/components/ui/Button";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { useUserId } from "@/lib/useUserId";

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
    goal: string;
    isPublic: boolean;
  };
  thread: EventThreadSummary;
  events: unknown[];
  agents: AgentInstanceSummary[];
}

const formatDateTime = (value: string) =>
  new Date(value).toLocaleString("ja-JP", { hour12: false });

const formatCompactId = (value?: string | null) => {
  if (!value) return "-";
  if (value.length <= 10) return value;
  return `${value.slice(0, 4)}…${value.slice(-4)}`;
};

export default function AdminExtraPage({
  params,
}: {
  params: Promise<{ sessionId: string }>;
}) {
  const { sessionId } = use(params);
  const { userId, isLoading: isUserIdLoading } = useUserId();
  const [threadData, setThreadData] = useState<EventThreadResponse | null>(
    null,
  );
  const [threadLoading, setThreadLoading] = useState(true);
  const [threadError, setThreadError] = useState<string | null>(null);

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
    if (isUserIdLoading || !userId) {
      return;
    }
    void fetchEventThread(true);
    const intervalId = window.setInterval(() => {
      void fetchEventThread();
    }, 5000);
    return () => window.clearInterval(intervalId);
  }, [fetchEventThread, isUserIdLoading, userId]);

  if (isUserIdLoading || threadLoading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-background">
      <div className="max-w-5xl mx-auto px-4 py-12 sm:px-6 lg:px-8 space-y-8">
        <div>
          <h1 className="text-3xl font-bold tracking-tight mb-2">
            追加情報・モニタリング
          </h1>
          <p className="text-muted-foreground">
            Agent Monitor と PCA 分析
          </p>
        </div>

        <Card>
          <CardHeader className="gap-3 lg:flex-row lg:items-center lg:justify-between">
            <div>
              <CardTitle>Agent Monitor</CardTitle>
              <CardDescription>
                Ptolemyなど、Event Threadに紐づくエージェントの状態
              </CardDescription>
            </div>
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
      </div>
    </div>
  );
}
