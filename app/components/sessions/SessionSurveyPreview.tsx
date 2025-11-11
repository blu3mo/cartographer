"use client";

import axios from "axios";
import { Loader2, RefreshCw, Sparkles } from "lucide-react";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";

import { Button } from "@/components/ui/Button";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { createAuthorizationHeader } from "@/lib/auth";

import type { CreatedSession } from "./CreateSessionForm";

type TimelineStatement = {
  id: string;
  text: string;
  orderIndex: number;
};

type TimelineEvent = {
  id: string;
  type: "plan" | "survey" | "survey_analysis" | "user_message";
  orderIndex: number;
  updatedAt: string;
  statements: TimelineStatement[];
  payload: Record<string, unknown>;
};

type EventThreadResponse = {
  events: TimelineEvent[];
};

type SessionSurveyPreviewProps = {
  session: CreatedSession;
  userId: string | null;
  onReset?: () => void;
};

const formatRelativeDate = (value: string) => {
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
    hour: "numeric",
    minute: "numeric",
  });
};

export function SessionSurveyPreview({
  session,
  userId,
  onReset,
}: SessionSurveyPreviewProps) {
  const [surveyEvent, setSurveyEvent] = useState<TimelineEvent | null>(null);
  const [isLoading, setIsLoading] = useState(true);
  const [isRefreshing, setIsRefreshing] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [fetchCount, setFetchCount] = useState(0);
  const hasSurveyRef = useRef(false);
  const timeoutRef = useRef<NodeJS.Timeout | null>(null);

  const hasAdminAccess = Boolean(session.adminAccessToken);

  const fetchThread = useCallback(
    async ({
      showSpinner = false,
      manual = false,
    }: {
      showSpinner?: boolean;
      manual?: boolean;
    } = {}) => {
      if (!hasAdminAccess || !userId) {
        setError(
          "プレビューを表示するための権限情報が不足しています。モーダルを閉じてダッシュボードで確認してください。",
        );
        setIsLoading(false);
        return;
      }

      if (showSpinner) {
        setIsLoading(true);
      }
      if (manual) {
        setIsRefreshing(true);
      }

      try {
        const response = await axios.get<EventThreadResponse>(
          `/api/sessions/${session.id}/${session.adminAccessToken}/event-thread`,
          {
            headers: createAuthorizationHeader(userId),
          },
        );

        setFetchCount((previous) => previous + 1);
        setError(null);

        const events = response.data.events ?? [];
        const latestSurvey = [...events]
          .filter(
            (event) => event.type === "survey" && event.statements.length > 0,
          )
          .sort((a, b) => b.orderIndex - a.orderIndex)[0];

        hasSurveyRef.current = Boolean(latestSurvey);
        setSurveyEvent(latestSurvey ?? null);
      } catch (err) {
        console.error("Failed to fetch survey preview:", err);
        setError(
          "質問プレビューの取得に失敗しました。しばらく待ってから再取得してください。",
        );
      } finally {
        if (showSpinner) {
          setIsLoading(false);
        }
        if (manual) {
          setIsRefreshing(false);
        }
      }
    },
    [hasAdminAccess, session.adminAccessToken, session.id, userId],
  );

  useEffect(() => {
    hasSurveyRef.current = false;
    setSurveyEvent(null);
    setError(null);
    setFetchCount(0);

    const MAX_ATTEMPTS = 5;
    let attempts = 0;
    let isCancelled = false;

    const run = async (withSpinner: boolean) => {
      if (isCancelled) return;
      await fetchThread({ showSpinner: withSpinner });
      if (isCancelled || hasSurveyRef.current) return;
      if (attempts >= MAX_ATTEMPTS) return;
      attempts += 1;
      timeoutRef.current = setTimeout(() => {
        run(false);
      }, 3500);
    };

    run(true);

    return () => {
      isCancelled = true;
      if (timeoutRef.current) {
        clearTimeout(timeoutRef.current);
        timeoutRef.current = null;
      }
    };
  }, [fetchThread]);

  const handleManualRefresh = () => {
    fetchThread({ manual: true, showSpinner: !surveyEvent });
  };

  const helperMessage = useMemo(() => {
    if (error) {
      return error;
    }
    if (isLoading) {
      return "AIが質問を準備しています…";
    }
    if (!surveyEvent) {
      return "まだ質問が揃っていません。少し時間を置いて再取得してください。";
    }
    return null;
  }, [error, isLoading, surveyEvent]);

  return (
    <Card className="border-none shadow-none">
      <CardHeader className="space-y-2">
        <CardTitle className="text-xl">質問のプレビュー</CardTitle>
        <CardDescription>
          モーダルを閉じずに、生成された質問をざっと確認できます。
        </CardDescription>
      </CardHeader>
      <CardContent className="space-y-6">
        <div className="rounded-3xl border border-slate-200 bg-white/80 px-5 py-4">
          <p className="text-xs font-semibold uppercase tracking-wide text-slate-500">
            作成したセッション
          </p>
          <p className="mt-1 text-lg font-semibold text-slate-900">
            {session.title || "名称未設定のセッション"}
          </p>
          <p className="mt-1 text-sm text-slate-600 whitespace-pre-wrap">
            {session.goal ||
              session.context ||
              "詳細情報はまだ入力されていません。"}
          </p>
        </div>

        <div className="space-y-4 rounded-3xl border border-emerald-200 bg-emerald-50/60 p-4">
          {helperMessage ? (
            <div className="flex items-center gap-3 text-sm text-slate-600">
              {isLoading ? (
                <Loader2 className="h-4 w-4 animate-spin text-emerald-600" />
              ) : (
                <Sparkles className="h-4 w-4 text-emerald-600" />
              )}
              <p>{helperMessage}</p>
            </div>
          ) : surveyEvent ? (
            <div className="space-y-4">
              <div className="flex flex-wrap items-center justify-between gap-3">
                <div className="inline-flex items-center gap-2 rounded-full border border-emerald-200 bg-white/80 px-3 py-1 text-xs font-semibold text-emerald-700">
                  <span>Survey</span>
                  <span className="text-[10px] text-slate-400">
                    #{String(surveyEvent.orderIndex).padStart(3, "0")}・
                    {formatRelativeDate(surveyEvent.updatedAt)}
                  </span>
                </div>
                <span className="text-xs text-slate-500">
                  直近{surveyEvent.statements.length}件の質問
                </span>
              </div>
              <p className="text-sm text-slate-700">
                新しく{surveyEvent.statements.length}
                個の質問を作成しました。皆さんの回答をお待ちしています。
              </p>
              <div className="max-h-[360px] space-y-2 overflow-y-auto pr-1">
                {surveyEvent.statements.map((statement) => (
                  <div
                    key={statement.id}
                    className="rounded-2xl border border-slate-200 bg-white px-3 py-2 text-sm text-slate-800 shadow-sm"
                  >
                    <span className="mr-2 text-[11px] font-semibold text-slate-400">
                      #{statement.orderIndex + 1}
                    </span>
                    {statement.text}
                  </div>
                ))}
              </div>
            </div>
          ) : null}
        </div>

        <div className="flex flex-col gap-3 sm:flex-row">
          <Button
            type="button"
            variant="outline"
            onClick={handleManualRefresh}
            disabled={isRefreshing || !hasAdminAccess || isLoading}
            className="gap-1.5"
          >
            <RefreshCw
              className={`h-4 w-4 ${isRefreshing ? "animate-spin" : ""}`}
            />
            最新の質問を取得
          </Button>
          <Button
            type="button"
            variant="ghost"
            onClick={onReset}
            className="gap-1.5 text-slate-600 hover:text-slate-900"
          >
            もう一度セッションを作成
          </Button>
        </div>
        <p className="text-[11px] text-slate-400">
          {fetchCount > 0
            ? `プレビューを${fetchCount}回更新しました。より詳しい編集は、モーダルを閉じて管理ビューから行えます。`
            : "プレビューは数秒おきに自動更新されます。"}
        </p>
      </CardContent>
    </Card>
  );
}
