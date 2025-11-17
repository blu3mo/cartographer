"use client";

import { ChevronDown, ChevronUp, Loader2 } from "lucide-react";

import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";

type ParticipantProgress = {
  userId: string;
  name: string;
  answeredCount: number;
  completionRate: number;
  totalStatements: number;
  updatedAt: string;
};

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

const formatPercent = (value: number) => {
  if (Number.isNaN(value)) return "0%";
  const rounded = Math.round(value * 10) / 10;
  return Number.isInteger(rounded)
    ? `${rounded.toFixed(0)}%`
    : `${rounded.toFixed(1)}%`;
};

interface MonitoringCardProps {
  participantProgress: ParticipantProgress[];
  isCollapsed: boolean;
  onToggleCollapsed: () => void;
  isLoading: boolean;
  hasSelectedSession: boolean;
}

export function MonitoringCard({
  participantProgress,
  isCollapsed,
  onToggleCollapsed,
  isLoading,
  hasSelectedSession,
}: MonitoringCardProps) {
  return (
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
            onClick={onToggleCollapsed}
            className="text-muted-foreground transition hover:text-foreground"
            aria-expanded={!isCollapsed}
            aria-controls="aside-monitoring"
          >
            {isCollapsed ? (
              <ChevronDown className="h-4 w-4" />
            ) : (
              <ChevronUp className="h-4 w-4" />
            )}
            <span className="sr-only">セクションを切り替え</span>
          </button>
        </div>
      </CardHeader>
      {!isCollapsed && (
        <CardContent id="aside-monitoring" className="space-y-3">
          {isLoading && hasSelectedSession ? (
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
      )}
    </Card>
  );
}
