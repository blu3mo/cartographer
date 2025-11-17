"use client";

import { ChevronDown, ChevronUp, Loader2 } from "lucide-react";

import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";

type StatementHighlight = {
  id: string;
  text: string;
  totalResponses: number;
  agreementScore: number;
  positiveShare: number;
  negativeShare: number;
};

const formatPercent = (value: number) => {
  if (Number.isNaN(value)) return "0%";
  const rounded = Math.round(value * 10) / 10;
  return Number.isInteger(rounded)
    ? `${rounded.toFixed(0)}%`
    : `${rounded.toFixed(1)}%`;
};

interface StatementHighlightsCardProps {
  statementHighlights: StatementHighlight[];
  isCollapsed: boolean;
  onToggleCollapsed: () => void;
  isLoading: boolean;
  hasSelectedSession: boolean;
}

export function StatementHighlightsCard({
  statementHighlights,
  isCollapsed,
  onToggleCollapsed,
  isLoading,
  hasSelectedSession,
}: StatementHighlightsCardProps) {
  return (
    <Card>
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
            onClick={onToggleCollapsed}
            className="text-muted-foreground transition hover:text-foreground"
            aria-expanded={!isCollapsed}
            aria-controls="aside-statementHighlights"
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
        <CardContent id="aside-statementHighlights" className="space-y-3">
          {isLoading && hasSelectedSession ? (
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
    </Card>
  );
}
