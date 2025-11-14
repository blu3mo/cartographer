"use client";

import type { ReactNode } from "react";

import { RightSidebar as RightSidebarSection } from "@/dashboard/_components/layout/desktop/main/group1/RightSidebar";
import { SessionMetricGrid } from "./SessionMetrics";
import type { SessionInsight } from "./types";

type SessionSummaryProps = {
  insights: SessionInsight[];
  hasSelection: boolean;
  shareButton?: ReactNode;
  deleteButton?: ReactNode;
};

export function SessionSummary({
  insights,
  hasSelection,
  shareButton,
  deleteButton,
}: SessionSummaryProps) {
  return (
    <RightSidebarSection
      summary={
        hasSelection ? (
          <SessionMetricGrid metrics={insights} />
        ) : (
          <p className="text-sm text-slate-500">
            セッションを選択すると、主要な統計情報とクイックアクションが表示されます。
          </p>
        )
      }
      actions={
        hasSelection && (shareButton || deleteButton) ? (
          <div className="flex flex-col gap-2">
            {shareButton}
            {deleteButton}
          </div>
        ) : undefined
      }
    />
  );
}
