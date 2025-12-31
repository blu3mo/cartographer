import type * as React from "react";

import { cn } from "@/lib/utils";
import { Badge } from "./badge";

type SessionReportStatus = "pending" | "generating" | "completed" | "failed";

const STATUS_CONFIG: Record<
  SessionReportStatus,
  { label: string; dotClass: string; textClass: string }
> = {
  pending: {
    label: "待機中",
    dotClass: "bg-status-pending",
    textClass: "text-status-pending-foreground",
  },
  generating: {
    label: "生成中",
    dotClass: "bg-status-generating",
    textClass: "text-status-generating-foreground",
  },
  completed: {
    label: "完了",
    dotClass: "bg-status-completed",
    textClass: "text-status-completed-foreground",
  },
  failed: {
    label: "失敗",
    dotClass: "bg-status-failed",
    textClass: "text-status-failed-foreground",
  },
};

export interface StatusBadgeProps extends React.HTMLAttributes<HTMLDivElement> {
  status: SessionReportStatus;
}

export function StatusBadge({ status, className, ...props }: StatusBadgeProps) {
  const config = STATUS_CONFIG[status];

  return (
    <Badge variant="outline" className={cn("gap-1.5", className)} {...props}>
      <span className={cn("h-2 w-2 rounded-full", config.dotClass)} />
      <span className={config.textClass}>{config.label}</span>
    </Badge>
  );
}
