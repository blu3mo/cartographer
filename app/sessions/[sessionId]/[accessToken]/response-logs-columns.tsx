"use client";

import type { ColumnDef } from "@tanstack/react-table";

export interface ResponseLogItem {
  statementId: string;
  statementText: string;
  orderIndex: number;
  responseType: "scale" | "free_text" | null;
  value: number | null;
  textResponse: string | null;
  answeredAt: string | null;
}

export interface ResponseLogParticipant {
  userId: string;
  name: string;
  joinedAt: string;
  responses: ResponseLogItem[];
}

// フラットなテーブル行の型定義
export type ResponseLogRow = {
  participantUserId: string;
  participantName: string;
  joinedAt: string;
} & Record<string, ResponseLogItem | string>;

function formatDateTime(isoString: string): string {
  const date = new Date(isoString);
  return new Intl.DateTimeFormat("ja-JP", {
    year: "numeric",
    month: "2-digit",
    day: "2-digit",
    hour: "2-digit",
    minute: "2-digit",
  }).format(date);
}

function renderResponseCell(response: ResponseLogItem | null | undefined) {
  if (!response || response.responseType === null) {
    return <span className="text-xs text-muted-foreground">未回答</span>;
  }

  if (response.responseType === "scale") {
    return (
      <div className="flex items-center gap-2">
        <span
          className={`inline-flex h-7 w-7 items-center justify-center rounded-full text-xs font-semibold ${
            response.value === 2
              ? "bg-emerald-100 text-emerald-700 dark:bg-emerald-900/30 dark:text-emerald-400"
              : response.value === 1
                ? "bg-emerald-50 text-emerald-600 dark:bg-emerald-900/20 dark:text-emerald-500"
                : response.value === 0
                  ? "bg-muted text-muted-foreground"
                  : response.value === -1
                    ? "bg-amber-50 text-amber-600 dark:bg-amber-900/20 dark:text-amber-500"
                    : response.value === -2
                      ? "bg-amber-100 text-amber-700 dark:bg-amber-900/30 dark:text-amber-400"
                      : "bg-muted text-muted-foreground"
          }`}
        >
          {response.value !== null
            ? response.value > 0
              ? `+${response.value}`
              : response.value
            : "?"}
        </span>
        <span className="text-xs text-muted-foreground">
          {response.value === 2
            ? "強く同意"
            : response.value === 1
              ? "同意"
              : response.value === 0
                ? "わからない"
                : response.value === -1
                  ? "反対"
                  : response.value === -2
                    ? "強く反対"
                    : "未回答"}
        </span>
      </div>
    );
  }

  if (response.responseType === "free_text") {
    return (
      <div className="max-w-xs">
        <p className="line-clamp-3 text-xs text-muted-foreground">
          {response.textResponse}
        </p>
      </div>
    );
  }

  return <span className="text-xs text-muted-foreground">未回答</span>;
}

export function createResponseLogColumns(
  statements: Array<{ id: string; text: string; orderIndex: number }>,
): ColumnDef<ResponseLogRow>[] {
  const baseColumns: ColumnDef<ResponseLogRow>[] = [
    {
      accessorKey: "participantName",
      header: "参加者",
      cell: ({ row }) => (
        <div className="flex flex-col gap-0.5">
          <span className="font-medium text-foreground">
            {row.original.participantName}
          </span>
          <span className="text-xs text-muted-foreground">
            {formatDateTime(row.original.joinedAt)}
          </span>
        </div>
      ),
    },
  ];

  const statementColumns: ColumnDef<ResponseLogRow>[] = statements.map(
    (statement) => ({
      accessorKey: `statement_${statement.id}`,
      header: () => (
        <div className="flex flex-col gap-1 min-w-[200px]">
          <span className="text-xs text-muted-foreground">
            設問 {statement.orderIndex + 1}
          </span>
          <span className="line-clamp-2 text-xs font-normal">
            {statement.text}
          </span>
        </div>
      ),
      cell: ({ row }) => {
        const response = row.original[
          `statement_${statement.id}`
        ] as ResponseLogItem;
        return renderResponseCell(response);
      },
    }),
  );

  return [...baseColumns, ...statementColumns];
}
