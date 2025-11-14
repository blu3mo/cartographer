"use client";

import { ExternalLink, FileText, Lock, Plus } from "lucide-react";
import { useRouter } from "next/navigation";
import type { ReactNode } from "react";

import { Button } from "@/components/ui/Button";
import { Chat as ChatSection } from "@/dashboard/_components/layout/desktop/main/group1/Chat";
import { Group1 } from "@/dashboard/_components/layout/desktop/main/group1/Group1";
import { Report as ReportSection } from "@/dashboard/_components/layout/desktop/main/group1/Report";
import type { Session, SessionInsight } from "@/dashboard/_components/types";

import { SessionMetricGrid } from "./SessionMetrics";

type SessionWorkspaceProps = {
  selectedSession: Session | null;
  hasAccessToken: boolean;
  metrics: SessionInsight[];
  reportContent: ReactNode;
  shareButton?: ReactNode;
  deleteButton?: ReactNode;
  error?: string | null;
  onCreateSession: () => void;
  totalSessions: number;
  adminSessionsCount: number;
  rightSidebar: ReactNode;
};

export function SessionWorkspace({
  selectedSession,
  hasAccessToken,
  metrics,
  reportContent,
  shareButton,
  deleteButton,
  error,
  onCreateSession,
  totalSessions,
  adminSessionsCount,
  rightSidebar,
}: SessionWorkspaceProps) {
  const router = useRouter();

  if (selectedSession && !hasAccessToken) {
    return (
      <div className="flex min-h-0 flex-1 flex-col gap-4">
        <div className="flex min-h-0 flex-1 flex-col items-center justify-center rounded-3xl border border-dashed border-slate-200 bg-slate-50 px-6 py-12 text-center text-sm text-slate-500">
          <p className="text-base font-semibold text-slate-600">
            管理トークンを確認できません
          </p>
          <p>管理者権限を発行してから再度お試しください。</p>
        </div>
      </div>
    );
  }

  if (!selectedSession) {
    if (totalSessions === 0) {
      return (
        <div className="flex min-h-0 flex-1 flex-col gap-4">
          <div className="flex flex-1 flex-col items-center justify-center gap-4 rounded-3xl border border-slate-200 bg-white px-6 py-12 text-center shadow-sm">
            <div className="flex h-16 w-16 items-center justify-center rounded-full bg-primary/10">
              <FileText className="h-8 w-8 text-primary" />
            </div>
            <div className="space-y-2">
              <p className="text-base font-semibold text-slate-800">
                セッションがありません
              </p>
              <p className="text-sm text-slate-500">
                新しいセッションを作成して、チームとの対話を始めましょう。
              </p>
            </div>
            <Button onClick={onCreateSession}>
              <Plus className="mr-1 h-4 w-4" />
              最初のセッションを作成
            </Button>
          </div>
        </div>
      );
    }

    return (
      <div className="flex min-h-0 flex-1 flex-col gap-4">
        <div className="flex flex-1 flex-col items-center justify-center gap-2 rounded-3xl border border-dashed border-slate-200 bg-slate-50 px-6 py-12 text-center text-sm text-slate-500">
          <p className="text-base font-semibold text-slate-600">
            {adminSessionsCount > 0
              ? "サイドバーからセッションを選択してください"
              : "管理可能なセッションが見つかりません"}
          </p>
          <p>
            {adminSessionsCount > 0
              ? "「管理」ボタンを押すと、ここにセッションの管理ビューが表示されます。"
              : "新しいセッションを作成するか、サイドバーから管理セッションを追加してください。"}
          </p>
        </div>
      </div>
    );
  }

  const sessionDetailButton = (
    <Button
      type="button"
      variant="outline"
      size="sm"
      className="gap-1.5"
      onClick={() => router.push(`/sessions/${selectedSession.id}`)}
    >
      <ExternalLink className="h-3.5 w-3.5" />
      詳細ビューへ
    </Button>
  );

  return (
    <div className="flex min-h-0 flex-1 flex-col gap-4">
      <div className="rounded-3xl border border-slate-200 bg-white px-6 py-5 shadow-sm">
        <div className="flex flex-col gap-4 lg:flex-row lg:items-start lg:justify-between">
          <div className="space-y-2">
            <p className="text-xs font-semibold uppercase tracking-wide text-slate-500">
              選択中のセッション
            </p>
            <div className="space-y-1.5">
              <div className="flex flex-wrap items-center gap-2">
                <h1 className="text-2xl font-bold text-slate-900">
                  {selectedSession.title || "名称未設定"}
                </h1>
                {!selectedSession.isPublic && (
                  <span className="flex items-center justify-center rounded-full border border-slate-300 bg-slate-100 p-1.5">
                    <Lock className="h-3 w-3 text-slate-700" />
                  </span>
                )}
              </div>
              <p className="text-sm text-slate-500 whitespace-pre-wrap">
                {selectedSession.goal ||
                  selectedSession.context ||
                  "詳細情報は未設定です。"}
              </p>
            </div>
          </div>
          <div className="flex flex-wrap items-center gap-2">
            {shareButton}
            {deleteButton}
          </div>
        </div>
      </div>

      <Group1
        report={
          <ReportSection
            header={
              <div className="space-y-4">
                {error && (
                  <div className="rounded-2xl border border-amber-300 bg-amber-50 px-4 py-3 text-sm text-amber-700">
                    {error}
                  </div>
                )}
                <SessionMetricGrid
                  metrics={metrics}
                  className="sm:grid-cols-3"
                />
              </div>
            }
            content={reportContent}
          />
        }
        chat={
          <ChatSection
            title="コメント"
            description="チームから寄せられた意見やメモをまとめて、次のアクションにつなげましょう。"
            action={sessionDetailButton}
          />
        }
        rightSidebar={rightSidebar}
      />
    </div>
  );
}
