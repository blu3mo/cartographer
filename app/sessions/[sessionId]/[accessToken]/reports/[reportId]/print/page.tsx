"use client";

import axios from "axios";
import { ArrowLeft, Loader2, Printer } from "lucide-react";
import { use, useEffect, useState } from "react";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";

import { Button } from "@/components/ui/Button";
import { useUserId } from "@/lib/useUserId";

type SessionReportStatus = "pending" | "generating" | "completed" | "failed";

interface SessionReport {
  id: string;
  sessionId: string;
  version: number;
  status: SessionReportStatus;
  requestMarkdown: string;
  contentMarkdown: string | null;
  createdBy: string;
  model: string;
  errorMessage: string | null;
  createdAt: string;
  updatedAt: string;
  completedAt: string | null;
}

export default function SessionReportPrintPage({
  params,
}: {
  params: Promise<{
    sessionId: string;
    accessToken: string;
    reportId: string;
  }>;
}) {
  const { sessionId, accessToken, reportId } = use(params);
  const { userId, isLoading: isUserLoading } = useUserId();
  const [report, setReport] = useState<SessionReport | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (isUserLoading || !userId) return;

    const fetchReport = async () => {
      try {
        setLoading(true);
        const response = await axios.get(
          `/api/sessions/${sessionId}/${accessToken}/reports/${reportId}`,
          {
            headers: { Authorization: `Bearer ${userId}` },
          },
        );
        setReport(response.data.data as SessionReport);
        setError(null);
      } catch (err) {
        console.error("Failed to load report:", err);
        setError("レポートの取得に失敗しました。");
      } finally {
        setLoading(false);
      }
    };

    void fetchReport();
  }, [sessionId, accessToken, reportId, userId, isUserLoading]);

  const handlePrint = () => {
    window.print();
  };

  if (loading) {
    return (
      <div className="min-h-screen bg-white flex items-center justify-center">
        <Loader2 className="h-6 w-6 animate-spin text-slate-400" />
      </div>
    );
  }

  if (error || !report) {
    return (
      <div className="min-h-screen bg-white flex flex-col items-center justify-center gap-4">
        <p className="text-sm text-slate-500">
          {error ?? "レポートが見つかりません。"}
        </p>
        <Button
          type="button"
          variant="outline"
          size="sm"
          onClick={() => window.history.back()}
        >
          <ArrowLeft className="h-4 w-4" />
          戻る
        </Button>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-white text-slate-900 print:bg-white">
      <div className="mx-auto max-w-4xl px-6 py-8 space-y-8 print:max-w-none print:px-0 print:py-0 print:space-y-6">
        <div className="flex items-center justify-end gap-4 print:hidden">
          <Button
            type="button"
            variant="outline"
            size="sm"
            onClick={handlePrint}
            className="gap-1.5 text-xs"
          >
            <Printer className="h-4 w-4" />
            印刷する
          </Button>
        </div>

        <header className="space-y-2 text-center print:hidden">
          <h1 className="text-3xl font-semibold">
            セッションレポート v{String(report.version).padStart(2, "0")}
          </h1>
          <div className="flex justify-center gap-4 text-xs text-slate-500">
            <span>セッションID: {sessionId}</span>
            <span>レポートID: {report.id}</span>
          </div>
        </header>

        {report.requestMarkdown ? (
          <section className="rounded-3xl border border-indigo-100 bg-indigo-50/70 p-6 text-sm text-indigo-900 print:hidden">
            <p className="text-[11px] font-semibold uppercase tracking-[0.18em] text-indigo-400">
              Admin Request
            </p>
            <p className="mt-2 whitespace-pre-wrap leading-relaxed">
              {report.requestMarkdown}
            </p>
          </section>
        ) : null}

        <section className="rounded-3xl border border-slate-200 bg-white p-8 shadow-sm print:border-0 print:bg-transparent print:p-0 print:shadow-none">
          {report.status === "completed" && report.contentMarkdown ? (
            <div className="markdown-body prose prose-slate max-w-none text-base leading-relaxed print:text-[12pt]">
              <ReactMarkdown remarkPlugins={[remarkGfm]}>
                {report.contentMarkdown}
              </ReactMarkdown>
            </div>
          ) : report.status === "failed" ? (
            <p className="text-sm text-rose-600">
              レポート生成に失敗しました:{" "}
              {report.errorMessage ?? "詳細は管理画面を確認してください。"}
            </p>
          ) : (
            <p className="text-sm text-slate-500">
              レポートはまだ完成していません。管理画面から進行状況を確認してください。
            </p>
          )}
        </section>

        <footer className="text-center text-[11px] uppercase tracking-[0.2em] text-slate-400 print:hidden">
          <p>
            作成:{" "}
            {new Date(report.createdAt).toLocaleString("ja-JP", {
              hour12: false,
            })}
            {report.completedAt
              ? ` / 更新: ${new Date(report.completedAt).toLocaleString("ja-JP", { hour12: false })}`
              : ""}
          </p>
        </footer>
      </div>
    </div>
  );
}
