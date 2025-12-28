"use client";

import axios from "axios";
import { ArrowLeft, Loader2, Printer } from "lucide-react";
import { type ReactNode, useCallback, useEffect, useState } from "react";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";

import { StatementTagPopover } from "@/components/report/StatementTagPopover";
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

// Regex to match #n pattern (e.g., #1, #12, #123)
const STATEMENT_TAG_REGEX = /#(\d+)/g;

export default function SessionReportPrintPage({
  sessionId,
  accessToken,
  reportId,
}: {
  sessionId: string;
  accessToken: string;
  reportId: string;
}) {
  const { userId, isLoading: isUserLoading } = useUserId();
  const [report, setReport] = useState<SessionReport | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  // Process a single string to replace #n with interactive popovers
  const processString = useCallback(
    (text: string): ReactNode => {
      if (!userId) return text;

      const parts: ReactNode[] = [];
      let lastIndex = 0;
      let match: RegExpExecArray | null;

      // Reset regex state
      STATEMENT_TAG_REGEX.lastIndex = 0;

      while ((match = STATEMENT_TAG_REGEX.exec(text)) !== null) {
        // Add text before the match
        if (match.index > lastIndex) {
          parts.push(text.slice(lastIndex, match.index));
        }

        const statementNumber = parseInt(match[1], 10);
        parts.push(
          <StatementTagPopover
            key={`${match.index}-${statementNumber}`}
            statementNumber={statementNumber}
            sessionId={sessionId}
            accessToken={accessToken}
            reportId={reportId}
            userId={userId}
          />,
        );

        lastIndex = match.index + match[0].length;
      }

      // Add remaining text after last match
      if (lastIndex < text.length) {
        parts.push(text.slice(lastIndex));
      }

      return parts.length > 0 ? <>{parts}</> : text;
    },
    [userId, sessionId, accessToken, reportId],
  );

  // Recursively process ReactNode children
  const processStatementTags = useCallback(
    (children: ReactNode): ReactNode => {
      if (!userId) return children;

      if (typeof children === "string") {
        return processString(children);
      }

      if (typeof children === "number") {
        return processString(String(children));
      }

      if (Array.isArray(children)) {
        return children.map((child, index) => {
          const processed = processStatementTags(child);
          // Wrap in fragment with key if it's a processed node
          if (typeof child === "string" || typeof child === "number") {
            return <span key={index}>{processed}</span>;
          }
          return processed;
        });
      }

      // Handle React elements - we need to check if it's an element and process its children
      if (children && typeof children === "object" && "props" in children) {
        const element = children as React.ReactElement<{ children?: ReactNode }>;
        if (element.props?.children) {
          // Don't clone, just return original - the component renderers handle children
          return children;
        }
      }

      return children;
    },
    [userId, processString],
  );

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
              レポート生成のリクエスト
            </p>
            <p className="mt-2 whitespace-pre-wrap leading-relaxed">
              {report.requestMarkdown}
            </p>
          </section>
        ) : null}

        <section className="rounded-3xl border border-slate-200 bg-white p-8 shadow-sm print:border-0 print:bg-transparent print:p-0 print:shadow-none">
          {report.status === "completed" && report.contentMarkdown ? (
            <div className="markdown-body prose prose-slate max-w-none text-base leading-relaxed print:text-[12pt]">
              <ReactMarkdown
                remarkPlugins={[remarkGfm]}
                components={{
                  p: ({ children }) => <p>{processStatementTags(children)}</p>,
                  li: ({ children }) => (
                    <li>{processStatementTags(children)}</li>
                  ),
                  td: ({ children }) => (
                    <td>{processStatementTags(children)}</td>
                  ),
                  th: ({ children }) => (
                    <th>{processStatementTags(children)}</th>
                  ),
                  strong: ({ children }) => (
                    <strong>{processStatementTags(children)}</strong>
                  ),
                  em: ({ children }) => (
                    <em>{processStatementTags(children)}</em>
                  ),
                }}
              >
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
