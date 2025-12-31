"use client";

import axios from "axios";
import { ArrowLeft, Copy, Loader2, Printer } from "lucide-react";
import { type ReactNode, useCallback, useEffect, useState } from "react";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";

import { StatementTagPopover } from "@/components/report/StatementTagPopover";
import { Button } from "@/components/ui/Button";
import { ThemeToggle } from "@/components/ThemeToggle";
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
  const [copyStatus, setCopyStatus] = useState<"idle" | "copied" | "error">("idle");

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

  const handleCopy = async () => {
    if (!report?.contentMarkdown) return;

    try {
      await navigator.clipboard.writeText(report.contentMarkdown);
      setCopyStatus("copied");
      setTimeout(() => setCopyStatus("idle"), 2000);
    } catch (err) {
      console.error("Failed to copy report:", err);
      setCopyStatus("error");
      setTimeout(() => setCopyStatus("idle"), 2000);
    }
  };

  if (loading) {
    return (
      <div className="min-h-screen bg-background dark:bg-background flex items-center justify-center">
        <div className="flex flex-col items-center gap-4">
          <Loader2 className="h-8 w-8 animate-spin text-primary" />
          <p className="text-sm text-muted-foreground">レポートを読み込んでいます...</p>
        </div>
      </div>
    );
  }

  if (error || !report) {
    return (
      <div className="min-h-screen bg-background dark:bg-background flex flex-col items-center justify-center gap-6 px-4">
        <div className="text-center space-y-2">
          <h2 className="scroll-m-20 text-2xl font-semibold tracking-tight">
            {error ? "エラーが発生しました" : "レポートが見つかりません"}
          </h2>
          <p className="text-sm text-muted-foreground leading-7">
            {error ?? "指定されたレポートは存在しないか、アクセス権限がありません。"}
          </p>
        </div>
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
    <div className="min-h-screen bg-background dark:bg-background text-foreground print:bg-white print:text-black">
      <div className="mx-auto max-w-4xl px-6 py-8 space-y-8 print:max-w-none print:px-0 print:py-0 print:space-y-6">
        <div className="flex items-center justify-between print:hidden">
          <ThemeToggle />
          <div className="flex gap-2">
            <Button
              type="button"
              variant="outline"
              size="sm"
              onClick={handleCopy}
              disabled={!report?.contentMarkdown}
              className="gap-1.5 text-xs"
            >
              <Copy className="h-4 w-4" />
              {copyStatus === "copied"
                ? "コピー済み"
                : copyStatus === "error"
                  ? "コピー失敗"
                  : "Markdownをコピー"}
            </Button>
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
        </div>

        <header className="space-y-4 border-b border-border pb-6 print:hidden">
          <div className="text-center space-y-3">
            <h1 className="scroll-m-20 text-4xl font-extrabold tracking-tight text-balance">
              セッションレポート
            </h1>
            <p className="text-xl text-muted-foreground">
              バージョン {String(report.version).padStart(2, "0")}
            </p>
          </div>

          <div className="grid grid-cols-1 sm:grid-cols-2 gap-3 max-w-2xl mx-auto pt-4">
            <div className="flex flex-col items-center gap-1 p-3 rounded-lg bg-muted/50 dark:bg-muted/30">
              <span className="text-xs font-medium text-muted-foreground uppercase tracking-wide">セッションID</span>
              <span className="text-sm font-mono text-foreground">{sessionId}</span>
            </div>
            <div className="flex flex-col items-center gap-1 p-3 rounded-lg bg-muted/50 dark:bg-muted/30">
              <span className="text-xs font-medium text-muted-foreground uppercase tracking-wide">レポートID</span>
              <span className="text-sm font-mono text-foreground">{report.id}</span>
            </div>
          </div>
        </header>

        {report.requestMarkdown ? (
          <section className="rounded-2xl border border-indigo-200 dark:border-indigo-800 bg-indigo-50/70 dark:bg-indigo-950/30 p-6 print:hidden">
            <div className="space-y-3">
              <h3 className="scroll-m-20 text-lg font-semibold tracking-tight text-indigo-900 dark:text-indigo-200">
                レポート生成のリクエスト
              </h3>
              <p className="whitespace-pre-wrap leading-7 text-sm text-indigo-800 dark:text-indigo-300">
                {report.requestMarkdown}
              </p>
            </div>
          </section>
        ) : null}

        <section className="rounded-2xl border border-border bg-card dark:bg-card p-8 shadow-sm print:border-0 print:bg-transparent print:p-0 print:shadow-none">
          {report.status === "completed" && report.contentMarkdown ? (
            <article className="markdown-body prose prose-slate dark:prose-invert max-w-none text-base leading-7 print:text-[12pt] prose-headings:scroll-m-20 prose-headings:tracking-tight prose-h1:text-4xl prose-h1:font-extrabold prose-h1:text-balance prose-h2:border-b prose-h2:pb-2 prose-h2:text-3xl prose-h2:font-semibold prose-h2:first:mt-0 prose-h3:text-2xl prose-h3:font-semibold prose-h4:text-xl prose-h4:font-semibold prose-p:leading-7 prose-p:[&:not(:first-child)]:mt-6 prose-blockquote:mt-6 prose-blockquote:border-l-2 prose-blockquote:pl-6 prose-blockquote:italic prose-code:relative prose-code:rounded prose-code:bg-muted prose-code:px-[0.3rem] prose-code:py-[0.2rem] prose-code:font-mono prose-code:text-sm prose-code:font-semibold prose-ul:my-6 prose-ul:[&:not(:first-child)]:mt-6 prose-ol:my-6 prose-ol:[&:not(:first-child)]:mt-6 prose-li:my-2">
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
            </article>
          ) : report.status === "failed" ? (
            <div className="py-8 text-center space-y-3">
              <div className="mx-auto flex h-12 w-12 items-center justify-center rounded-full bg-destructive/10 dark:bg-destructive/20">
                <span className="text-2xl">⚠️</span>
              </div>
              <div className="space-y-2">
                <h3 className="scroll-m-20 text-lg font-semibold tracking-tight text-destructive">
                  レポート生成に失敗しました
                </h3>
                <p className="text-sm text-muted-foreground leading-7 max-w-md mx-auto">
                  {report.errorMessage ?? "詳細は管理画面を確認してください。"}
                </p>
              </div>
            </div>
          ) : (
            <div className="py-8 text-center space-y-3">
              <div className="mx-auto flex h-12 w-12 items-center justify-center rounded-full bg-muted dark:bg-muted/50">
                <Loader2 className="h-6 w-6 text-muted-foreground animate-spin" />
              </div>
              <div className="space-y-2">
                <h3 className="scroll-m-20 text-lg font-semibold tracking-tight">
                  レポートを生成中
                </h3>
                <p className="text-sm text-muted-foreground leading-7 max-w-md mx-auto">
                  レポートはまだ完成していません。管理画面から進行状況を確認してください。
                </p>
              </div>
            </div>
          )}
        </section>

        <footer className="border-t border-border pt-6 print:hidden">
          <div className="flex flex-col sm:flex-row justify-center items-center gap-3 sm:gap-6">
            <div className="flex items-center gap-2 text-sm text-muted-foreground">
              <span className="font-medium">作成:</span>
              <time dateTime={report.createdAt} className="font-mono">
                {new Date(report.createdAt).toLocaleString("ja-JP", {
                  hour12: false,
                })}
              </time>
            </div>
            {report.completedAt && (
              <>
                <span className="hidden sm:inline text-muted-foreground">•</span>
                <div className="flex items-center gap-2 text-sm text-muted-foreground">
                  <span className="font-medium">完成:</span>
                  <time dateTime={report.completedAt} className="font-mono">
                    {new Date(report.completedAt).toLocaleString("ja-JP", {
                      hour12: false,
                    })}
                  </time>
                </div>
              </>
            )}
          </div>
          <div className="mt-4 text-center">
            <p className="text-xs text-muted-foreground">
              Model: <span className="font-mono">{report.model}</span>
            </p>
          </div>
        </footer>
      </div>
    </div>
  );
}
