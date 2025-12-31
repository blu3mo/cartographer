"use client";

import axios from "axios";
import { ArrowLeft, ChevronDown, Copy, Loader2, Printer } from "lucide-react";
import { type ReactNode, useCallback, useEffect, useState } from "react";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";

import { StatementTagPopover } from "@/components/report/StatementTagPopover";
import { ThemeToggle } from "@/components/ThemeToggle";
import { Button } from "@/components/ui/Button";
import { useUserId } from "@/lib/useUserId";
import { cn } from "@/lib/utils";

type SessionReportStatus = "pending" | "generating" | "completed" | "failed";

interface SessionReport {
  id: string;
  sessionId: string;
  version: number;
  status: SessionReportStatus;
  requestMarkdown: string;
  contentMarkdown: string | null;
  promptSnapshot: Record<string, unknown> | null;
  createdBy: string;
  model: string;
  errorMessage: string | null;
  createdAt: string;
  updatedAt: string;
  completedAt: string | null;
}

type SessionSnapshot = {
  session?: {
    title?: string;
    goal?: string;
    context?: string;
  };
};

// Regex to match #n pattern (e.g., #1, #12, #123)
const STATEMENT_TAG_REGEX = /#(\d+)/g;

const getSnapshotSession = (
  snapshot: SessionReport["promptSnapshot"],
): SessionSnapshot["session"] | null => {
  if (!snapshot || typeof snapshot !== "object") {
    return null;
  }

  const session = (snapshot as SessionSnapshot).session;
  if (!session || typeof session !== "object") {
    return null;
  }

  const sessionData = session as {
    title?: unknown;
    goal?: unknown;
    context?: unknown;
  };
  return {
    title:
      typeof sessionData.title === "string" ? sessionData.title : undefined,
    goal: typeof sessionData.goal === "string" ? sessionData.goal : undefined,
    context:
      typeof sessionData.context === "string" ? sessionData.context : undefined,
  };
};

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
  const [copyStatus, setCopyStatus] = useState<"idle" | "copied" | "error">(
    "idle",
  );
  const [infoOpen, setInfoOpen] = useState({ goal: true, context: false });

  // Process a single string to replace #n with interactive popovers
  const processString = useCallback(
    (text: string): ReactNode => {
      if (!userId) return text;

      const parts: ReactNode[] = [];
      let lastIndex = 0;
      let match: RegExpExecArray | null = null;

      // Reset regex state
      STATEMENT_TAG_REGEX.lastIndex = 0;

      match = STATEMENT_TAG_REGEX.exec(text);
      while (match !== null) {
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
        match = STATEMENT_TAG_REGEX.exec(text);
      }

      // Add remaining text after last match
      if (lastIndex < text.length) {
        parts.push(text.slice(lastIndex));
      }

      return parts.length > 0 ? parts : text;
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
        return children.map((child) => processStatementTags(child));
      }

      // Handle React elements - we need to check if it's an element and process its children
      if (children && typeof children === "object" && "props" in children) {
        const element = children as React.ReactElement<{
          children?: ReactNode;
        }>;
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
          <p className="text-sm text-muted-foreground">
            レポートを読み込んでいます...
          </p>
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
            {error ??
              "指定されたレポートは存在しないか、アクセス権限がありません。"}
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

  const snapshotSession = getSnapshotSession(report.promptSnapshot);
  const sessionTitle = snapshotSession?.title?.trim() || "セッションレポート";
  const sessionGoal = snapshotSession?.goal?.trim();
  const sessionContext = snapshotSession?.context?.trim();
  const createdAtLabel = new Date(report.createdAt).toLocaleString("ja-JP", {
    hour12: false,
  });
  const completedAtLabel = report.completedAt
    ? new Date(report.completedAt).toLocaleString("ja-JP", {
        hour12: false,
      })
    : null;

  const toggleInfo = (key: "goal" | "context") => {
    setInfoOpen((prev) => ({ ...prev, [key]: !prev[key] }));
  };

  return (
    <div className="min-h-screen bg-background dark:bg-background text-foreground print:bg-white print:text-black">
      <div className="mx-auto max-w-4xl px-6 py-8 space-y-8 print:max-w-none print:px-0 print:py-0 print:space-y-6">
        <div className="flex items-center justify-end print:hidden">
          <ThemeToggle />
        </div>

        <header className="space-y-4 border-b border-border pb-6 print:hidden">
          <div className="text-center space-y-3">
            <h1 className="scroll-m-20 text-4xl font-extrabold tracking-tight text-balance">
              {sessionTitle}
            </h1>
            <p className="text-xl text-muted-foreground">
              バージョン {String(report.version).padStart(2, "0")}
            </p>
          </div>

          <div className="mx-auto w-full max-w-3xl">
            <div className="overflow-hidden rounded-xl border border-border/70 bg-muted/30">
              <div className="border-b border-border/70 last:border-b-0">
                <button
                  type="button"
                  onClick={() => toggleInfo("goal")}
                  aria-expanded={infoOpen.goal}
                  className="flex w-full items-center justify-between gap-4 px-4 py-3 text-left text-sm font-semibold text-foreground/90 transition hover:bg-muted/60"
                >
                  <span>ゴール</span>
                  <ChevronDown
                    className={cn(
                      "h-4 w-4 text-muted-foreground transition-transform",
                      infoOpen.goal ? "rotate-180" : "",
                    )}
                  />
                </button>
                <div
                  className={cn(
                    "px-4 pb-4 text-sm leading-7 text-muted-foreground whitespace-pre-wrap",
                    infoOpen.goal ? "block" : "hidden print:block",
                  )}
                >
                  {sessionGoal && sessionGoal.length > 0
                    ? sessionGoal
                    : "未設定"}
                </div>
              </div>

              <div className="border-b border-border/70 last:border-b-0">
                <button
                  type="button"
                  onClick={() => toggleInfo("context")}
                  aria-expanded={infoOpen.context}
                  className="flex w-full items-center justify-between gap-4 px-4 py-3 text-left text-sm font-semibold text-foreground/90 transition hover:bg-muted/60"
                >
                  <span>背景情報</span>
                  <ChevronDown
                    className={cn(
                      "h-4 w-4 text-muted-foreground transition-transform",
                      infoOpen.context ? "rotate-180" : "",
                    )}
                  />
                </button>
                <div
                  className={cn(
                    "px-4 pb-4 text-sm leading-7 text-muted-foreground whitespace-pre-wrap",
                    infoOpen.context ? "block" : "hidden print:block",
                  )}
                >
                  {sessionContext && sessionContext.length > 0
                    ? sessionContext
                    : "未設定"}
                </div>
              </div>
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
            <div className="mb-4 flex items-center justify-between gap-3 print:hidden">
              <p className="text-xs font-semibold text-muted-foreground">
                レポート本文
              </p>
              <div className="flex gap-2">
                <Button
                  type="button"
                  variant="outline"
                  size="sm"
                  onClick={handleCopy}
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
          ) : null}
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

        <section className="rounded-xl border border-border/60 bg-muted/30 px-4 py-4 text-xs text-muted-foreground print:border-border/40 print:bg-transparent print:text-[9pt]">
          <dl className="grid grid-cols-1 gap-x-6 gap-y-4 sm:grid-cols-3">
            <div className="space-y-1">
              <dt className="text-[10px] font-medium uppercase tracking-wide text-muted-foreground/70">
                Model
              </dt>
              <dd className="font-mono text-foreground/80">{report.model}</dd>
            </div>
            <div className="space-y-1">
              <dt className="text-[10px] font-medium uppercase tracking-wide text-muted-foreground/70">
                作成
              </dt>
              <dd className="font-mono text-foreground/80">{createdAtLabel}</dd>
            </div>
            {completedAtLabel ? (
              <div className="space-y-1">
                <dt className="text-[10px] font-medium uppercase tracking-wide text-muted-foreground/70">
                  完成
                </dt>
                <dd className="font-mono text-foreground/80">
                  {completedAtLabel}
                </dd>
              </div>
            ) : null}
            <div className="space-y-1 sm:col-span-3">
              <dt className="text-[10px] font-medium uppercase tracking-wide text-muted-foreground/70">
                セッションID
              </dt>
              <dd className="break-all font-mono text-foreground/80">
                {sessionId}
              </dd>
            </div>
            <div className="space-y-1 sm:col-span-3">
              <dt className="text-[10px] font-medium uppercase tracking-wide text-muted-foreground/70">
                レポートID
              </dt>
              <dd className="break-all font-mono text-foreground/80">
                {report.id}
              </dd>
            </div>
          </dl>
        </section>
      </div>
    </div>
  );
}
