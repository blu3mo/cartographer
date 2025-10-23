"use client";

import axios from "axios";
import { Loader2, Plus, Printer, Sparkles } from "lucide-react";
import { use, useEffect, useState } from "react";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";
import UserMap from "@/components/UserMap";
import { Button } from "@/components/ui/Button";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import { useUserId } from "@/lib/useUserId";

interface ResponseStats {
  strongYes: number;
  yes: number;
  dontKnow: number;
  no: number;
  strongNo: number;
  totalCount: number;
}

interface StatementWithStats {
  id: string;
  sessionId: string;
  text: string;
  orderIndex: number;
  responses: ResponseStats;
  agreementScore: number;
}

interface SituationAnalysisReport {
  id: string;
  sessionId: string;
  contentMarkdown: string;
  createdAt: string;
}

interface SessionAdminData {
  id: string;
  title: string;
  context: string;
  isPublic: boolean;
  createdAt: string;
  statements: StatementWithStats[];
  latestSituationAnalysisReport?: SituationAnalysisReport;
}

type SortType = "agreement" | "yes" | "dontKnow" | "no";

export default function AdminPage({
  params,
}: {
  params: Promise<{ sessionId: string }>;
}) {
  const { sessionId } = use(params);
  const { userId, isLoading: isUserIdLoading } = useUserId();
  const [data, setData] = useState<SessionAdminData | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [generating, setGenerating] = useState(false);
  const [generatingStatements, setGeneratingStatements] = useState(false);
  const [sortType, setSortType] = useState<SortType>("agreement");
  const [isReportExpanded, setIsReportExpanded] = useState(false);
  const [editingTitle, setEditingTitle] = useState("");
  const [editingContext, setEditingContext] = useState("");
  const [editingVisibility, setEditingVisibility] = useState<
    "public" | "private"
  >("public");
  const [isSavingSettings, setIsSavingSettings] = useState(false);
  const [settingsMessage, setSettingsMessage] = useState<string | null>(null);
  const [settingsError, setSettingsError] = useState<string | null>(null);

  useEffect(() => {
    if (isUserIdLoading || !userId) return;
    fetchAdminData();
  }, [userId, isUserIdLoading, sessionId]);

  const fetchAdminData = async () => {
    try {
      setLoading(true);
      const response = await axios.get(`/api/sessions/${sessionId}/admin`, {
        headers: {
          Authorization: `Bearer ${userId}`,
        },
      });
      setData(response.data.data);
      setError(null);
    } catch (err: any) {
      console.error("Failed to fetch admin data:", err);
      if (err.response?.status === 403) {
        setError("このセッションの管理権限がありません。");
      } else {
        setError("データの取得に失敗しました。");
      }
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    if (data) {
      setEditingTitle(data.title);
      setEditingContext(data.context);
      setEditingVisibility(data.isPublic ? "public" : "private");
    }
  }, [data]);

  // Update document title when session data is available
  useEffect(() => {
    if (data?.title) {
      document.title = `${data.title} - 管理画面 - Cartographer`;
    }
    return () => {
      document.title = "Cartographer - 認識を可視化し、合意形成を促進する";
    };
  }, [data?.title]);

  const handleSaveSettings = async (event: React.FormEvent) => {
    event.preventDefault();
    if (!userId) return;

    setIsSavingSettings(true);
    setSettingsMessage(null);
    setSettingsError(null);

    try {
      const response = await axios.patch(
        `/api/sessions/${sessionId}/admin`,
        {
          title: editingTitle,
          context: editingContext,
          isPublic: editingVisibility === "public",
        },
        {
          headers: {
            Authorization: `Bearer ${userId}`,
          },
        },
      );

      const updated = response.data.data as {
        title: string;
        context: string;
        isPublic: boolean;
      };

      setData((prev) =>
        prev
          ? {
              ...prev,
              title: updated.title,
              context: updated.context,
              isPublic: updated.isPublic,
            }
          : prev,
      );
      setSettingsMessage("セッション情報を更新しました。");
    } catch (err) {
      console.error("Failed to update session settings:", err);
      setSettingsError("セッション情報の更新に失敗しました。");
    } finally {
      setIsSavingSettings(false);
    }
  };

  const generateReport = async () => {
    try {
      setGenerating(true);
      const response = await axios.post(
        `/api/sessions/${sessionId}/reports/situation-analysis`,
        {},
        {
          headers: {
            Authorization: `Bearer ${userId}`,
          },
        },
      );
      // Update the data with the new report
      if (data) {
        setData({
          ...data,
          latestSituationAnalysisReport: response.data.report,
        });
      }
    } catch (err) {
      console.error("Failed to generate report:", err);
      alert("レポートの生成に失敗しました。");
    } finally {
      setGenerating(false);
    }
  };

  const generateNewStatements = async () => {
    try {
      setGeneratingStatements(true);
      await axios.post(
        `/api/sessions/${sessionId}/statements/generate`,
        {},
        {
          headers: {
            Authorization: `Bearer ${userId}`,
          },
        },
      );
      // Refresh the admin data to show new statements
      await fetchAdminData();
      alert("新しいステートメントを10個生成しました。");
    } catch (err) {
      console.error("Failed to generate new statements:", err);
      alert("ステートメントの生成に失敗しました。");
    } finally {
      setGeneratingStatements(false);
    }
  };

  const getSortedStatements = () => {
    if (!data) return [];
    const statements = [...data.statements];

    switch (sortType) {
      case "agreement":
        return statements.sort((a, b) => b.agreementScore - a.agreementScore);
      case "yes":
        return statements.sort((a, b) => {
          const aYes = a.responses.strongYes + a.responses.yes;
          const bYes = b.responses.strongYes + b.responses.yes;
          return bYes - aYes;
        });
      case "dontKnow":
        return statements.sort(
          (a, b) => b.responses.dontKnow - a.responses.dontKnow,
        );
      case "no":
        return statements.sort((a, b) => {
          const aNo = a.responses.strongNo + a.responses.no;
          const bNo = b.responses.strongNo + b.responses.no;
          return bNo - aNo;
        });
      default:
        return statements;
    }
  };

  if (isUserIdLoading || loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
      </div>
    );
  }

  if (error) {
    return (
      <div className="min-h-screen bg-background">
        <div className="max-w-4xl mx-auto px-4 py-12 sm:px-6 lg:px-8">
          <Card className="border-destructive">
            <CardContent className="pt-6">
              <p className="text-destructive">{error}</p>
            </CardContent>
          </Card>
        </div>
      </div>
    );
  }

  if (!data) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <p className="text-muted-foreground">セッションが見つかりません。</p>
      </div>
    );
  }

  const sortedStatements = getSortedStatements();

  return (
    <div className="min-h-screen bg-background">
      <div className="max-w-4xl mx-auto px-4 py-12 sm:px-6 lg:px-8">
        <div className="mb-8">
          <h1 className="text-3xl font-bold tracking-tight mb-2">
            {editingTitle || data.title}
          </h1>
          <p className="text-muted-foreground">管理画面</p>
        </div>

        {/* Session Settings */}
        <Card className="mb-8">
          <CardHeader>
            <CardTitle>セッション設定</CardTitle>
            <CardDescription>
              タイトル、公開設定、コンテキストを編集できます
            </CardDescription>
          </CardHeader>
          <CardContent>
            <form onSubmit={handleSaveSettings} className="space-y-6">
              <div className="space-y-2">
                <label htmlFor="sessionTitle" className="text-sm font-medium">
                  セッションのタイトル
                </label>
                <Input
                  id="sessionTitle"
                  type="text"
                  value={editingTitle}
                  onChange={(event) => setEditingTitle(event.target.value)}
                  required
                />
                <p className="text-xs text-muted-foreground">
                  参加者にとってわかりやすいタイトルを設定しましょう。
                </p>
              </div>

              <div className="space-y-3">
                <span className="text-sm font-medium">公開設定</span>
                <div className="flex flex-col gap-3 sm:flex-row">
                  <label className="flex items-start gap-3 rounded-lg border border-input bg-muted px-4 py-3 text-sm shadow-sm transition hover:border-primary/60">
                    <input
                      type="radio"
                      name="sessionVisibility"
                      value="public"
                      checked={editingVisibility === "public"}
                      onChange={() => setEditingVisibility("public")}
                      className="mt-0.5"
                    />
                    <span>
                      <span className="font-medium">公開セッション</span>
                      <br />
                      <span className="text-xs text-muted-foreground">
                        Cartographerのトップページで参加者を募集できます。
                      </span>
                    </span>
                  </label>
                  <label className="flex items-start gap-3 rounded-lg border border-input bg-muted px-4 py-3 text-sm shadow-sm transition hover:border-primary/60">
                    <input
                      type="radio"
                      name="sessionVisibility"
                      value="private"
                      checked={editingVisibility === "private"}
                      onChange={() => setEditingVisibility("private")}
                      className="mt-0.5"
                    />
                    <span>
                      <span className="font-medium">非公開セッション</span>
                      <br />
                      <span className="text-xs text-muted-foreground">
                        直接URLを共有したメンバーだけがアクセスできます。
                      </span>
                    </span>
                  </label>
                </div>
              </div>

              <div className="space-y-2">
                <label htmlFor="sessionContext" className="text-sm font-medium">
                  コンテキスト
                </label>
                <textarea
                  id="sessionContext"
                  value={editingContext}
                  onChange={(event) => setEditingContext(event.target.value)}
                  required
                  rows={12}
                  className="flex w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50 resize-none"
                  placeholder="セッションの目的や背景など、AIに共有したい情報を記入してください。"
                />
              </div>

              {(settingsMessage || settingsError) && (
                <Card
                  className={
                    settingsError ? "border-destructive" : "border-emerald-500"
                  }
                >
                  <CardContent className="pt-6">
                    <p
                      className={`text-sm ${settingsError ? "text-destructive" : "text-emerald-600"}`}
                    >
                      {settingsError ?? settingsMessage}
                    </p>
                  </CardContent>
                </Card>
              )}

              <Button
                type="submit"
                disabled={isSavingSettings}
                isLoading={isSavingSettings}
                className="w-full sm:w-auto"
              >
                セッション情報を保存
              </Button>
            </form>
          </CardContent>
        </Card>

        {/* Control Panel */}
        <Card className="mb-8">
          <CardHeader>
            <CardTitle>コントロールパネル</CardTitle>
            <CardDescription>
              レポートの生成と新しいステートメントの追加
            </CardDescription>
          </CardHeader>
          <CardContent>
            <div className="flex flex-wrap gap-3">
              <Button
                onClick={generateReport}
                disabled={generating}
                isLoading={generating}
                variant="default"
              >
                <Sparkles className="h-4 w-4" />
                現状分析レポートを生成
              </Button>
              <Button
                onClick={generateNewStatements}
                disabled={generatingStatements}
                isLoading={generatingStatements}
                variant="secondary"
              >
                <Plus className="h-4 w-4" />
                新しいStatementを10個生成
              </Button>
            </div>
          </CardContent>
        </Card>

        {/* User Map */}
        <Card className="mb-8">
          <CardHeader>
            <CardTitle>ユーザーマップ（PCA分析）</CardTitle>
            <CardDescription>
              参加者の回答パターンを2次元マップで可視化
            </CardDescription>
          </CardHeader>
          <CardContent>
            {userId && <UserMap sessionId={sessionId} userId={userId} />}
          </CardContent>
        </Card>

        {/* Latest Report */}
        {data.latestSituationAnalysisReport && (
          <Card className="mb-8">
            <CardHeader>
              <div className="flex items-start justify-between">
                <div>
                  <CardTitle>最新の現状分析レポート</CardTitle>
                  <CardDescription>
                    生成日時:{" "}
                    {new Date(
                      data.latestSituationAnalysisReport.createdAt,
                    ).toLocaleString("ja-JP")}
                  </CardDescription>
                </div>
                <a
                  href={`/sessions/${sessionId}/admin/print`}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="flex items-center gap-2 px-3 py-1.5 text-sm text-muted-foreground hover:text-foreground transition-colors rounded-md hover:bg-accent"
                  title="印刷用ページを開く"
                >
                  <Printer className="h-4 w-4" />
                  <span>印刷用</span>
                </a>
              </div>
            </CardHeader>
            <CardContent>
              <div
                className={`relative ${!isReportExpanded ? "max-h-32 overflow-hidden" : ""}`}
              >
                <div className="markdown-body prose prose-sm max-w-none">
                  <ReactMarkdown remarkPlugins={[remarkGfm]}>
                    {data.latestSituationAnalysisReport.contentMarkdown}
                  </ReactMarkdown>
                </div>
                {!isReportExpanded && (
                  <div
                    className="absolute inset-0 pointer-events-none"
                    style={{
                      background:
                        "linear-gradient(to bottom, rgba(255,255,255,0) 0%, rgba(255,255,255,0) 30%, hsl(var(--background)) 100%)",
                    }}
                  />
                )}
              </div>
              <div className="mt-3 text-center">
                <button
                  onClick={() => setIsReportExpanded(!isReportExpanded)}
                  className="text-sm text-muted-foreground hover:text-foreground transition-colors px-4 py-2 rounded-md hover:bg-accent"
                >
                  {isReportExpanded ? "▲ 折りたたむ" : "▼ 全文を表示"}
                </button>
              </div>
            </CardContent>
          </Card>
        )}

        {/* Statements List */}
        <Card>
          <CardHeader>
            <div className="flex items-center justify-between">
              <div>
                <CardTitle>ステートメント一覧</CardTitle>
                <CardDescription>
                  全{sortedStatements.length}件のステートメント
                </CardDescription>
              </div>
              <div className="flex items-center gap-2">
                <label
                  htmlFor="sort-select"
                  className="text-sm text-muted-foreground"
                >
                  並び替え:
                </label>
                <select
                  id="sort-select"
                  value={sortType}
                  onChange={(e) => setSortType(e.target.value as SortType)}
                  className="px-3 py-1.5 border border-input bg-background rounded-md text-sm focus:outline-none focus:ring-1 focus:ring-ring"
                >
                  <option value="agreement">合意度順</option>
                  <option value="yes">Yesの割合順</option>
                  <option value="dontKnow">わからないの割合順</option>
                  <option value="no">Noの割合順</option>
                </select>
              </div>
            </div>
          </CardHeader>
          <CardContent>
            <div className="space-y-4">
              {sortedStatements.map((statement) => (
                <StatementCard key={statement.id} statement={statement} />
              ))}
            </div>
          </CardContent>
        </Card>
      </div>
    </div>
  );
}

function StatementCard({ statement }: { statement: StatementWithStats }) {
  const { responses } = statement;
  const hasResponses = responses.totalCount > 0;

  return (
    <div className="border rounded-lg p-4 bg-card hover:shadow-sm transition-shadow">
      <p className="text-sm font-medium mb-3 leading-relaxed">
        {statement.text}
      </p>

      {hasResponses ? (
        <>
          {/* Horizontal Bar Chart with Tooltip */}
          <div className="flex w-full h-2 rounded-full overflow-visible mb-3 bg-muted">
            {responses.strongYes > 0 && (
              <div
                className="bg-emerald-600 hover:bg-emerald-700 transition-colors relative group cursor-pointer"
                style={{ width: `${responses.strongYes}%` }}
              >
                <div className="opacity-0 group-hover:opacity-100 absolute bottom-full left-1/2 transform -translate-x-1/2 -translate-y-2 px-3 py-2 bg-gray-900 text-white text-xs rounded shadow-lg whitespace-nowrap pointer-events-none transition-opacity">
                  👍 強く賛成: {responses.strongYes.toFixed(1)}%
                </div>
              </div>
            )}
            {responses.yes > 0 && (
              <div
                className="bg-green-500 hover:bg-green-600 transition-colors relative group cursor-pointer"
                style={{ width: `${responses.yes}%` }}
              >
                <div className="opacity-0 group-hover:opacity-100 absolute bottom-full left-1/2 transform -translate-x-1/2 -translate-y-2 px-3 py-2 bg-gray-900 text-white text-xs rounded shadow-lg whitespace-nowrap pointer-events-none transition-opacity">
                  ✓ 賛成: {responses.yes.toFixed(1)}%
                </div>
              </div>
            )}
            {responses.dontKnow > 0 && (
              <div
                className="bg-amber-400 hover:bg-amber-500 transition-colors relative group cursor-pointer"
                style={{ width: `${responses.dontKnow}%` }}
              >
                <div className="opacity-0 group-hover:opacity-100 absolute bottom-full left-1/2 transform -translate-x-1/2 -translate-y-2 px-3 py-2 bg-gray-900 text-white text-xs rounded shadow-lg whitespace-nowrap pointer-events-none transition-opacity">
                  🤔 わからない: {responses.dontKnow.toFixed(1)}%
                </div>
              </div>
            )}
            {responses.no > 0 && (
              <div
                className="bg-rose-500 hover:bg-rose-600 transition-colors relative group cursor-pointer"
                style={{ width: `${responses.no}%` }}
              >
                <div className="opacity-0 group-hover:opacity-100 absolute bottom-full left-1/2 transform -translate-x-1/2 -translate-y-2 px-3 py-2 bg-gray-900 text-white text-xs rounded shadow-lg whitespace-nowrap pointer-events-none transition-opacity">
                  ✗ 反対: {responses.no.toFixed(1)}%
                </div>
              </div>
            )}
            {responses.strongNo > 0 && (
              <div
                className="bg-red-600 hover:bg-red-700 transition-colors relative group cursor-pointer"
                style={{ width: `${responses.strongNo}%` }}
              >
                <div className="opacity-0 group-hover:opacity-100 absolute bottom-full left-1/2 transform -translate-x-1/2 -translate-y-2 px-3 py-2 bg-gray-900 text-white text-xs rounded shadow-lg whitespace-nowrap pointer-events-none transition-opacity">
                  👎 強く反対: {responses.strongNo.toFixed(1)}%
                </div>
              </div>
            )}
          </div>

          <div className="flex items-center gap-4 text-xs text-muted-foreground">
            <span>回答者数: {responses.totalCount}人</span>
            <span className="h-1 w-1 rounded-full bg-muted-foreground" />
            <span>合意度スコア: {statement.agreementScore}</span>
          </div>
        </>
      ) : (
        <p className="text-sm text-muted-foreground">まだ回答がありません</p>
      )}
    </div>
  );
}
