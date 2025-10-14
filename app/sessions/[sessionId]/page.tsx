'use client';

import { use, useEffect, useRef, useState } from 'react';
import { useUserId } from '@/lib/useUserId';
import { createAuthorizationHeader } from '@/lib/auth';
import axios from 'axios';
import ReactMarkdown from 'react-markdown';
import remarkGfm from 'remark-gfm';
import { Button } from '@/components/ui/Button';
import { Card, CardContent, CardDescription, CardHeader, CardTitle, Skeleton } from '@/components/ui/card';
import { Input } from '@/components/ui/input';
import { FileText, Loader2 } from 'lucide-react';

type Statement = {
  id: string;
  text: string;
  sessionId: string;
  orderIndex: number;
};

type IndividualReport = {
  id: string;
  participantUserId: string;
  sessionId: string;
  contentMarkdown: string;
  createdAt: string;
};

type SessionState = 'NEEDS_NAME' | 'ANSWERING' | 'COMPLETED';

type SessionInfo = {
  id: string;
  title: string;
  context: string;
  isPublic: boolean;
  hostUserId: string;
  createdAt: string;
  updatedAt: string;
  isHost: boolean;
  isParticipant: boolean;
};

export default function SessionPage({
  params,
}: {
  params: Promise<{ sessionId: string }>;
}) {
  const { sessionId } = use(params);
  const { userId, isLoading: userLoading } = useUserId();
  const [sessionInfo, setSessionInfo] = useState<SessionInfo | null>(null);
  const [isSessionInfoLoading, setIsSessionInfoLoading] = useState(true);
  const [sessionInfoError, setSessionInfoError] = useState<string | null>(null);
  const [state, setState] = useState<SessionState>('NEEDS_NAME');
  const [name, setName] = useState('');
  const [currentStatement, setCurrentStatement] = useState<Statement | null>(null);
  const [prefetchedStatement, setPrefetchedStatement] = useState<Statement | null | undefined>(undefined);
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [individualReport, setIndividualReport] = useState<IndividualReport | null>(null);
  const [isGeneratingReport, setIsGeneratingReport] = useState(false);
  const [isCheckingParticipation, setIsCheckingParticipation] = useState(false);
  const [isLoadingReport, setIsLoadingReport] = useState(true);
  const hasJustCompletedRef = useRef(false);
  const sessionInfoId = sessionInfo?.id;

  useEffect(() => {
    if (!userId || userLoading) return;

    const fetchSessionInfo = async () => {
      setIsSessionInfoLoading(true);
      try {
        const response = await axios.get(
          `/api/sessions/${sessionId}`,
          { headers: createAuthorizationHeader(userId) }
        );
        setSessionInfo(response.data.session);
        setSessionInfoError(null);
      } catch (err: unknown) {
        console.error('Failed to fetch session info:', err);
        setSessionInfo(null);
        if (axios.isAxiosError(err)) {
          if (err.response?.status === 404) {
            setSessionInfoError('セッションが見つかりませんでした。');
          } else if (err.response?.status === 403) {
            setSessionInfoError('このセッションにアクセスする権限がありません。');
          } else {
            setSessionInfoError('セッション情報の取得に失敗しました。');
          }
        } else {
          setSessionInfoError('セッション情報の取得に失敗しました。');
        }
      } finally {
        setIsSessionInfoLoading(false);
      }
    };

    fetchSessionInfo();
  }, [userId, userLoading, sessionId]);

  useEffect(() => {
    if (!userId || userLoading) return;
    if (isSessionInfoLoading) return;
    if (sessionInfoError) return;
    if (!sessionInfoId) return;
    if (state !== 'NEEDS_NAME') return;

    // Check if already participating
    const checkParticipation = async () => {
      setIsCheckingParticipation(true);
      try {
        const response = await axios.get(
          `/api/sessions/${sessionId}/statements/next`,
          { headers: createAuthorizationHeader(userId) }
        );

        // If we got a statement, user is already participating
        if (response.data.statement) {
          setCurrentStatement(response.data.statement);
          setState('ANSWERING');
          setSessionInfo((prev) =>
            prev ? { ...prev, isParticipant: true } : prev
          );
        } else {
          setState('COMPLETED');
          setSessionInfo((prev) =>
            prev ? { ...prev, isParticipant: true } : prev
          );
        }
      } catch (err: unknown) {
        // If error is 401, user hasn't joined yet
        if (axios.isAxiosError(err) && err.response?.status === 401) {
          setState('NEEDS_NAME');
          setSessionInfo((prev) =>
            prev ? { ...prev, isParticipant: false } : prev
          );
        } else {
          setState('NEEDS_NAME');
        }
      } finally {
        setIsCheckingParticipation(false);
      }
    };

    checkParticipation();
  }, [
    userId,
    userLoading,
    sessionId,
    sessionInfoId,
    isSessionInfoLoading,
    sessionInfoError,
    state,
  ]);

  // Prefetch next statement when current statement is displayed
  useEffect(() => {
    if (!userId || userLoading) return;
    if (state !== 'ANSWERING') return;
    if (!currentStatement) return;

    // Reset prefetch state to undefined (loading state)
    setPrefetchedStatement(undefined);

    const prefetchNextStatement = async () => {
      try {
        const response = await axios.get(
          `/api/sessions/${sessionId}/statements/next?excludeStatementId=${currentStatement.id}`,
          { headers: createAuthorizationHeader(userId) }
        );

        if (response.data.statement) {
          setPrefetchedStatement(response.data.statement);
        } else {
          // null means this is the last question
          setPrefetchedStatement(null);
        }
      } catch (err) {
        // Silently fail prefetch - keep as undefined to trigger fallback
        console.error('Prefetch failed:', err);
        setPrefetchedStatement(undefined);
      }
    };

    prefetchNextStatement();
  }, [userId, userLoading, sessionId, currentStatement, state]);

  useEffect(() => {
    if (!userId || userLoading) return;
    if (state === 'NEEDS_NAME') return;

    const fetchIndividualReport = async () => {
      setIsLoadingReport(true);
      try {
        const response = await axios.get(
          `/api/sessions/${sessionId}/individual-report`,
          { headers: createAuthorizationHeader(userId) }
        );
        setIndividualReport(response.data.report);
      } catch (err) {
        if (axios.isAxiosError(err)) {
          // 403 means not a participant; 404 is handled as no report yet.
          if (err.response?.status === 403) {
            setIndividualReport(null);
            return;
          }
          if (err.response?.status === 404) {
            setIndividualReport(null);
            return;
          }
        }
        console.error('Failed to fetch individual report:', err);
      } finally {
        setIsLoadingReport(false);
      }
    };

    fetchIndividualReport();
  }, [userId, userLoading, sessionId, state]);

  // Auto-generate report when all questions are answered
  useEffect(() => {
    if (!userId || userLoading) return;
    if (state !== 'COMPLETED') return;

    // Only auto-generate if we just transitioned to COMPLETED (not on page reload)
    if (!hasJustCompletedRef.current) return;

    // Wait for initial report fetch to complete to avoid race condition
    if (isLoadingReport) return;

    // Reset the flag after using it
    hasJustCompletedRef.current = false;

    // Automatically generate/update report when user completes all questions
    const autoGenerateReport = async () => {
      setIsGeneratingReport(true);
      setError(null);

      try {
        const response = await axios.post(
          `/api/sessions/${sessionId}/individual-report`,
          {},
          { headers: createAuthorizationHeader(userId) }
        );

        setIndividualReport(response.data.report);
      } catch (err) {
        console.error('Failed to auto-generate report:', err);
        // Show error to user so they know auto-generation failed
        if (axios.isAxiosError(err) && err.response?.data?.error) {
          setError(`レポートの自動生成に失敗しました: ${err.response.data.error}`);
        } else {
          setError('レポートの自動生成に失敗しました。「レポートを生成」ボタンから手動で生成してください。');
        }
      } finally {
        setIsGeneratingReport(false);
      }
    };

    autoGenerateReport();
  }, [userId, userLoading, sessionId, state, isLoadingReport]);

  const handleJoinSession = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!userId) return;

    setIsLoading(true);
    setError(null);

    try {
      await axios.post(
        `/api/sessions/${sessionId}/participants`,
        { name },
        { headers: createAuthorizationHeader(userId) }
      );

      // Fetch first statement
      const response = await axios.get(
        `/api/sessions/${sessionId}/statements/next`,
        { headers: createAuthorizationHeader(userId) }
      );

      if (response.data.statement) {
        setCurrentStatement(response.data.statement);
        setState('ANSWERING');
      } else {
        // Set flag to trigger auto-generation (edge case: no questions in session)
        hasJustCompletedRef.current = true;
        setState('COMPLETED');
      }
      setSessionInfo((prev) =>
        prev ? { ...prev, isParticipant: true } : prev
      );
    } catch (err) {
      console.error('Failed to join session:', err);
      if (axios.isAxiosError(err) && err.response?.data?.error) {
        setError(`エラー: ${err.response.data.error}`);
      } else {
        setError('セッションへの参加に失敗しました。');
      }
    } finally {
      setIsLoading(false);
    }
  };

  const handleAnswer = async (value: number) => {
    if (!userId || !currentStatement || isLoading) return;

    const previousStatement = currentStatement;
    const cachedNextStatement = prefetchedStatement;
    setError(null);

    try {
      // undefined: prefetch is still loading or failed
      // null: this is the last question (no more questions available)
      // Statement object: next question is ready

      if (cachedNextStatement === null) {
        // This is the last question - submit answer and show completion
        setIsLoading(true);

        await axios.post(
          `/api/sessions/${sessionId}/responses`,
          { statementId: previousStatement.id, value },
          { headers: createAuthorizationHeader(userId) }
        );

        // Set flag to trigger auto-generation
        hasJustCompletedRef.current = true;
        setState('COMPLETED');
        setCurrentStatement(null);
        setPrefetchedStatement(undefined);
        setIsLoading(false);
      } else if (cachedNextStatement) {
        // We have a prefetched statement - use it immediately for instant transition
        setCurrentStatement(cachedNextStatement);
        setPrefetchedStatement(undefined);

        // Submit answer in background (no need to wait)
        axios.post(
          `/api/sessions/${sessionId}/responses`,
          { statementId: previousStatement.id, value },
          { headers: createAuthorizationHeader(userId) }
        ).catch((err) => {
          console.error('Failed to submit answer:', err);
          if (axios.isAxiosError(err) && err.response?.data?.error) {
            setError(`エラー: ${err.response.data.error}`);
          } else {
            setError('回答の送信に失敗しました。');
          }
        });
      } else {
        // cachedNextStatement === undefined
        // Prefetch hasn't completed yet (rapid clicking) - fall back to original behavior
        setIsLoading(true);

        const [, nextResponse] = await Promise.all([
          axios.post(
            `/api/sessions/${sessionId}/responses`,
            { statementId: previousStatement.id, value },
            { headers: createAuthorizationHeader(userId) }
          ),
          axios.get(
            `/api/sessions/${sessionId}/statements/next?excludeStatementId=${previousStatement.id}`,
            { headers: createAuthorizationHeader(userId) }
          ),
        ]);

        // Update to next question
        if (nextResponse.data.statement) {
          setCurrentStatement(nextResponse.data.statement);
        } else {
          // Set flag to trigger auto-generation
          hasJustCompletedRef.current = true;
          setState('COMPLETED');
          setCurrentStatement(null);
        }

        setIsLoading(false);
      }
    } catch (err) {
      console.error('Failed to submit answer:', err);
      if (axios.isAxiosError(err) && err.response?.data?.error) {
        setError(`エラー: ${err.response.data.error}`);
      } else {
        setError('回答の送信に失敗しました。');
      }
      setIsLoading(false);
    }
  };

  const handleGenerateReport = async () => {
    if (!userId) return;

    setIsGeneratingReport(true);
    setError(null);

    try {
      const response = await axios.post(
        `/api/sessions/${sessionId}/individual-report`,
        {},
        { headers: createAuthorizationHeader(userId) }
      );

      setIndividualReport(response.data.report);
    } catch (err) {
      console.error('Failed to generate report:', err);
      if (axios.isAxiosError(err) && err.response?.data?.error) {
        setError(`エラー: ${err.response.data.error}`);
      } else {
        setError('レポートの生成に失敗しました。');
      }
    } finally {
      setIsGeneratingReport(false);
    }
  };

  if (userLoading || isSessionInfoLoading || isCheckingParticipation) {
    return (
      <div className="min-h-screen bg-background">
        <div className="max-w-3xl mx-auto px-4 py-12 sm:px-6 lg:px-8">
          <div className="mb-8 space-y-2">
            <Skeleton className="h-8 w-48" />
            <Skeleton className="h-4 w-32" />
            <Skeleton className="h-4 w-64" />
          </div>
          <Card>
            <CardHeader>
              <Skeleton className="h-7 w-24 mb-2" />
              <Skeleton className="h-4 w-64" />
            </CardHeader>
            <CardContent>
              <div className="space-y-4">
                <Skeleton className="h-10 w-full" />
                <Skeleton className="h-10 w-full" />
              </div>
            </CardContent>
          </Card>
        </div>
      </div>
    );
  }

  if (sessionInfoError) {
    return (
      <div className="min-h-screen bg-background">
        <div className="max-w-3xl mx-auto px-4 py-12 sm:px-6 lg:px-8">
          <div className="mb-8">
            <h1 className="text-3xl font-bold tracking-tight">
              セッション
            </h1>
          </div>
          <Card>
            <CardContent className="pt-6 pb-6">
              <p className="text-sm text-muted-foreground">{sessionInfoError}</p>
            </CardContent>
          </Card>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-background">
      <div className="max-w-3xl mx-auto px-4 py-12 sm:px-6 lg:px-8">
        <div className="mb-8">
          <h1 className="text-3xl font-bold tracking-tight">
            {sessionInfo?.title ?? 'セッション'}
          </h1>
        </div>

        {state === 'NEEDS_NAME' && (
          <Card>
            <CardHeader>
              <CardTitle>ようこそ</CardTitle>
              <CardDescription>
                参加するには、まず名前を入力してください
              </CardDescription>
            </CardHeader>
            <CardContent>
              <form onSubmit={handleJoinSession} className="space-y-4">
                <Input
                  type="text"
                  value={name}
                  onChange={(e) => setName(e.target.value)}
                  required
                  placeholder="あなたの名前"
                />
                {error && (
                  <p className="text-sm text-destructive">{error}</p>
                )}
                <Button
                  type="submit"
                  disabled={isLoading}
                  isLoading={isLoading}
                  className="w-full"
                >
                  参加する
                </Button>
              </form>
            </CardContent>
          </Card>
        )}

        {state === 'ANSWERING' && currentStatement && (
          <Card className={isLoading ? 'opacity-50 pointer-events-none' : ''}>
            <CardContent className="pt-6">
              <div className="mb-8">
                <p className="text-xl font-medium leading-relaxed">
                  {currentStatement.text}
                </p>
              </div>

              <div className="grid grid-cols-5 gap-3">
                <button
                  onClick={() => handleAnswer(2)}
                  disabled={isLoading}
                  className="group relative flex flex-col items-center gap-2 px-3 py-5 bg-emerald-500 hover:bg-emerald-600 text-white border-2 border-emerald-600 hover:border-emerald-700 rounded-lg transition-all shadow-sm hover:shadow-md active:scale-[0.98] disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  <div className="text-3xl">👍</div>
                  <span className="text-xs font-semibold">Strong Yes</span>
                </button>
                <button
                  onClick={() => handleAnswer(1)}
                  disabled={isLoading}
                  className="group relative flex flex-col items-center gap-2 px-3 py-5 bg-green-400 hover:bg-green-500 text-white border-2 border-green-500 hover:border-green-600 rounded-lg transition-all shadow-sm hover:shadow-md active:scale-[0.98] disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  <div className="text-3xl">✓</div>
                  <span className="text-xs font-semibold">Yes</span>
                </button>
                <button
                  onClick={() => handleAnswer(0)}
                  disabled={isLoading}
                  className="group relative flex flex-col items-center gap-2 px-3 py-5 bg-amber-400 hover:bg-amber-500 text-gray-900 border-2 border-amber-500 hover:border-amber-600 rounded-lg transition-all shadow-sm hover:shadow-md active:scale-[0.98] disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  <div className="text-3xl">🤔</div>
                  <span className="text-xs font-semibold">わからない</span>
                </button>
                <button
                  onClick={() => handleAnswer(-1)}
                  disabled={isLoading}
                  className="group relative flex flex-col items-center gap-2 px-3 py-5 bg-rose-400 hover:bg-rose-500 text-white border-2 border-rose-500 hover:border-rose-600 rounded-lg transition-all shadow-sm hover:shadow-md active:scale-[0.98] disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  <div className="text-3xl">✗</div>
                  <span className="text-xs font-semibold">No</span>
                </button>
                <button
                  onClick={() => handleAnswer(-2)}
                  disabled={isLoading}
                  className="group relative flex flex-col items-center gap-2 px-3 py-5 bg-red-600 hover:bg-red-700 text-white border-2 border-red-700 hover:border-red-800 rounded-lg transition-all shadow-sm hover:shadow-md active:scale-[0.98] disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  <div className="text-3xl">👎</div>
                  <span className="text-xs font-semibold">Strong No</span>
                </button>
              </div>

              {error && (
                <p className="text-sm text-destructive mt-4">{error}</p>
              )}
            </CardContent>
          </Card>
        )}

        {state === 'COMPLETED' && (
          <Card>
            <CardContent className="pt-6 pb-6 text-center">
              <div className="flex flex-col items-center gap-3 py-8">
                <div className="text-5xl mb-2">🎉</div>
                <p className="text-xl font-semibold">完了しました！</p>
                <p className="text-muted-foreground max-w-sm">
                  全ての質問への回答が完了しました。お疲れ様でした！
                </p>
              </div>
            </CardContent>
          </Card>
        )}

        {/* Individual Report Section - Show after joining */}
        {(state === 'ANSWERING' || state === 'COMPLETED') && (
          <Card className="mt-8">
            <CardHeader>
              <div className="flex items-center justify-between">
                <CardTitle>じぶんレポート</CardTitle>
                <Button
                  onClick={handleGenerateReport}
                  disabled={isGeneratingReport}
                  isLoading={isGeneratingReport}
                  variant="secondary"
                  size="sm"
                >
                  {individualReport ? 'レポートを更新' : 'レポートを生成'}
                </Button>
              </div>
              <CardDescription>
                あなたの回答から生成された個別分析レポート
              </CardDescription>
            </CardHeader>
            <CardContent>
              {error && (
                <div className="mb-4 p-3 bg-destructive/10 border border-destructive/20 rounded-md">
                  <p className="text-sm text-destructive">{error}</p>
                </div>
              )}
              {isGeneratingReport && (
                <div className="flex flex-col items-center justify-center py-8 space-y-4 mb-6 border-b pb-6">
                  <Loader2 className="h-10 w-10 text-primary animate-spin" />
                  <div className="text-center space-y-2">
                    <p className="text-base font-medium text-foreground">
                      レポートを生成しています...
                    </p>
                    <p className="text-sm text-muted-foreground">
                      あなたの回答を分析しています。少々お待ちください。
                    </p>
                  </div>
                </div>
              )}
              {isLoadingReport ? (
                <div className="space-y-3">
                  <Skeleton className="h-4 w-full" />
                  <Skeleton className="h-4 w-5/6" />
                  <Skeleton className="h-4 w-4/5" />
                  <div className="pt-2">
                    <Skeleton className="h-4 w-full" />
                    <Skeleton className="h-4 w-full mt-3" />
                    <Skeleton className="h-4 w-3/4 mt-3" />
                  </div>
                </div>
              ) : individualReport ? (
                <div className={`markdown-body prose prose-sm max-w-none ${isGeneratingReport ? 'opacity-60' : ''}`}>
                  <ReactMarkdown remarkPlugins={[remarkGfm]}>
                    {individualReport.contentMarkdown}
                  </ReactMarkdown>
                </div>
              ) : !isGeneratingReport ? (
                <div className="text-center py-8">
                  <div className="h-12 w-12 rounded-full bg-muted flex items-center justify-center mx-auto mb-3">
                    <FileText className="h-6 w-6 text-muted-foreground" />
                  </div>
                  <p className="text-sm text-muted-foreground">
                    回答を進めると、あなた専用の分析レポートがここに表示されます
                  </p>
                </div>
              ) : null}
            </CardContent>
          </Card>
        )}
      </div>
    </div>
  );
}
