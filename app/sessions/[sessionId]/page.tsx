"use client";

import { use, useCallback, useEffect, useRef, useState } from "react";
import { useUserId } from "@/lib/useUserId";
import { createAuthorizationHeader } from "@/lib/auth";
import axios from "axios";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";
import { Button } from "@/components/ui/Button";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
  Skeleton,
} from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import { FileText, Loader2, Printer } from "lucide-react";
import { cn } from "@/lib/utils";

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

type SessionState = "NEEDS_NAME" | "ANSWERING" | "COMPLETED";

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

type ResponseValue = -2 | -1 | 0 | 1 | 2;

type ParticipantResponse = {
  id?: string;
  statementId: string;
  statementText: string;
  orderIndex: number;
  value: ResponseValue;
  createdAt: string;
};

const RESPONSE_CHOICES: Array<{
  value: ResponseValue;
  label: string;
  emoji: string;
  idleClass: string;
  activeClass: string;
}> = [
  {
    value: 2,
    label: "Strong Yes",
    emoji: "💯",
    idleClass:
      "bg-emerald-50 text-emerald-700 border-emerald-200 hover:bg-emerald-100",
    activeClass:
      "bg-emerald-500 text-white border-emerald-500 shadow-sm hover:bg-emerald-500",
  },
  {
    value: 1,
    label: "Yes",
    emoji: "✓",
    idleClass: "bg-green-50 text-green-700 border-green-200 hover:bg-green-100",
    activeClass:
      "bg-green-400 text-white border-green-400 shadow-sm hover:bg-green-400",
  },
  {
    value: 0,
    label: "わからない",
    emoji: "🤔",
    idleClass: "bg-amber-50 text-amber-700 border-amber-200 hover:bg-amber-100",
    activeClass:
      "bg-amber-400 text-gray-900 border-amber-400 shadow-sm hover:bg-amber-400",
  },
  {
    value: -1,
    label: "No",
    emoji: "✗",
    idleClass: "bg-rose-50 text-rose-700 border-rose-200 hover:bg-rose-100",
    activeClass:
      "bg-rose-400 text-white border-rose-400 shadow-sm hover:bg-rose-400",
  },
  {
    value: -2,
    label: "Strong No",
    emoji: "👎",
    idleClass: "bg-red-50 text-red-700 border-red-200 hover:bg-red-100",
    activeClass:
      "bg-red-600 text-white border-red-600 shadow-sm hover:bg-red-600",
  },
];

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
  const [state, setState] = useState<SessionState>("NEEDS_NAME");
  const [name, setName] = useState("");
  const [currentStatement, setCurrentStatement] = useState<Statement | null>(
    null,
  );
  const [prefetchedStatement, setPrefetchedStatement] = useState<
    Statement | null | undefined
  >(undefined);
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [individualReport, setIndividualReport] =
    useState<IndividualReport | null>(null);
  const [isGeneratingReport, setIsGeneratingReport] = useState(false);
  const [isCheckingParticipation, setIsCheckingParticipation] = useState(false);
  const [isLoadingReport, setIsLoadingReport] = useState(true);
  const [participantResponses, setParticipantResponses] = useState<
    ParticipantResponse[]
  >([]);
  const [isLoadingResponses, setIsLoadingResponses] = useState(false);
  const [responsesError, setResponsesError] = useState<string | null>(null);
  const [updatingResponseIds, setUpdatingResponseIds] = useState<Set<string>>(
    new Set(),
  );
  const hasJustCompletedRef = useRef(false);
  const pendingAnswerStatementIdsRef = useRef<Set<string>>(new Set());
  const sessionInfoId = sessionInfo?.id;
  const sortResponsesByRecency = useCallback((items: ParticipantResponse[]) => {
    return [...items].sort((a, b) => {
      const timeDiff =
        new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime();
      if (timeDiff !== 0) {
        return timeDiff;
      }

      return a.orderIndex - b.orderIndex;
    });
  }, []);
  const fetchParticipantResponses = useCallback(async () => {
    if (!userId) return;
    setIsLoadingResponses(true);
    setResponsesError(null);

    try {
      const response = await axios.get(`/api/sessions/${sessionId}/responses`, {
        headers: createAuthorizationHeader(userId),
      });

      const items = (response.data.responses ?? []) as Array<{
        id?: string;
        statementId: string;
        statementText: string;
        orderIndex?: number;
        value: ResponseValue;
        createdAt?: string;
      }>;

      const mapped = items.map((item) => ({
        id: item.id,
        statementId: item.statementId,
        statementText: item.statementText,
        orderIndex: item.orderIndex ?? 0,
        value: item.value,
        createdAt: item.createdAt ?? new Date().toISOString(),
      }));

      setParticipantResponses(sortResponsesByRecency(mapped));
    } catch (err) {
      console.error("Failed to fetch participant responses:", err);
      if (axios.isAxiosError(err) && err.response?.status === 404) {
        setParticipantResponses([]);
        setResponsesError(null);
      } else {
        setResponsesError(
          "これまでの回答を取得できませんでした。ページを更新して再度お試しください。",
        );
      }
    } finally {
      setIsLoadingResponses(false);
    }
  }, [sessionId, userId, sortResponsesByRecency]);

  const upsertParticipantResponse = useCallback(
    (statement: Statement, value: ResponseValue) => {
      setParticipantResponses((prev) => {
        const existing = prev.find((item) => item.statementId === statement.id);
        const nextResponse: ParticipantResponse = {
          id: existing?.id,
          statementId: statement.id,
          statementText: statement.text,
          orderIndex: statement.orderIndex,
          value,
          createdAt: existing?.createdAt ?? new Date().toISOString(),
        };

        if (existing) {
          return sortResponsesByRecency(
            prev.map((item) =>
              item.statementId === statement.id
                ? { ...item, ...nextResponse }
                : item,
            ),
          );
        }

        return sortResponsesByRecency([...prev, nextResponse]);
      });
    },
    [sortResponsesByRecency],
  );

  const revertParticipantResponse = useCallback(
    (statementId: string, previous: ParticipantResponse | null) => {
      setParticipantResponses((prev) => {
        if (previous) {
          const exists = prev.some((item) => item.statementId === statementId);
          const updatedList = exists
            ? prev.map((item) =>
                item.statementId === statementId ? previous : item,
              )
            : [...prev, previous];

          return sortResponsesByRecency(updatedList);
        }
        return sortResponsesByRecency(
          prev.filter((item) => item.statementId !== statementId),
        );
      });
    },
    [sortResponsesByRecency],
  );

  const addUpdatingResponseId = useCallback((statementId: string) => {
    setUpdatingResponseIds((prev) => {
      const next = new Set(prev);
      next.add(statementId);
      return next;
    });
  }, []);

  const removeUpdatingResponseId = useCallback((statementId: string) => {
    setUpdatingResponseIds((prev) => {
      const next = new Set(prev);
      next.delete(statementId);
      return next;
    });
  }, []);

  const syncParticipantResponseFromServer = useCallback(
    (payload: {
      id: string;
      statementId: string;
      value: number;
      createdAt: string;
    }) => {
      setParticipantResponses((prev) =>
        sortResponsesByRecency(
          prev.map((item) =>
            item.statementId === payload.statementId
              ? {
                  ...item,
                  id: payload.id,
                  value: payload.value as ResponseValue,
                  createdAt: payload.createdAt,
                }
              : item,
          ),
        ),
      );
    },
    [sortResponsesByRecency],
  );

  const buildExcludeQuery = (additionalIds: string[] = []) => {
    const ids = new Set<string>(additionalIds.filter(Boolean));

    if (currentStatement) {
      ids.add(currentStatement.id);
    }

    pendingAnswerStatementIdsRef.current.forEach((id) => ids.add(id));

    if (ids.size === 0) {
      return "";
    }

    const query = Array.from(ids)
      .map((id) => `excludeStatementId=${encodeURIComponent(id)}`)
      .join("&");

    return `?${query}`;
  };

  useEffect(() => {
    if (!userId || userLoading) return;

    const fetchSessionInfo = async () => {
      setIsSessionInfoLoading(true);
      try {
        const response = await axios.get(`/api/sessions/${sessionId}`, {
          headers: createAuthorizationHeader(userId),
        });
        setSessionInfo(response.data.session);
        setSessionInfoError(null);
      } catch (err: unknown) {
        console.error("Failed to fetch session info:", err);
        setSessionInfo(null);
        if (axios.isAxiosError(err)) {
          if (err.response?.status === 404) {
            setSessionInfoError("セッションが見つかりませんでした。");
          } else if (err.response?.status === 403) {
            setSessionInfoError(
              "このセッションにアクセスする権限がありません。",
            );
          } else {
            setSessionInfoError("セッション情報の取得に失敗しました。");
          }
        } else {
          setSessionInfoError("セッション情報の取得に失敗しました。");
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
    if (state !== "NEEDS_NAME") return;

    // Check if already participating
    const checkParticipation = async () => {
      setIsCheckingParticipation(true);
      try {
        const response = await axios.get(
          `/api/sessions/${sessionId}/statements/next`,
          { headers: createAuthorizationHeader(userId) },
        );

        // If we got a statement, user is already participating
        if (response.data.statement) {
          setCurrentStatement(response.data.statement);
          setState("ANSWERING");
          setSessionInfo((prev) =>
            prev ? { ...prev, isParticipant: true } : prev,
          );
        } else {
          setState("COMPLETED");
          setSessionInfo((prev) =>
            prev ? { ...prev, isParticipant: true } : prev,
          );
        }
      } catch (err: unknown) {
        // If error is 401, user hasn't joined yet
        if (axios.isAxiosError(err) && err.response?.status === 401) {
          setState("NEEDS_NAME");
          setSessionInfo((prev) =>
            prev ? { ...prev, isParticipant: false } : prev,
          );
        } else {
          setState("NEEDS_NAME");
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
    if (state !== "ANSWERING") return;
    if (!currentStatement) return;

    // Reset prefetch state to undefined (loading state)
    setPrefetchedStatement(undefined);

    const prefetchNextStatement = async () => {
      try {
        const excludeQuery = buildExcludeQuery();
        const response = await axios.get(
          `/api/sessions/${sessionId}/statements/next${excludeQuery}`,
          {
            headers: createAuthorizationHeader(userId),
          },
        );

        if (response.data.statement) {
          setPrefetchedStatement(response.data.statement);
        } else {
          // null means this is the last question
          setPrefetchedStatement(null);
        }
      } catch (err) {
        // Silently fail prefetch - keep as undefined to trigger fallback
        console.error("Prefetch failed:", err);
        setPrefetchedStatement(undefined);
      }
    };

    prefetchNextStatement();
  }, [userId, userLoading, sessionId, currentStatement, state]);

  useEffect(() => {
    if (!userId || userLoading) return;
    if (state === "NEEDS_NAME") return;

    const fetchIndividualReport = async () => {
      setIsLoadingReport(true);
      try {
        const response = await axios.get(
          `/api/sessions/${sessionId}/individual-report`,
          { headers: createAuthorizationHeader(userId) },
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
        console.error("Failed to fetch individual report:", err);
      } finally {
        setIsLoadingReport(false);
      }
    };

    fetchIndividualReport();
  }, [userId, userLoading, sessionId, state]);

  useEffect(() => {
    if (!userId || userLoading) return;
    if (state === "NEEDS_NAME") return;

    fetchParticipantResponses();
  }, [userId, userLoading, state, fetchParticipantResponses]);

  // Auto-generate report when all questions are answered
  useEffect(() => {
    if (!userId || userLoading) return;
    if (state !== "COMPLETED") return;

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
          { headers: createAuthorizationHeader(userId) },
        );

        setIndividualReport(response.data.report);
      } catch (err) {
        console.error("Failed to auto-generate report:", err);
        // Show error to user so they know auto-generation failed
        if (axios.isAxiosError(err) && err.response?.data?.error) {
          setError(
            `レポートの自動生成に失敗しました: ${err.response.data.error}`,
          );
        } else {
          setError(
            "レポートの自動生成に失敗しました。「レポートを生成」ボタンから手動で生成してください。",
          );
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
        { headers: createAuthorizationHeader(userId) },
      );

      // Fetch first statement
      const response = await axios.get(
        `/api/sessions/${sessionId}/statements/next`,
        { headers: createAuthorizationHeader(userId) },
      );

      if (response.data.statement) {
        setCurrentStatement(response.data.statement);
        setState("ANSWERING");
      } else {
        // Set flag to trigger auto-generation (edge case: no questions in session)
        hasJustCompletedRef.current = true;
        setState("COMPLETED");
      }
      setSessionInfo((prev) =>
        prev ? { ...prev, isParticipant: true } : prev,
      );
    } catch (err) {
      console.error("Failed to join session:", err);
      if (axios.isAxiosError(err) && err.response?.data?.error) {
        setError(`エラー: ${err.response.data.error}`);
      } else {
        setError("セッションへの参加に失敗しました。");
      }
    } finally {
      setIsLoading(false);
    }
  };

  const handleAnswer = async (value: ResponseValue) => {
    if (!userId || !currentStatement || isLoading) return;

    const previousStatement = currentStatement;
    const cachedNextStatement = prefetchedStatement;
    const previousResponse = participantResponses.find(
      (item) => item.statementId === previousStatement.id,
    );
    const previousResponseSnapshot = previousResponse
      ? { ...previousResponse }
      : null;
    setError(null);
    upsertParticipantResponse(previousStatement, value);
    pendingAnswerStatementIdsRef.current.add(previousStatement.id);
    const clearPendingStatement = () => {
      pendingAnswerStatementIdsRef.current.delete(previousStatement.id);
    };
    const revertOnFailure = () => {
      revertParticipantResponse(previousStatement.id, previousResponseSnapshot);
    };

    try {
      // undefined: prefetch is still loading or failed
      // null: this is the last question (no more questions available)
      // Statement object: next question is ready

      if (cachedNextStatement === null) {
        // This is the last question - submit answer and show completion
        setIsLoading(true);

        const postResult = await axios.post(
          `/api/sessions/${sessionId}/responses`,
          { statementId: previousStatement.id, value },
          { headers: createAuthorizationHeader(userId) },
        );
        const serverResponse = postResult.data?.response;
        if (serverResponse) {
          syncParticipantResponseFromServer(serverResponse);
        }
        clearPendingStatement();

        // Set flag to trigger auto-generation
        hasJustCompletedRef.current = true;
        setState("COMPLETED");
        setCurrentStatement(null);
        setPrefetchedStatement(undefined);
        setIsLoading(false);
      } else if (cachedNextStatement) {
        // We have a prefetched statement - use it immediately for instant transition
        setCurrentStatement(cachedNextStatement);
        setPrefetchedStatement(undefined);

        // Submit answer in background (no need to wait)
        axios
          .post(
            `/api/sessions/${sessionId}/responses`,
            { statementId: previousStatement.id, value },
            { headers: createAuthorizationHeader(userId) },
          )
          .then((res) => {
            const serverResponse = res.data?.response;
            if (serverResponse) {
              syncParticipantResponseFromServer(serverResponse);
            }
          })
          .catch((err) => {
            console.error("Failed to submit answer:", err);
            if (axios.isAxiosError(err) && err.response?.data?.error) {
              setError(`エラー: ${err.response.data.error}`);
            } else {
              setError("回答の送信に失敗しました。");
            }
            revertOnFailure();
          })
          .finally(() => {
            clearPendingStatement();
          });
      } else {
        // cachedNextStatement === undefined
        // Prefetch hasn't completed yet (rapid clicking) - fall back to original behavior
        setIsLoading(true);

        const [postResponse, nextResponse] = await Promise.all([
          axios.post(
            `/api/sessions/${sessionId}/responses`,
            { statementId: previousStatement.id, value },
            { headers: createAuthorizationHeader(userId) },
          ),
          axios.get(
            `/api/sessions/${sessionId}/statements/next${buildExcludeQuery([previousStatement.id])}`,
            { headers: createAuthorizationHeader(userId) },
          ),
        ]);
        clearPendingStatement();
        const serverResponse = postResponse.data?.response;
        if (serverResponse) {
          syncParticipantResponseFromServer(serverResponse);
        }

        // Update to next question
        if (nextResponse.data.statement) {
          setCurrentStatement(nextResponse.data.statement);
        } else {
          // Set flag to trigger auto-generation
          hasJustCompletedRef.current = true;
          setState("COMPLETED");
          setCurrentStatement(null);
        }

        setIsLoading(false);
      }
    } catch (err) {
      clearPendingStatement();
      console.error("Failed to submit answer:", err);
      revertOnFailure();
      if (axios.isAxiosError(err) && err.response?.data?.error) {
        setError(`エラー: ${err.response.data.error}`);
      } else {
        setError("回答の送信に失敗しました。");
      }
      setIsLoading(false);
    }
  };

  const handleUpdateResponse = async (
    statementId: string,
    value: ResponseValue,
  ) => {
    if (!userId) return;

    const currentResponse = participantResponses.find(
      (item) => item.statementId === statementId,
    );

    if (!currentResponse || currentResponse.value === value) {
      return;
    }

    const previousSnapshot: ParticipantResponse = { ...currentResponse };
    const stubStatement: Statement = {
      id: statementId,
      text: currentResponse.statementText,
      orderIndex: currentResponse.orderIndex,
      sessionId,
    };

    setResponsesError(null);
    upsertParticipantResponse(stubStatement, value);
    addUpdatingResponseId(statementId);

    try {
      const res = await axios.post(
        `/api/sessions/${sessionId}/responses`,
        { statementId, value },
        { headers: createAuthorizationHeader(userId) },
      );
      const serverResponse = res.data?.response;
      if (serverResponse) {
        syncParticipantResponseFromServer(serverResponse);
      }
    } catch (err) {
      console.error("Failed to update response:", err);
      revertParticipantResponse(statementId, previousSnapshot);
      if (axios.isAxiosError(err) && err.response?.data?.error) {
        setResponsesError(
          `回答の更新に失敗しました: ${err.response.data.error}`,
        );
      } else {
        setResponsesError(
          "回答の更新に失敗しました。時間をおいて再度お試しください。",
        );
      }
    } finally {
      removeUpdatingResponseId(statementId);
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
        { headers: createAuthorizationHeader(userId) },
      );

      setIndividualReport(response.data.report);
    } catch (err) {
      console.error("Failed to generate report:", err);
      if (axios.isAxiosError(err) && err.response?.data?.error) {
        setError(`エラー: ${err.response.data.error}`);
      } else {
        setError("レポートの生成に失敗しました。");
      }
    } finally {
      setIsGeneratingReport(false);
    }
  };

  // Update document title when session info is available
  useEffect(() => {
    if (sessionInfo?.title) {
      document.title = `${sessionInfo.title} - セッションに参加 - Cartographer`;
    }
    return () => {
      document.title = "Cartographer - 認識を可視化し、合意形成を促進する";
    };
  }, [sessionInfo?.title]);

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
            <h1 className="text-3xl font-bold tracking-tight">セッション</h1>
          </div>
          <Card>
            <CardContent className="pt-6 pb-6">
              <p className="text-sm text-muted-foreground">
                {sessionInfoError}
              </p>
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
            {sessionInfo?.title ?? "セッション"}
          </h1>
        </div>

        {state === "NEEDS_NAME" && (
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
                {error && <p className="text-sm text-destructive">{error}</p>}
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

        {state === "ANSWERING" && currentStatement && (
          <Card className={isLoading ? "opacity-50 pointer-events-none" : ""}>
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

        {state === "COMPLETED" && (
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

        {/* Previous Responses & Individual Report - Show after joining */}
        {(state === "ANSWERING" || state === "COMPLETED") && (
          <>
            <Card className="mt-8">
              <CardHeader>
                <CardTitle>これまでの回答</CardTitle>
              </CardHeader>
              <CardContent>
                {responsesError && (
                  <div className="mb-4 rounded-md border border-destructive/20 bg-destructive/10 p-3">
                    <p className="text-sm text-destructive">{responsesError}</p>
                  </div>
                )}
                {isLoadingResponses ? (
                  <div className="space-y-3">
                    {[0, 1, 2].map((index) => (
                      <div
                        key={index}
                        className="space-y-2 rounded-lg border border-border/40 bg-muted/20 p-3"
                      >
                        <Skeleton className="h-4 w-3/4" />
                        <div className="flex gap-2">
                          <Skeleton className="h-6 w-20" />
                          <Skeleton className="h-6 w-16" />
                          <Skeleton className="h-6 w-24" />
                        </div>
                      </div>
                    ))}
                  </div>
                ) : participantResponses.length > 0 ? (
                  <div className="max-h-64 space-y-3 overflow-y-auto pr-1">
                    {participantResponses.map((response) => {
                      const isPending =
                        pendingAnswerStatementIdsRef.current.has(
                          response.statementId,
                        );
                      const isUpdating = updatingResponseIds.has(
                        response.statementId,
                      );
                      return (
                        <div
                          key={response.statementId}
                          className="rounded-lg border border-border/60 bg-muted/20 p-3 shadow-sm"
                        >
                          <p className="text-sm font-medium text-foreground">
                            {response.statementText}
                          </p>
                          <div className="mt-3 flex flex-wrap gap-2">
                            {RESPONSE_CHOICES.map((choice) => {
                              const isActive = response.value === choice.value;
                              const isDisabled =
                                isPending ||
                                isUpdating ||
                                isLoading ||
                                isActive;

                              return (
                                <button
                                  key={choice.value}
                                  type="button"
                                  onClick={() =>
                                    handleUpdateResponse(
                                      response.statementId,
                                      choice.value,
                                    )
                                  }
                                  disabled={isDisabled}
                                  className={cn(
                                    "flex items-center gap-1 rounded-full border px-3 py-1.5 text-xs font-semibold transition-all focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-offset-2",
                                    isActive
                                      ? choice.activeClass
                                      : choice.idleClass,
                                    (isPending || isUpdating) && "opacity-70",
                                  )}
                                >
                                  <span>{choice.emoji}</span>
                                  <span>{choice.label}</span>
                                </button>
                              );
                            })}
                          </div>
                        </div>
                      );
                    })}
                  </div>
                ) : (
                  <div className="rounded-lg border border-dashed border-border/60 bg-muted/20 py-8 text-center">
                    <p className="text-sm text-muted-foreground">
                      回答をするとここに一覧が表示され、いつでも更新できます
                    </p>
                  </div>
                )}
              </CardContent>
            </Card>

            <Card className="mt-8">
              <CardHeader>
                <div className="flex items-center justify-between">
                  <CardTitle>じぶんレポート</CardTitle>
                  <div className="flex items-center gap-2">
                    {individualReport && (
                      <Button
                        onClick={() =>
                          window.open(
                            `/sessions/${sessionId}/print/individual`,
                            "_blank"
                          )
                        }
                        variant="ghost"
                        size="sm"
                        title="印刷用ページを開く"
                      >
                        <Printer className="w-4 h-4" />
                      </Button>
                    )}
                    <Button
                      onClick={handleGenerateReport}
                      disabled={isGeneratingReport}
                      isLoading={isGeneratingReport}
                      variant="secondary"
                      size="sm"
                    >
                      {individualReport ? "レポートを更新" : "レポートを生成"}
                    </Button>
                  </div>
                </div>
                <CardDescription>
                  あなたの回答から生成された個別分析レポート
                </CardDescription>
              </CardHeader>
              <CardContent>
                {error && (
                  <div className="mb-4 rounded-md border border-destructive/20 bg-destructive/10 p-3">
                    <p className="text-sm text-destructive">{error}</p>
                  </div>
                )}
                {isGeneratingReport && (
                  <div className="mb-6 flex flex-col items-center justify-center space-y-4 border-b pb-6 pt-8">
                    <Loader2 className="h-10 w-10 animate-spin text-primary" />
                    <div className="space-y-2 text-center">
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
                      <Skeleton className="mt-3 h-4 w-full" />
                      <Skeleton className="mt-3 h-4 w-3/4" />
                    </div>
                  </div>
                ) : individualReport ? (
                  <div
                    className={cn(
                      "markdown-body prose prose-sm max-w-none",
                      isGeneratingReport && "opacity-60",
                    )}
                  >
                    <ReactMarkdown remarkPlugins={[remarkGfm]}>
                      {individualReport.contentMarkdown}
                    </ReactMarkdown>
                  </div>
                ) : !isGeneratingReport ? (
                  <div className="py-8 text-center">
                    <div className="mb-3 mx-auto flex h-12 w-12 items-center justify-center rounded-full bg-muted">
                      <FileText className="h-6 w-6 text-muted-foreground" />
                    </div>
                    <p className="text-sm text-muted-foreground">
                      回答を進めると、あなた専用の分析レポートがここに表示されます
                    </p>
                  </div>
                ) : null}
              </CardContent>
            </Card>
          </>
        )}
      </div>
    </div>
  );
}
