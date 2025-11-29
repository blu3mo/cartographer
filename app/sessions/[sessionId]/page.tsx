"use client";

import axios from "axios";
import { FileText, Info, Loader2 } from "lucide-react";
import { use, useCallback, useEffect, useMemo, useRef, useState } from "react";
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
import { createAuthorizationHeader } from "@/lib/auth";
import { useUserId } from "@/lib/useUserId";
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

type SessionState = "NEEDS_NAME" | "ANSWERING" | "REFLECTION" | "COMPLETED";

type SessionInfo = {
  id: string;
  title: string;
  goal: string;
  context: string;
  isPublic: boolean;
  hostUserId: string;
  createdAt: string;
  updatedAt: string;
  isHost: boolean;
  isParticipant: boolean;
};

type ResponseValue = -2 | -1 | 0 | 1 | 2;
type ResponseType = "scale" | "free_text";

type ParticipantResponse = {
  id?: string;
  statementId: string;
  statementText: string;
  orderIndex: number;
  responseType: ResponseType;
  value: ResponseValue | null;
  textResponse?: string | null;
  createdAt: string;
};

type ParticipantReflection = {
  id: string;
  text: string;
  createdAt: string;
  submittedAt: string;
};

type HistoryEntry =
  | {
      type: "response";
      createdAt: string;
      response: ParticipantResponse;
      key: string;
    }
  | {
      type: "reflection";
      createdAt: string;
      reflection: ParticipantReflection;
      key: string;
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
    label: "強く同意",
    emoji: "💯",
    idleClass:
      "bg-emerald-50 text-emerald-700 border-emerald-200 hover:bg-emerald-100",
    activeClass:
      "bg-emerald-500 text-white border-emerald-500 shadow-sm hover:bg-emerald-500",
  },
  {
    value: 1,
    label: "同意",
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
    label: "反対",
    emoji: "✗",
    idleClass: "bg-rose-50 text-rose-700 border-rose-200 hover:bg-rose-100",
    activeClass:
      "bg-rose-400 text-white border-rose-400 shadow-sm hover:bg-rose-400",
  },
  {
    value: -2,
    label: "強く反対",
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
  const [remainingQuestions, setRemainingQuestions] = useState<number | null>(
    null,
  );
  const [prefetchedRemainingQuestions, setPrefetchedRemainingQuestions] =
    useState<number | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [freeTextInput, setFreeTextInput] = useState("");
  const [isSubmittingFreeText, setIsSubmittingFreeText] = useState(false);
  const [showAlternatives, setShowAlternatives] = useState(false);
  const [aiSuggestions, setAiSuggestions] = useState<string[]>([]);
  const [isLoadingSuggestions, setIsLoadingSuggestions] = useState(false);
  const [prefetchedAiSuggestions, setPrefetchedAiSuggestions] = useState<
    string[] | undefined
  >(undefined);
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
  const [participantReflections, setParticipantReflections] = useState<
    ParticipantReflection[]
  >([]);
  const [isLoadingReflections, setIsLoadingReflections] = useState(false);
  const [reflectionsError, setReflectionsError] = useState<string | null>(null);
  const [reflectionText, setReflectionText] = useState("");
  const [isSubmittingReflection, setIsSubmittingReflection] = useState(false);
  const [reflectionSubmissionError, setReflectionSubmissionError] = useState<
    string | null
  >(null);
  const hasJustCompletedRef = useRef(false);
  const pendingAnswerStatementIdsRef = useRef<Set<string>>(new Set());
  const prefetchedStatementIdRef = useRef<string | null>(null);
  const freeTextSectionRef = useRef<HTMLDivElement | null>(null);
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
  const historyItems = useMemo<HistoryEntry[]>(() => {
    const responses = participantResponses.map((response) => ({
      type: "response" as const,
      createdAt: response.createdAt,
      response,
      key: `response-${response.statementId}`,
    }));
    const reflections = participantReflections.map((reflection) => ({
      type: "reflection" as const,
      createdAt: reflection.submittedAt ?? reflection.createdAt,
      reflection,
      key: `reflection-${reflection.id}`,
    }));

    return [...responses, ...reflections].sort((a, b) => {
      const diff =
        new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime();
      if (diff !== 0) {
        return diff;
      }
      return a.key.localeCompare(b.key);
    });
  }, [participantResponses, participantReflections]);
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
        responseType?: ResponseType;
        value?: ResponseValue | null;
        textResponse?: string | null;
        createdAt?: string;
      }>;

      const mapped = items.map((item) => ({
        id: item.id,
        statementId: item.statementId,
        statementText: item.statementText,
        orderIndex: item.orderIndex ?? 0,
        responseType: item.responseType ?? "scale",
        value: item.value ?? null,
        textResponse: item.textResponse ?? null,
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
  const fetchParticipantReflections = useCallback(async () => {
    if (!userId) return;
    setIsLoadingReflections(true);
    setReflectionsError(null);

    try {
      const response = await axios.get(
        `/api/sessions/${sessionId}/reflections`,
        {
          headers: createAuthorizationHeader(userId),
        },
      );

      const items = (response.data.reflections ?? []) as Array<{
        id: string;
        text: string;
        createdAt?: string;
        submittedAt?: string;
      }>;

      setParticipantReflections(
        items
          .map((item) => ({
            id: item.id,
            text: item.text,
            createdAt: item.createdAt ?? new Date().toISOString(),
            submittedAt: item.submittedAt ?? new Date().toISOString(),
          }))
          .sort(
            (a, b) =>
              new Date(b.submittedAt).getTime() -
              new Date(a.submittedAt).getTime(),
          ),
      );
    } catch (err) {
      console.error("Failed to fetch participant reflections:", err);
      if (axios.isAxiosError(err) && err.response?.status === 404) {
        setParticipantReflections([]);
        setReflectionsError(null);
      } else {
        setReflectionsError(
          "これまでのふりかえりを取得できませんでした。更新して再度お試しください。",
        );
      }
    } finally {
      setIsLoadingReflections(false);
    }
  }, [sessionId, userId]);

  const upsertParticipantResponse = useCallback(
    (
      statement: Statement,
      response: {
        responseType: ResponseType;
        value: ResponseValue | null;
        textResponse?: string | null;
      },
    ) => {
      setParticipantResponses((prev) => {
        const existing = prev.find((item) => item.statementId === statement.id);
        const nextResponse: ParticipantResponse = {
          id: existing?.id,
          statementId: statement.id,
          statementText: statement.text,
          orderIndex: statement.orderIndex,
          responseType: response.responseType,
          value: response.value,
          textResponse: response.textResponse ?? null,
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
      value: number | null;
      responseType: ResponseType;
      textResponse?: string | null;
      statementText?: string;
      orderIndex?: number;
      createdAt: string;
    }) => {
      setParticipantResponses((prev) =>
        sortResponsesByRecency(
          prev.map((item) =>
            item.statementId === payload.statementId
              ? {
                  ...item,
                  id: payload.id,
                  value: payload.value as ResponseValue | null,
                  responseType: payload.responseType,
                  textResponse: payload.textResponse ?? item.textResponse,
                  statementText:
                    payload.statementText ?? item.statementText ?? "",
                  orderIndex: payload.orderIndex ?? item.orderIndex ?? 0,
                  createdAt: payload.createdAt,
                }
              : item,
          ),
        ),
      );
    },
    [sortResponsesByRecency],
  );

  const buildExcludeQuery = useCallback(
    (additionalIds: string[] = []) => {
      const ids = new Set<string>(additionalIds.filter(Boolean));

      if (currentStatement) {
        ids.add(currentStatement.id);
      }

      pendingAnswerStatementIdsRef.current.forEach((id) => {
        ids.add(id);
      });

      if (ids.size === 0) {
        return "";
      }

      const query = Array.from(ids)
        .map((id) => `excludeStatementId=${encodeURIComponent(id)}`)
        .join("&");

      return `?${query}`;
    },
    [currentStatement],
  );

  const enterReflectionMode = useCallback(() => {
    setReflectionSubmissionError(null);
    setReflectionText("");
    setCurrentStatement(null);
    setPrefetchedStatement(undefined);
    setRemainingQuestions(0);
    setPrefetchedRemainingQuestions(null);
    setAiSuggestions([]);
    setIsLoadingSuggestions(false);
    setPrefetchedAiSuggestions(undefined);
    hasJustCompletedRef.current = true;
    setState("REFLECTION");
  }, []);

  const enterCompletedState = useCallback(() => {
    hasJustCompletedRef.current = true;
    setState("COMPLETED");
    setCurrentStatement(null);
    setPrefetchedStatement(undefined);
    setRemainingQuestions(0);
    setPrefetchedRemainingQuestions(null);
    setAiSuggestions([]);
    setIsLoadingSuggestions(false);
    setPrefetchedAiSuggestions(undefined);
  }, []);

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

  // Reset UI state when statement changes
  useEffect(() => {
    if (currentStatement) {
      setShowAlternatives(false);
      setFreeTextInput("");
    }
  }, [currentStatement]);

  // Prefetch next statement and AI suggestions when current statement is displayed
  useEffect(() => {
    if (!userId || userLoading) return;
    if (state !== "ANSWERING") return;
    if (!currentStatement) return;

    // Check if this statement came from prefetch
    const isFromPrefetch = prefetchedStatementIdRef.current === currentStatement.id;

    if (isFromPrefetch) {
      // This statement came from prefetch, suggestions are already set
      // Reset the ref for next time
      prefetchedStatementIdRef.current = null;
    } else {
      // This is a fresh statement (e.g., first load, or fallback path)
      // Need to fetch suggestions for current statement
      setAiSuggestions([]);
      setIsLoadingSuggestions(true);

      const prefetchCurrentSuggestions = async () => {
        try {
          const response = await axios.get(
            `/api/sessions/${sessionId}/statements/${currentStatement.id}/suggestions`,
            {
              headers: createAuthorizationHeader(userId),
            },
          );

          if (response.data.suggestions && Array.isArray(response.data.suggestions)) {
            setAiSuggestions(response.data.suggestions);
          }
        } catch (err) {
          console.error("Failed to fetch AI suggestions:", err);
          // Set fallback suggestions
          setAiSuggestions([
            "状況によって賛成できる",
            "一部には賛成だが全体には反対",
            "今は判断できない",
          ]);
        } finally {
          setIsLoadingSuggestions(false);
        }
      };

      prefetchCurrentSuggestions();
    }

    // Always reset prefetch state and prefetch next statement
    setPrefetchedStatement(undefined);
    setPrefetchedRemainingQuestions(null);
    setPrefetchedAiSuggestions(undefined);

    const prefetchNextStatementAndSuggestions = async () => {
      try {
        const excludeQuery = buildExcludeQuery();
        const response = await axios.get(
          `/api/sessions/${sessionId}/statements/next${excludeQuery}`,
          {
            headers: createAuthorizationHeader(userId),
          },
        );

        if (response.data.statement) {
          const nextStatement = response.data.statement;
          setPrefetchedStatement(nextStatement);
          setPrefetchedRemainingQuestions(response.data.remainingCount ?? null);

          // Prefetch suggestions for the next statement
          try {
            const suggestionsResponse = await axios.get(
              `/api/sessions/${sessionId}/statements/${nextStatement.id}/suggestions`,
              {
                headers: createAuthorizationHeader(userId),
              },
            );

            if (suggestionsResponse.data.suggestions && Array.isArray(suggestionsResponse.data.suggestions)) {
              setPrefetchedAiSuggestions(suggestionsResponse.data.suggestions);
            } else {
              setPrefetchedAiSuggestions([]);
            }
          } catch (err) {
            console.error("Failed to prefetch AI suggestions for next statement:", err);
            // Set fallback suggestions for next statement
            setPrefetchedAiSuggestions([
              "状況によって賛成できる",
              "一部には賛成だが全体には反対",
              "今は判断できない",
            ]);
          }
        } else {
          // null means this is the last question
          setPrefetchedStatement(null);
          setPrefetchedRemainingQuestions(0);
          setPrefetchedAiSuggestions(undefined); // No next statement, so no suggestions needed
        }
      } catch (err) {
        // Silently fail prefetch - keep as undefined to trigger fallback
        console.error("Prefetch failed:", err);
        setPrefetchedStatement(undefined);
        setPrefetchedRemainingQuestions(null);
        setPrefetchedAiSuggestions(undefined);
      }
    };

    prefetchNextStatementAndSuggestions();
  }, [
    userId,
    userLoading,
    sessionId,
    currentStatement,
    state,
    buildExcludeQuery,
  ]);

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
    fetchParticipantReflections();
  }, [
    userId,
    userLoading,
    state,
    fetchParticipantResponses,
    fetchParticipantReflections,
  ]);

  // Auto-generate report when all questions are answered (even before reflection submission)
  useEffect(() => {
    if (!userId || userLoading) return;
    if (state !== "COMPLETED" && state !== "REFLECTION") return;

    // Only auto-generate if we just transitioned out of answering (not on page reload)
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
        setRemainingQuestions(response.data.remainingCount ?? null);
      } else {
        // Set flag to trigger auto-generation (edge case: no questions in session)
        hasJustCompletedRef.current = true;
        setState("COMPLETED");
        setRemainingQuestions(0);
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

  const handleSubmitResponse = async (payload: {
    responseType: ResponseType;
    value?: ResponseValue;
    textResponse?: string;
  }) => {
    if (!userId || !currentStatement || isLoading) return;
    if (
      payload.responseType === "free_text" &&
      (!payload.textResponse || payload.textResponse.trim().length === 0)
    ) {
      setError("自由記述を入力してください。");
      return;
    }
    if (payload.responseType === "free_text") {
      setIsSubmittingFreeText(true);
    }

    const previousStatement = currentStatement;
    const cachedNextStatement = prefetchedStatement;
    const previousResponse = participantResponses.find(
      (item) => item.statementId === previousStatement.id,
    );
    const previousResponseSnapshot = previousResponse
      ? { ...previousResponse }
      : null;

    setError(null);
    upsertParticipantResponse(previousStatement, {
      responseType: payload.responseType,
      value: payload.responseType === "scale" ? payload.value ?? null : null,
      textResponse:
        payload.responseType === "free_text"
          ? payload.textResponse ?? ""
          : null,
    });
    pendingAnswerStatementIdsRef.current.add(previousStatement.id);
    const clearPendingStatement = () => {
      pendingAnswerStatementIdsRef.current.delete(previousStatement.id);
    };
    const revertOnFailure = () => {
      revertParticipantResponse(previousStatement.id, previousResponseSnapshot);
    };

    const headers = createAuthorizationHeader(userId);

    try {
      if (cachedNextStatement === null) {
        setIsLoading(true);

        const postResult = await axios.post(
          `/api/sessions/${sessionId}/responses`,
          {
            statementId: previousStatement.id,
            value: payload.value,
            responseType: payload.responseType,
            textResponse: payload.textResponse,
          },
          { headers },
        );

        const serverResponse = postResult.data?.response;
        if (serverResponse) {
          syncParticipantResponseFromServer(serverResponse);
        }

        clearPendingStatement();

        enterReflectionMode();
        if (payload.responseType === "free_text") {
          setFreeTextInput("");
        }

        setIsLoading(false);
        return;
      }

      if (cachedNextStatement) {
        setCurrentStatement(cachedNextStatement);
        setPrefetchedStatement(undefined);
        setRemainingQuestions(prefetchedRemainingQuestions ?? null);
        setPrefetchedRemainingQuestions(null);

        // Use prefetched suggestions for the next statement
        if (prefetchedAiSuggestions !== undefined) {
          // Suggestions are ready - mark this as a prefetch transition
          prefetchedStatementIdRef.current = cachedNextStatement.id;
          setAiSuggestions(prefetchedAiSuggestions);
          setIsLoadingSuggestions(false);
          setPrefetchedAiSuggestions(undefined);
        } else {
          // Fallback: suggestions weren't prefetched yet
          // Don't set ref - let useEffect fetch suggestions normally
          setAiSuggestions([]);
          setIsLoadingSuggestions(true);
          setPrefetchedAiSuggestions(undefined);
        }

        axios
          .post(
            `/api/sessions/${sessionId}/responses`,
            {
              statementId: previousStatement.id,
              value: payload.value,
              responseType: payload.responseType,
              textResponse: payload.textResponse,
            },
            { headers },
          )
          .then((res) => {
            const serverResponse = res.data?.response;
            if (serverResponse) {
              syncParticipantResponseFromServer(serverResponse);
            }
          })
          .catch((err) => {
            console.error("Failed to submit answer:", err);
            revertOnFailure();
            if (axios.isAxiosError(err) && err.response?.data?.error) {
              setError(`エラー: ${err.response.data.error}`);
            } else {
              setError("回答の送信に失敗しました。");
            }
          })
          .finally(() => {
            clearPendingStatement();
          });
        if (payload.responseType === "free_text") {
          setFreeTextInput("");
        }
        return;
      }

      setIsLoading(true);

      const [postResponse, nextResponse] = await Promise.all([
        axios.post(
          `/api/sessions/${sessionId}/responses`,
          {
            statementId: previousStatement.id,
            value: payload.value,
            responseType: payload.responseType,
            textResponse: payload.textResponse,
          },
          { headers },
        ),
        axios.get(
          `/api/sessions/${sessionId}/statements/next${buildExcludeQuery([previousStatement.id])}`,
          { headers },
        ),
      ]);

      clearPendingStatement();

      const serverResponse = postResponse.data?.response;
      if (serverResponse) {
        syncParticipantResponseFromServer(serverResponse);
      }

      const nextStatement = nextResponse.data?.statement ?? null;
      const remainingCount = nextResponse.data?.remainingCount ?? null;

      if (nextStatement) {
        setCurrentStatement(nextStatement);
        setRemainingQuestions(remainingCount);
        // Reset suggestions - will be fetched by useEffect
        setAiSuggestions([]);
        setIsLoadingSuggestions(true);
        setPrefetchedAiSuggestions(undefined);
      } else {
        enterReflectionMode();
      }

      setIsLoading(false);
      if (payload.responseType === "free_text") {
        setFreeTextInput("");
      }
    } catch (err) {
      clearPendingStatement();
      console.error("Failed to submit answer:", err);
      revertOnFailure();
      if (
        axios.isAxiosError(err) &&
        err.config?.url?.includes("/statements/next")
      ) {
        setError(
          "次の質問を取得できませんでした。ページを更新して再度お試しください。",
        );
      } else if (axios.isAxiosError(err) && err.response?.data?.error) {
        setError(`エラー: ${err.response.data.error}`);
      } else {
        setError("回答の送信に失敗しました。");
      }
      setIsLoading(false);
    } finally {
      if (payload.responseType === "free_text") {
        setIsSubmittingFreeText(false);
      }
    }
  };

  const handleAnswer = async (value: ResponseValue) => {
    if (value === 0) {
      setShowAlternatives((prev) => !prev);
      return;
    }
    return handleSubmitResponse({ responseType: "scale", value });
  };

  const handleSuggestionClick = async (suggestion: string) => {
    return handleSubmitResponse({
      responseType: "free_text",
      textResponse: suggestion,
    });
  };

  const handleInfoClick = () => {
    setShowAlternatives(true);
    requestAnimationFrame(() => {
      freeTextSectionRef.current?.scrollIntoView({
        behavior: "smooth",
        block: "start",
      });
    });
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

    if (currentResponse.responseType === "free_text") {
      setResponsesError("自由記述で回答した項目はここから変更できません。");
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
    upsertParticipantResponse(stubStatement, {
      responseType: "scale",
      value,
      textResponse: null,
    });
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

  const handleSubmitReflection = async (overrideText?: string) => {
    if (!userId || isSubmittingReflection) return;

    setIsSubmittingReflection(true);
    setReflectionSubmissionError(null);

    const submissionText = overrideText ?? reflectionText;

    try {
      const response = await axios.post(
        `/api/sessions/${sessionId}/reflections`,
        { text: submissionText },
        { headers: createAuthorizationHeader(userId) },
      );

      const reflection = response.data?.reflection as
        | {
            id: string;
            text: string;
            createdAt?: string;
            submittedAt?: string;
          }
        | undefined;

      if (reflection) {
        setParticipantReflections((prev) =>
          [
            {
              id: reflection.id,
              text: reflection.text,
              createdAt: reflection.createdAt ?? new Date().toISOString(),
              submittedAt: reflection.submittedAt ?? new Date().toISOString(),
            },
            ...prev,
          ].sort(
            (a, b) =>
              new Date(b.submittedAt).getTime() -
              new Date(a.submittedAt).getTime(),
          ),
        );
      }

      setReflectionText("");
      setIsLoading(true);

      try {
        const nextStatementResponse = await axios.get(
          `/api/sessions/${sessionId}/statements/next`,
          { headers: createAuthorizationHeader(userId) },
        );
        const statement = nextStatementResponse.data?.statement ?? null;
        const nextRemainingCount =
          nextStatementResponse.data?.remainingCount ?? null;

        if (statement) {
          setCurrentStatement(statement);
          setPrefetchedStatement(undefined);
          setState("ANSWERING");
          setRemainingQuestions(nextRemainingCount);
          // Reset suggestions - will be fetched by useEffect
          setAiSuggestions([]);
          setIsLoadingSuggestions(true);
          setPrefetchedAiSuggestions(undefined);
        } else {
          enterCompletedState();
        }
      } catch (err) {
        console.error("Failed to fetch next statement after reflection:", err);
        setError(
          "次の質問を取得できませんでした。ページを更新して再度お試しください。",
        );
      } finally {
        setIsLoading(false);
      }
    } catch (err) {
      console.error("Failed to submit reflection:", err);
      if (axios.isAxiosError(err) && err.response?.data?.error) {
        setReflectionSubmissionError(
          `ふりかえりの送信に失敗しました: ${err.response.data.error}`,
        );
      } else {
        setReflectionSubmissionError(
          "ふりかえりの送信に失敗しました。時間をおいて再度お試しください。",
        );
      }
    } finally {
      setIsSubmittingReflection(false);
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
        <div className="mb-8 space-y-6">
          <div>
            <h1 className="text-3xl font-bold tracking-tight">
              {sessionInfo?.title ?? "セッション"}
            </h1>
          </div>
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

        {state === "REFLECTION" && (
          <Card>
            <CardHeader>
              <CardTitle>追加の論点・ご意見</CardTitle>
              <CardDescription>
                これまでに取り上げていない話題で、『こんなことについて議論したい』『他の人の意見も聞いてみたい』というテーマや問いがあれば、ぜひ教えてください。今後の質問に反映されます。
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-6">
              <textarea
                value={reflectionText}
                onChange={(event) => setReflectionText(event.target.value)}
                rows={6}
                className="w-full resize-y rounded-lg border border-border bg-background px-3 py-2 text-sm text-foreground placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2"
                placeholder="例）「○○についてもっと掘り下げたい」「まだ○○に関する視点が足りていないと思う」"
                disabled={isSubmittingReflection}
              />
              {reflectionSubmissionError && (
                <p className="text-sm text-destructive">
                  {reflectionSubmissionError}
                </p>
              )}
              <div className="flex justify-end gap-3">
                <Button
                  type="button"
                  variant="outline"
                  disabled={isSubmittingReflection}
                  onClick={() => {
                    void handleSubmitReflection("");
                  }}
                >
                  特にない／次へ進む
                </Button>
                <Button
                  type="button"
                  onClick={() => {
                    void handleSubmitReflection();
                  }}
                  disabled={isSubmittingReflection}
                  isLoading={isSubmittingReflection}
                >
                  提出して次へ
                </Button>
              </div>
            </CardContent>
          </Card>
        )}

        {state === "ANSWERING" && currentStatement && (
          <Card className={isLoading ? "opacity-50 pointer-events-none" : ""}>
            <CardContent className="pt-6">
              <div
                key={currentStatement.id}
                className="mb-8 space-y-2 question-change"
              >
                {typeof remainingQuestions === "number" &&
                  remainingQuestions > 0 && (
                    <p className="text-sm text-muted-foreground">
                      あと{remainingQuestions}個の質問があります
                    </p>
                  )}
                <button
                  type="button"
                  className="inline-flex items-center gap-2 text-sm font-medium text-muted-foreground underline underline-offset-4 hover:text-foreground"
                  onClick={handleInfoClick}
                  aria-label="質問が私たちの前提を把握できていない"
                >
                  <Info className="h-4 w-4" />
                  質問が矛盾している・自分たちの前提を把握できていない場合
                </button>
                <p className="mt-3 text-xl font-medium leading-relaxed">
                  {currentStatement.text}
                </p>
              </div>

              <div className="grid grid-cols-5 gap-2 sm:gap-3">
                <button
                  type="button"
                  onClick={() => handleAnswer(2)}
                  disabled={isLoading}
                  className="group relative flex flex-col items-center gap-1 sm:gap-2 px-1 sm:px-3 py-3 sm:py-5 bg-emerald-500 hover:bg-emerald-600 text-white border-2 border-emerald-600 hover:border-emerald-700 rounded-lg transition-all shadow-sm hover:shadow-md active:scale-[0.98] disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  <div className="text-xl sm:text-3xl">👍</div>
                  <span className="text-[9px] sm:text-xs font-semibold text-center leading-tight">
                    強く同意
                  </span>
                </button>
                <button
                  type="button"
                  onClick={() => handleAnswer(1)}
                  disabled={isLoading}
                  className="group relative flex flex-col items-center gap-1 sm:gap-2 px-1 sm:px-3 py-3 sm:py-5 bg-green-400 hover:bg-green-500 text-white border-2 border-green-500 hover:border-green-600 rounded-lg transition-all shadow-sm hover:shadow-md active:scale-[0.98] disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  <div className="text-xl sm:text-3xl">✓</div>
                  <span className="text-[9px] sm:text-xs font-semibold text-center leading-tight">
                    同意
                  </span>
                </button>
                <button
                  type="button"
                  onClick={() => handleAnswer(0)}
                  disabled={isLoading}
                  className="group relative flex flex-col items-center gap-1 sm:gap-2 px-1 sm:px-3 py-3 sm:py-5 bg-amber-400 hover:bg-amber-500 text-gray-900 border-2 border-amber-500 hover:border-amber-600 rounded-lg transition-all shadow-sm hover:shadow-md active:scale-[0.98] disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  <div className="text-xl sm:text-3xl">🤔</div>
                  <span className="text-[9px] sm:text-xs font-semibold text-center leading-tight">
                    {showAlternatives
                      ? "わからない▲"
                      : "わからない▼"}
                  </span>
                </button>
                <button
                  type="button"
                  onClick={() => handleAnswer(-1)}
                  disabled={isLoading}
                  className="group relative flex flex-col items-center gap-1 sm:gap-2 px-1 sm:px-3 py-3 sm:py-5 bg-rose-400 hover:bg-rose-500 text-white border-2 border-rose-500 hover:border-rose-600 rounded-lg transition-all shadow-sm hover:shadow-md active:scale-[0.98] disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  <div className="text-xl sm:text-3xl">✗</div>
                  <span className="text-[9px] sm:text-xs font-semibold text-center leading-tight">
                    反対
                  </span>
                </button>
                <button
                  type="button"
                  onClick={() => handleAnswer(-2)}
                  disabled={isLoading}
                  className="group relative flex flex-col items-center gap-1 sm:gap-2 px-1 sm:px-3 py-3 sm:py-5 bg-red-600 hover:bg-red-700 text-white border-2 border-red-700 hover:border-red-800 rounded-lg transition-all shadow-sm hover:shadow-md active:scale-[0.98] disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  <div className="text-xl sm:text-3xl">👎</div>
                  <span className="text-[9px] sm:text-xs font-semibold text-center leading-tight">
                    強く反対
                  </span>
                </button>
              </div>

              <div className="mt-6 space-y-3">
                {showAlternatives && (
                  <div className="space-y-4 rounded-lg border border-border/60 bg-muted/30 p-4 animate-in slide-in-from-top-2 duration-200">
                    <div className="space-y-2">
                      <p className="text-sm font-semibold text-foreground">
                        その他の選択肢
                      </p>
                      <button
                        type="button"
                        onClick={() =>
                          handleSubmitResponse({ responseType: "scale", value: 0 })
                        }
                        disabled={isLoading || isSubmittingFreeText}
                        className="w-full px-4 py-3.5 text-left rounded-lg border border-amber-300 bg-white hover:bg-amber-50 hover:border-amber-400 text-sm font-semibold text-amber-700 transition-all duration-200 shadow-sm hover:shadow-md active:scale-[0.98] disabled:opacity-50 disabled:cursor-not-allowed"
                      >
                        （自分はこの質問に対して）確信が持てない・情報を把握していない
                      </button>
                    </div>

                    {isLoadingSuggestions ? (
                      <div className="space-y-2">
                        <div className="h-10 bg-muted rounded-md animate-pulse" />
                        <div className="h-10 bg-muted rounded-md animate-pulse" />
                        <div className="h-10 bg-muted rounded-md animate-pulse" />
                      </div>
                    ) : (
                      <div className="space-y-3">
                        {aiSuggestions.map((suggestion, index) => (
                          <button
                            key={index}
                            type="button"
                            onClick={() => handleSuggestionClick(suggestion)}
                            disabled={isLoading || isSubmittingFreeText}
                            className="w-full px-4 py-3.5 text-left rounded-lg border border-border bg-card hover:bg-accent hover:border-primary/30 text-sm text-foreground transition-all duration-200 shadow-sm hover:shadow-md active:scale-[0.98] disabled:opacity-50 disabled:cursor-not-allowed"
                          >
                            {suggestion}
                          </button>
                        ))}
                      </div>
                    )}

                    <div className="pt-3 border-t border-border/60 space-y-3">
                      <div ref={freeTextSectionRef} />
                      <div>
                        <p className="text-sm font-semibold text-foreground mb-1">
                          自由記述で回答する
                        </p>
                        <p className="text-xs text-muted-foreground">
                          選択肢に当てはまらない場合・質問の前提が間違っている場合はここに意見や補足を書いてください。
                        </p>
                      </div>
                      <textarea
                        value={freeTextInput}
                        onChange={(event) => setFreeTextInput(event.target.value)}
                        rows={4}
                        className="w-full resize-y rounded-md border border-border bg-background px-3 py-2 text-sm text-foreground placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring focus-visible:ring-offset-2"
                        placeholder="この問いに対するあなたの考えや、別の視点からのコメントを自由に書いてください。"
                        disabled={isLoading || isSubmittingFreeText}
                      />
                      <div className="flex justify-end">
                        <Button
                          type="button"
                          variant="outline"
                          onClick={() =>
                            handleSubmitResponse({
                              responseType: "free_text",
                              textResponse: freeTextInput,
                            })
                          }
                          disabled={
                            isLoading ||
                            isSubmittingFreeText ||
                            freeTextInput.trim().length === 0
                          }
                          isLoading={isSubmittingFreeText}
                        >
                          自由記述を送信
                        </Button>
                      </div>
                    </div>
                  </div>
                )}
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
        {(state === "ANSWERING" ||
          state === "REFLECTION" ||
          state === "COMPLETED") && (
          <>
            <Card className="mt-8">
              <CardHeader>
                <CardTitle>質問への回答履歴</CardTitle>
                <CardDescription>
                  あなたの回答とふりかえりの履歴を時系列で確認できます。回答を変更したい場合、質問の回答を再度選択することで変更できます。
                </CardDescription>
              </CardHeader>
              <CardContent>
                {(responsesError || reflectionsError) && (
                  <div className="mb-4 space-y-1 rounded-md border border-destructive/20 bg-destructive/10 p-3">
                    {responsesError && (
                      <p className="text-sm text-destructive">
                        {responsesError}
                      </p>
                    )}
                    {reflectionsError && (
                      <p className="text-sm text-destructive">
                        {reflectionsError}
                      </p>
                    )}
                  </div>
                )}
                {isLoadingResponses || isLoadingReflections ? (
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
                ) : historyItems.length > 0 ? (
                  <div className="max-h-64 space-y-3 overflow-y-auto pr-1">
                    {historyItems.map((item) => {
                      if (item.type === "response") {
                        const response = item.response;
                        const isPending =
                          pendingAnswerStatementIdsRef.current.has(
                            response.statementId,
                          );
                        const isUpdating = updatingResponseIds.has(
                          response.statementId,
                        );

                        if (response.responseType === "free_text") {
                          return (
                            <div
                              key={item.key}
                              className="rounded-lg border border-border/60 bg-muted/20 p-3 shadow-sm"
                            >
                              <div className="flex items-center justify-between gap-3">
                                <p className="text-sm font-medium text-foreground">
                                  {response.statementText}
                                </p>
                              </div>
                              <div className="mt-3 rounded-md border border-border/70 bg-background px-3 py-2">
                                <p className="whitespace-pre-wrap text-sm leading-relaxed text-foreground">
                                  {response.textResponse?.trim().length
                                    ? response.textResponse
                                    : "（記入なし）"}
                                </p>
                              </div>
                            </div>
                          );
                        }

                        return (
                          <div
                            key={item.key}
                            className="rounded-lg border border-border/60 bg-muted/20 p-3 shadow-sm"
                          >
                            <div className="flex items-center justify-between gap-3">
                              <p className="text-sm font-medium text-foreground">
                                {response.statementText}
                              </p>
                            </div>
                            <div className="mt-3 flex flex-wrap gap-2">
                              {RESPONSE_CHOICES.map((choice) => {
                                const isActive =
                                  response.value === choice.value;
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
                      }

                      const { reflection } = item;
                      return (
                        <div
                          key={item.key}
                          className="relative overflow-hidden rounded-lg border border-border/60 bg-muted/20 p-3 shadow-sm"
                        >
                          <div className="flex items-start justify-between gap-3">
                            <span className="inline-flex items-center rounded-full border border-blue-200 bg-blue-50 px-3 py-1 text-xs font-semibold text-blue-600">
                              ふりかえり
                            </span>
                          </div>
                          <div className="mt-3 rounded-md border border-gray-300 bg-gray-50 px-3 py-3 shadow-inner">
                            <p className="whitespace-pre-wrap text-sm leading-relaxed text-gray-800">
                              {reflection.text.trim().length > 0
                                ? reflection.text
                                : "（記入なし）"}
                            </p>
                          </div>
                        </div>
                      );
                    })}
                  </div>
                ) : (
                  <div className="rounded-lg border border-dashed border-border/60 bg-muted/20 py-8 text-center">
                    <p className="text-sm text-muted-foreground">
                      回答やふりかえりを進めると、ここに履歴が表示されます
                    </p>
                  </div>
                )}
              </CardContent>
            </Card>

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
                    {individualReport ? "レポートを更新" : "レポートを生成"}
                  </Button>
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
