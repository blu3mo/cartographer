"use client";

import axios from "axios";
import {
  ChevronDown,
  Copy,
  FileText,
  Loader2,
  MessageCircleQuestion,
} from "lucide-react";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";

import { ReportTasteSelect } from "@/components/report/ReportTasteSelect";
import { ThemeToggle } from "@/components/ThemeToggle";
import {
  AlertDialog,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogHeader,
  AlertDialogTitle,
} from "@/components/ui/alert-dialog";
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
import { Progress } from "@/components/ui/progress";
import { Spinner } from "@/components/ui/spinner";
import { Textarea } from "@/components/ui/textarea";
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

type GoalSection = {
  id: string;
  label: string;
  value: string;
};

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

type ReportTaste = {
  id: string;
  label: string;
  description: string;
  emoji: string;
};

const REPORT_TASTES: ReportTaste[] = [
  {
    id: "neutral",
    label: "標準",
    description: "バランスの取れた客観的な分析",
    emoji: "📊",
  },
  {
    id: "encouraging",
    label: "励まし",
    description: "ポジティブで建設的なフィードバック",
    emoji: "💪",
  },
  {
    id: "analytical",
    label: "分析的",
    description: "詳細な論理的分析と洞察",
    emoji: "🔍",
  },
  {
    id: "casual",
    label: "カジュアル",
    description: "親しみやすくフレンドリーな表現",
    emoji: "😊",
  },
];

const INDIVIDUAL_REPORT_TASTE_OPTIONS = REPORT_TASTES.map((taste) => ({
  value: taste.id,
  label: taste.label,
  icon: <span className="text-base">{taste.emoji}</span>,
}));

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
      "bg-emerald-100 text-emerald-800 border-emerald-200 hover:bg-emerald-200 dark:bg-emerald-950/30 dark:text-emerald-400 dark:border-emerald-800 dark:hover:bg-emerald-950/50",
    activeClass:
      "bg-emerald-500 text-white border-emerald-500 shadow-sm hover:bg-emerald-500 dark:bg-emerald-600 dark:border-emerald-600",
  },
  {
    value: 1,
    label: "同意",
    emoji: "✓",
    idleClass:
      "bg-green-100 text-green-800 border-green-200 hover:bg-green-200 dark:bg-green-950/30 dark:text-green-400 dark:border-green-800 dark:hover:bg-green-950/50",
    activeClass:
      "bg-green-400 text-white border-green-400 shadow-sm hover:bg-green-400 dark:bg-green-500 dark:border-green-500",
  },
  {
    value: 0,
    label: "わからない・自信がない",
    emoji: "🤔",
    idleClass:
      "bg-amber-100 text-amber-800 border-amber-200 hover:bg-amber-200 dark:bg-amber-950/30 dark:text-amber-400 dark:border-amber-800 dark:hover:bg-amber-950/50",
    activeClass:
      "bg-amber-400 text-slate-900 border-amber-400 shadow-sm hover:bg-amber-400 dark:bg-amber-500 dark:text-slate-100 dark:border-amber-500",
  },
  {
    value: -1,
    label: "反対",
    emoji: "✗",
    idleClass:
      "bg-rose-100 text-rose-800 border-rose-200 hover:bg-rose-200 dark:bg-rose-950/30 dark:text-rose-400 dark:border-rose-800 dark:hover:bg-rose-950/50",
    activeClass:
      "bg-rose-400 text-white border-rose-400 shadow-sm hover:bg-rose-400 dark:bg-rose-500 dark:border-rose-500",
  },
  {
    value: -2,
    label: "強く反対",
    emoji: "👎",
    idleClass:
      "bg-red-100 text-red-800 border-red-200 hover:bg-red-200 dark:bg-red-950/30 dark:text-red-400 dark:border-red-800 dark:hover:bg-red-950/50",
    activeClass:
      "bg-red-600 text-white border-red-600 shadow-sm hover:bg-red-600 dark:bg-red-700 dark:border-red-700",
  },
];

const getResponseLabel = (value: ResponseValue | null) => {
  const choice = RESPONSE_CHOICES.find((item) => item.value === value);
  return choice ? `${choice.emoji} ${choice.label}` : "回答済み";
};

// Helper function to fetch suggestions with streaming
async function fetchSuggestionsStreaming(
  sessionId: string,
  statementId: string,
  userId: string,
  onSuggestion: (suggestion: string) => void,
  onComplete: () => void,
  onError: (error: Error) => void,
): Promise<void> {
  try {
    const response = await fetch(
      `/api/sessions/${sessionId}/statements/${statementId}/suggestions`,
      {
        headers: createAuthorizationHeader(userId),
      },
    );

    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }

    const reader = response.body?.getReader();
    if (!reader) {
      throw new Error("No reader available");
    }

    const decoder = new TextDecoder();
    let buffer = "";

    while (true) {
      const { done, value } = await reader.read();
      if (done) break;

      buffer += decoder.decode(value, { stream: true });
      const lines = buffer.split("\n");
      buffer = lines.pop() || "";

      for (const line of lines) {
        if (line.startsWith("data: ")) {
          const data = line.slice(6);
          if (data === "[DONE]") {
            onComplete();
            return;
          }

          try {
            const parsed = JSON.parse(data);
            if (parsed.suggestion) {
              onSuggestion(parsed.suggestion);
            }
          } catch (e) {
            console.warn("Failed to parse streaming data:", e);
          }
        }
      }
    }

    onComplete();
  } catch (error) {
    console.error("Streaming error:", error);
    onError(error as Error);
  }
}

const normalizeGoalLabel = (label: string | null) => {
  if (!label) return null;
  const normalized = label.replace(/\s/g, "");
  if (normalized.includes("何のため") || normalized.includes("目的")) {
    return "purpose" as const;
  }
  if (normalized.includes("何の認識") || normalized.includes("認識")) {
    return "focus" as const;
  }
  return null;
};

const GOAL_LABELS = {
  purpose: "何のために洗い出しますか？",
  focus: "何の認識を洗い出しますか？",
} as const;

export default function SessionPage({ sessionId }: { sessionId: string }) {
  const { userId, isLoading: userLoading } = useUserId();
  const [sessionInfo, setSessionInfo] = useState<SessionInfo | null>(null);
  const [isSessionInfoLoading, setIsSessionInfoLoading] = useState(true);
  const [sessionInfoError, setSessionInfoError] = useState<string | null>(null);
  const [state, setState] = useState<SessionState>("NEEDS_NAME");
  const [hasStarted, setHasStarted] = useState(false);
  const [infoOpen, setInfoOpen] = useState({ goal: true, context: true });
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
  const [allStatements, setAllStatements] = useState<Statement[]>([]);
  const [currentStatementIndex, setCurrentStatementIndex] = useState<
    number | null
  >(null);
  const [isLoading, setIsLoading] = useState(false);
  const [isAutoScrolling, setIsAutoScrolling] = useState(false);
  const [showScrollToActive, setShowScrollToActive] = useState(false);
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
  const [_isLoadingResponses, setIsLoadingResponses] = useState(false);
  const [responsesError, setResponsesError] = useState<string | null>(null);
  const [reflectionText, setReflectionText] = useState("");
  const [isSubmittingReflection, setIsSubmittingReflection] = useState(false);
  const [reflectionSubmissionError, setReflectionSubmissionError] = useState<
    string | null
  >(null);
  const [isTasteModalOpen, setIsTasteModalOpen] = useState(false);
  const [selectedTaste, setSelectedTaste] = useState<string>("neutral");
  const [individualReportCopyStatus, setIndividualReportCopyStatus] = useState<
    "idle" | "copied" | "error"
  >("idle");
  const hasJustCompletedRef = useRef(false);
  const pendingAnswerStatementIdsRef = useRef<Set<string>>(new Set());
  const prefetchedStatementIdRef = useRef<string | null>(null);
  const containerRef = useRef<HTMLDivElement | null>(null);
  const freeTextSectionRef = useRef<HTMLDivElement | null>(null);
  const currentQuestionRef = useRef<HTMLDivElement | null>(null);
  const sessionInfoId = sessionInfo?.id;
  const currentStatementId = currentStatement?.id ?? null;
  const totalQuestions = allStatements.length;
  const progressPercent =
    totalQuestions > 0 && remainingQuestions !== null
      ? Math.min(
          100,
          Math.max(
            0,
            ((totalQuestions - remainingQuestions) / totalQuestions) * 100,
          ),
        )
      : null;
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
  const previewStatement = useMemo(() => {
    const answeredIds = new Set(participantResponses.map((r) => r.statementId));
    if (currentStatementIndex === null || currentStatementIndex < 0) {
      return prefetchedStatement ?? null;
    }

    for (let i = currentStatementIndex + 1; i < allStatements.length; i++) {
      const candidate = allStatements[i];
      if (candidate && !answeredIds.has(candidate.id)) {
        return candidate;
      }
    }

    return prefetchedStatement ?? null;
  }, [
    allStatements,
    participantResponses,
    currentStatementIndex,
    prefetchedStatement,
  ]);

  const responsesByStatementId = useMemo(() => {
    return new Map(
      participantResponses.map((response) => [response.statementId, response]),
    );
  }, [participantResponses]);

  const orderedStatements = useMemo(() => {
    const statementMap = new Map<string, Statement>();
    const addStatement = (statement: Statement | null | undefined) => {
      if (!statement) return;
      statementMap.set(statement.id, statement);
    };

    allStatements.forEach(addStatement);
    participantResponses.forEach((response) => {
      if (!statementMap.has(response.statementId)) {
        statementMap.set(response.statementId, {
          id: response.statementId,
          text: response.statementText,
          orderIndex: response.orderIndex,
          sessionId,
        });
      }
    });
    addStatement(currentStatement);
    addStatement(previewStatement);

    return Array.from(statementMap.values()).sort(
      (a, b) => a.orderIndex - b.orderIndex,
    );
  }, [
    allStatements,
    participantResponses,
    currentStatement,
    previewStatement,
    sessionId,
  ]);

  const activeStatementIndex = useMemo(() => {
    if (!currentStatement) return null;
    const index = orderedStatements.findIndex(
      (statement) => statement.id === currentStatement.id,
    );
    return index >= 0 ? index : null;
  }, [orderedStatements, currentStatement]);
  const currentQuestionNumber = useMemo(() => {
    if (activeStatementIndex !== null) {
      return activeStatementIndex + 1;
    }
    if (currentStatementIndex !== null) {
      return currentStatementIndex + 1;
    }
    return null;
  }, [activeStatementIndex, currentStatementIndex]);
  const shouldShowHeader =
    state === "ANSWERING" &&
    progressPercent !== null &&
    currentQuestionNumber !== null &&
    totalQuestions > 0;

  useEffect(() => {
    if (!currentStatementId) {
      setCurrentStatementIndex(null);
      return;
    }
    if (allStatements.length === 0) return;
    const index = allStatements.findIndex(
      (statement) => statement.id === currentStatementId,
    );
    setCurrentStatementIndex(index !== -1 ? index : null);
  }, [currentStatementId, allStatements]);
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
          createdAt: new Date().toISOString(), // Always update timestamp for sorting
        };

        if (existing) {
          // Update existing response - put it at the beginning (most recent)
          const filtered = prev.filter(
            (item) => item.statementId !== statement.id,
          );
          return [nextResponse, ...filtered];
        }

        // Add new response at the beginning (most recent)
        return [nextResponse, ...prev];
      });
    },
    [], // No dependencies - stable function
  );

  const revertParticipantResponse = useCallback(
    (statementId: string, previous: ParticipantResponse | null) => {
      setParticipantResponses((prev) => {
        if (previous) {
          const exists = prev.some((item) => item.statementId === statementId);
          if (exists) {
            // Replace existing with previous version
            return prev.map((item) =>
              item.statementId === statementId ? previous : item,
            );
          }
          // Add back the previous response
          return [previous, ...prev];
        }
        // Remove the response
        return prev.filter((item) => item.statementId !== statementId);
      });
    },
    [], // No dependencies - stable function
  );

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
      setParticipantResponses((prev) => {
        const existingIndex = prev.findIndex(
          (item) => item.statementId === payload.statementId,
        );

        if (existingIndex === -1) return prev;

        const existing = prev[existingIndex];
        if (!existing) return prev;

        const updated = {
          ...existing,
          id: payload.id,
          value: payload.value as ResponseValue | null,
          responseType: payload.responseType,
          textResponse: payload.textResponse ?? existing.textResponse,
          statementText: payload.statementText ?? existing.statementText ?? "",
          orderIndex: payload.orderIndex ?? existing.orderIndex ?? 0,
          createdAt: payload.createdAt,
        };

        // Move to front if timestamp changed (indicates new update)
        if (payload.createdAt !== existing.createdAt) {
          const filtered = prev.filter((_, i) => i !== existingIndex);
          return [updated, ...filtered];
        }

        // Just update in place if timestamp same
        const newArray = [...prev];
        newArray[existingIndex] = updated;
        return newArray;
      });
    },
    [], // No dependencies - stable function
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
          setRemainingQuestions(response.data.remainingCount ?? null);
          if (response.data.allStatements) {
            setAllStatements(response.data.allStatements);
            const index = response.data.allStatements.findIndex(
              (s: Statement) => s.id === response.data.statement.id,
            );
            setCurrentStatementIndex(index !== -1 ? index : null);
          }
          setSessionInfo((prev) =>
            prev ? { ...prev, isParticipant: true } : prev,
          );
        } else {
          setState("COMPLETED");
          setRemainingQuestions(response.data.remainingCount ?? 0);
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
    const isFromPrefetch =
      prefetchedStatementIdRef.current === currentStatement.id;

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
        const suggestions: string[] = [];

        await fetchSuggestionsStreaming(
          sessionId,
          currentStatement.id,
          userId,
          (suggestion) => {
            suggestions.push(suggestion);
            setAiSuggestions([...suggestions]);
          },
          () => {
            setIsLoadingSuggestions(false);
          },
          (err) => {
            console.error("Failed to fetch AI suggestions:", err);
            // Set fallback suggestions
            setAiSuggestions([
              "状況によって賛成できる",
              "一部には賛成だが全体には反対",
              "今は判断できない",
            ]);
            setIsLoadingSuggestions(false);
          },
        );
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

          // Prefetch suggestions for the next statement with streaming
          const prefetchedSuggestions: string[] = [];

          fetchSuggestionsStreaming(
            sessionId,
            nextStatement.id,
            userId,
            (suggestion) => {
              prefetchedSuggestions.push(suggestion);
              setPrefetchedAiSuggestions([...prefetchedSuggestions]);
            },
            () => {
              // Streaming complete
            },
            (err) => {
              console.error(
                "Failed to prefetch AI suggestions for next statement:",
                err,
              );
              // Set fallback suggestions for next statement
              setPrefetchedAiSuggestions([
                "状況によって賛成できる",
                "一部には賛成だが全体には反対",
                "今は判断できない",
              ]);
            },
          ).catch((err) => {
            console.error("Prefetch streaming error:", err);
          });
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

  // Auto-scroll to current question when it changes
  useEffect(() => {
    if (state !== "ANSWERING" || !currentStatementId) return;
    if (!currentQuestionRef.current) return;

    let finishTimeout: number | null = null;
    const startTimeout = window.setTimeout(() => {
      setIsAutoScrolling(true);
      setShowScrollToActive(false);
      currentQuestionRef.current?.scrollIntoView({
        behavior: "smooth",
        block: "center",
      });

      finishTimeout = window.setTimeout(() => {
        setIsAutoScrolling(false);
      }, 800);
    }, 150);

    return () => {
      window.clearTimeout(startTimeout);
      if (finishTimeout) {
        window.clearTimeout(finishTimeout);
      }
    };
  }, [currentStatementId, state]);

  useEffect(() => {
    if (state !== "ANSWERING") return;
    if (!currentStatementId) return;
    if (!currentQuestionRef.current) return;

    const target = currentQuestionRef.current;
    const observer = new IntersectionObserver(
      ([entry]) => {
        if (!isAutoScrolling) {
          setShowScrollToActive(!entry.isIntersecting);
        }
      },
      {
        root: null,
        threshold: 0.2,
        rootMargin: "0px",
      },
    );

    observer.observe(target);

    return () => {
      observer.disconnect();
    };
  }, [currentStatementId, state, isAutoScrolling]);

  useEffect(() => {
    if (!userId || userLoading) return;
    if (state === "NEEDS_NAME") return;

    fetchParticipantResponses();
  }, [userId, userLoading, state, fetchParticipantResponses]);

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

        // Store all statements for timeline view
        if (response.data.allStatements) {
          setAllStatements(response.data.allStatements);
          // Find current statement index
          const index = response.data.allStatements.findIndex(
            (s: Statement) => s.id === response.data.statement.id,
          );
          setCurrentStatementIndex(index !== -1 ? index : null);
        }
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
      value: payload.responseType === "scale" ? (payload.value ?? null) : null,
      textResponse:
        payload.responseType === "free_text"
          ? (payload.textResponse ?? "")
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
        // Batch all state updates together for performance
        const hasPrefetchedSuggestions = prefetchedAiSuggestions !== undefined;

        if (hasPrefetchedSuggestions) {
          prefetchedStatementIdRef.current = cachedNextStatement.id;
        }

        // Single batched state update
        setCurrentStatement(cachedNextStatement);
        setPrefetchedStatement(undefined);
        setRemainingQuestions(prefetchedRemainingQuestions ?? null);
        setPrefetchedRemainingQuestions(null);
        setAiSuggestions(
          hasPrefetchedSuggestions ? prefetchedAiSuggestions : [],
        );
        setIsLoadingSuggestions(!hasPrefetchedSuggestions);
        setPrefetchedAiSuggestions(undefined);

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
            // Don't show error message for background submission
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
        // Batch all state updates together
        setCurrentStatement(nextStatement);
        setRemainingQuestions(remainingCount);
        setAiSuggestions([]);
        setIsLoadingSuggestions(true);
        setPrefetchedAiSuggestions(undefined);

        // Update all statements list if provided
        if (nextResponse.data?.allStatements) {
          setAllStatements(nextResponse.data.allStatements);
          // Find current statement index
          const index = nextResponse.data.allStatements.findIndex(
            (s: Statement) => s.id === nextStatement.id,
          );
          setCurrentStatementIndex(index !== -1 ? index : null);
        }
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

  const scrollToActive = () => {
    if (!currentQuestionRef.current) return;
    setIsAutoScrolling(true);
    setShowScrollToActive(false);
    currentQuestionRef.current.scrollIntoView({
      behavior: "smooth",
      block: "center",
    });
    window.setTimeout(() => {
      setIsAutoScrolling(false);
    }, 1000);
  };

  const handleSubmitReflection = async (overrideText?: string) => {
    if (!userId || isSubmittingReflection) return;

    setIsSubmittingReflection(true);
    setReflectionSubmissionError(null);

    const submissionText = overrideText ?? reflectionText;

    try {
      await axios.post(
        `/api/sessions/${sessionId}/reflections`,
        { text: submissionText },
        { headers: createAuthorizationHeader(userId) },
      );

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
          // Batch all state updates together
          setCurrentStatement(statement);
          setPrefetchedStatement(undefined);
          setState("ANSWERING");
          setRemainingQuestions(nextRemainingCount);
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

  const handleGenerateReport = async (taste?: string) => {
    if (!userId) return;

    setIsGeneratingReport(true);
    setError(null);

    const tasteToUse = taste || selectedTaste;

    try {
      const response = await axios.post(
        `/api/sessions/${sessionId}/individual-report`,
        { taste: tasteToUse },
        { headers: createAuthorizationHeader(userId) },
      );

      setIndividualReport(response.data.report);
      setIsTasteModalOpen(false);
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

  const handleCopyIndividualReport = async () => {
    if (!individualReport?.contentMarkdown) return;

    try {
      await navigator.clipboard.writeText(individualReport.contentMarkdown);
      setIndividualReportCopyStatus("copied");
      setTimeout(() => setIndividualReportCopyStatus("idle"), 2000);
    } catch (err) {
      console.error("Failed to copy report:", err);
      setIndividualReportCopyStatus("error");
      setTimeout(() => setIndividualReportCopyStatus("idle"), 2000);
    }
  };

  // Update document title when session info is available
  useEffect(() => {
    if (sessionInfo?.title) {
      document.title = `${sessionInfo.title} - セッションに参加 - 倍速会議`;
    }
    return () => {
      document.title = "倍速会議 - 認識を可視化し、合意形成を促進する";
    };
  }, [sessionInfo?.title]);

  const toggleInfo = useCallback((key: "goal" | "context") => {
    setInfoOpen((prev) => ({ ...prev, [key]: !prev[key] }));
  }, []);

  const goalSections = useMemo<GoalSection[]>(() => {
    const goal = sessionInfo?.goal?.trim();
    if (!goal) return [];

    const lines = goal
      .split("\n")
      .map((line) => line.trim())
      .filter(Boolean);
    const buckets: Record<"purpose" | "focus", string[]> = {
      purpose: [],
      focus: [],
    };
    const fallback: string[] = [];

    for (const line of lines) {
      let label: string | null = null;
      let value = line;
      const bracketMatch = line.match(/^【(.+?)】(.*)$/);
      if (bracketMatch) {
        label = bracketMatch[1]?.trim() || null;
        value = bracketMatch[2]?.trim() || "";
      } else {
        const asciiIndex = line.indexOf(":");
        const jpIndex = line.indexOf("：");
        const colonIndex =
          asciiIndex === -1
            ? jpIndex
            : jpIndex === -1
              ? asciiIndex
              : Math.min(asciiIndex, jpIndex);
        if (colonIndex !== -1) {
          label = line.slice(0, colonIndex).trim() || null;
          value = line.slice(colonIndex + 1).trim() || "";
        }
      }

      if (!value) continue;
      const normalized = normalizeGoalLabel(label);
      if (normalized) {
        buckets[normalized].push(value);
      } else {
        fallback.push(label ? `${label} ${value}` : value);
      }
    }

    const sections: GoalSection[] = [];
    if (buckets.purpose.length > 0) {
      sections.push({
        id: "purpose",
        label: GOAL_LABELS.purpose,
        value: buckets.purpose.join("\n"),
      });
    }
    if (buckets.focus.length > 0) {
      sections.push({
        id: "focus",
        label: GOAL_LABELS.focus,
        value: buckets.focus.join("\n"),
      });
    }
    if (sections.length === 0 && fallback.length > 0) {
      sections.push({
        id: "goal",
        label: "目的",
        value: fallback.join("\n"),
      });
    }

    return sections;
  }, [sessionInfo?.goal]);

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
    <div className="min-h-screen bg-background dark:bg-black">
      {/* Sticky Header */}
      {shouldShowHeader && (
        <header className="sticky top-0 left-0 right-0 bg-card/95 dark:bg-card/80 backdrop-blur-md z-50 border-b border-border shadow-sm flex flex-col">
          <div className="h-16 flex items-center justify-center px-6 relative">
            <div className="text-xs font-medium text-muted-foreground">
              {currentQuestionNumber} / {totalQuestions} 問目
            </div>
            <div className="absolute right-6 top-1/2 -translate-y-1/2">
              <ThemeToggle />
            </div>
          </div>
          <Progress value={progressPercent} className="h-1 rounded-none" />
        </header>
      )}

      <div
        className={`max-w-3xl mx-auto px-4 sm:px-6 lg:px-8 ${
          state === "ANSWERING" ? "pt-4 pb-0" : "pt-4 pb-12"
        }`}
      >
        <div className="mb-8 space-y-6">
          {state === "NEEDS_NAME" && !hasStarted && (
            <header className="space-y-4 border-b border-border pb-6">
              <div className="text-center space-y-3">
                <h1 className="scroll-m-20 text-4xl font-extrabold tracking-tight text-balance">
                  {sessionInfo?.title ?? "セッション"}
                </h1>
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
                        infoOpen.goal ? "block" : "hidden",
                      )}
                    >
                      {goalSections.length > 0 ? (
                        <div className="space-y-3">
                          {goalSections.map((section) => (
                            <div key={section.id} className="space-y-1">
                              <p className="text-xs font-semibold text-foreground/80">
                                {section.label}
                              </p>
                              <p className="whitespace-pre-line">
                                {section.value}
                              </p>
                            </div>
                          ))}
                        </div>
                      ) : (
                        "未設定"
                      )}
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
                        infoOpen.context ? "block" : "hidden",
                      )}
                    >
                      {sessionInfo?.context?.trim()
                        ? sessionInfo.context
                        : "未設定"}
                    </div>
                  </div>
                  <div className="border-t border-border/70 bg-card p-4">
                    <Button
                      type="button"
                      className="w-full"
                      onClick={() => setHasStarted(true)}
                    >
                      始める
                    </Button>
                  </div>
                </div>
              </div>
            </header>
          )}
          {state === "NEEDS_NAME" && hasStarted && (
            <div>
              <h1 className="text-3xl font-bold tracking-tight mb-3">
                {sessionInfo?.title ?? "セッション"}
              </h1>
            </div>
          )}
        </div>

        {state === "NEEDS_NAME" && hasStarted && (
          <Card>
            <CardHeader>
              <CardTitle>ようこそ</CardTitle>
              <CardDescription>まず名前を入力してください</CardDescription>
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
              <Textarea
                value={reflectionText}
                onChange={(event) => setReflectionText(event.target.value)}
                rows={6}
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
          <main
            ref={containerRef}
            className="relative w-full max-w-xl mx-auto min-h-screen flex flex-col items-center gap-6 pb-40"
          >
            {responsesError && (
              <div className="w-full rounded-md border border-destructive/20 bg-destructive/10 p-3">
                <p className="text-sm text-destructive">{responsesError}</p>
              </div>
            )}

            {orderedStatements.map((statement, index) => {
              const response = responsesByStatementId.get(statement.id);
              const isActive = statement.id === currentStatement.id;
              const isPast =
                activeStatementIndex !== null && index < activeStatementIndex;
              const isFuture =
                activeStatementIndex !== null && index > activeStatementIndex;
              const distance =
                activeStatementIndex !== null
                  ? index - activeStatementIndex
                  : 0;

              return (
                <div
                  key={statement.id}
                  ref={isActive ? currentQuestionRef : null}
                  data-testid="response-item"
                  className={cn(
                    "w-full transform transition-all duration-500 ease-out",
                    isActive && "scale-100 opacity-100 translate-y-0 z-20",
                    isPast && "scale-[0.96] opacity-60 -translate-y-2 z-10",
                    isFuture && "scale-[0.94] translate-y-4 z-0 blur-[0.5px]",
                  )}
                  style={{
                    opacity: isFuture
                      ? Math.max(0.5, 0.9 - distance * 0.1)
                      : isPast
                        ? 0.6
                        : 1,
                    willChange: isActive ? "transform, opacity" : "auto",
                    transitionTimingFunction: "cubic-bezier(0.4, 0, 0.2, 1)",
                  }}
                >
                  <Card
                    className={cn(
                      "relative overflow-hidden transition-all duration-500 ease-out",
                      isActive &&
                        "shadow-xl ring-4 ring-primary/10 border-primary/50 dark:ring-primary/20 dark:border-primary/60",
                      isActive && isLoading && "opacity-50 pointer-events-none",
                      isFuture &&
                        "bg-muted/30 dark:bg-muted/20 border-dashed pointer-events-none",
                    )}
                    style={{
                      transitionTimingFunction: "cubic-bezier(0.4, 0, 0.2, 1)",
                    }}
                  >
                    <CardHeader className="pb-6">
                      <CardTitle
                        key={isActive ? currentStatement.id : undefined}
                        className={cn(
                          "transition-all duration-300",
                          isActive && "question-change",
                          isPast && "text-muted-foreground",
                          isFuture && "text-muted-foreground/60",
                        )}
                      >
                        {statement.text}
                      </CardTitle>
                    </CardHeader>

                    {isPast && response && (
                      <CardContent className="pt-0 pb-6">
                        <div className="flex items-center gap-3 pt-4 border-t border-border/50 dark:border-border/30">
                          {response.responseType === "free_text" ? (
                            <>
                              <span className="text-xs text-muted-foreground">
                                あなたの回答:
                              </span>
                              <div
                                className="flex-1 text-sm text-foreground line-clamp-2"
                                data-testid="response-value"
                              >
                                {response.textResponse?.trim().length
                                  ? response.textResponse
                                  : "（記入なし）"}
                              </div>
                            </>
                          ) : (
                            <>
                              <span className="text-xs text-muted-foreground">
                                あなたの回答:
                              </span>
                              <div
                                className={cn(
                                  "px-3 py-1 rounded-full text-sm font-bold flex items-center gap-2",
                                  response.value === 2 &&
                                    "bg-emerald-100 text-emerald-700 dark:bg-emerald-950/40 dark:text-emerald-400",
                                  response.value === 1 &&
                                    "bg-green-100 text-green-700 dark:bg-green-950/40 dark:text-green-400",
                                  response.value === 0 &&
                                    "bg-amber-100 text-amber-700 dark:bg-amber-950/40 dark:text-amber-400",
                                  response.value === -1 &&
                                    "bg-rose-100 text-rose-700 dark:bg-rose-950/40 dark:text-rose-400",
                                  response.value === -2 &&
                                    "bg-red-100 text-red-700 dark:bg-red-950/40 dark:text-red-400",
                                )}
                              >
                                <span data-testid="response-value">
                                  {getResponseLabel(response.value)}
                                </span>
                              </div>
                            </>
                          )}
                          <button
                            type="button"
                            onClick={() => {
                              setCurrentStatement(statement);
                              // If the response was free_text, open the alternatives section
                              const isFreeText =
                                response.responseType === "free_text";
                              setShowAlternatives(isFreeText);

                              // Scroll to free text section after a short delay
                              if (isFreeText) {
                                requestAnimationFrame(() => {
                                  setTimeout(() => {
                                    freeTextSectionRef.current?.scrollIntoView({
                                      behavior: "smooth",
                                      block: "start",
                                    });
                                  }, 300);
                                });
                              }
                            }}
                            className="ml-auto text-xs text-primary hover:text-primary/80 font-medium underline"
                          >
                            修正する
                          </button>
                        </div>
                      </CardContent>
                    )}

                    {isActive && (
                      <CardContent className="pt-0 pb-6">
                        <div className="bg-muted/80 dark:bg-muted/30 backdrop-blur-sm border-t border-border dark:border-border/30 -mx-6 px-3 sm:px-4 py-3 sm:py-4">
                          <div className="grid grid-cols-5 gap-2 sm:gap-3">
                            <button
                              type="button"
                              onClick={() => handleAnswer(2)}
                              disabled={isLoading}
                              className={cn(
                                "group relative flex flex-col items-center justify-center gap-1.5 sm:gap-2 px-1 sm:px-3 py-4 sm:py-5 border-2 rounded-lg transition-all shadow-sm hover:shadow-md active:scale-[0.98] disabled:opacity-50 disabled:cursor-not-allowed",
                                response?.value === 2
                                  ? "bg-emerald-500 hover:bg-emerald-600 text-white border-emerald-600 hover:border-emerald-700 dark:bg-emerald-600 dark:hover:bg-emerald-700"
                                  : response &&
                                      response.responseType === "scale"
                                    ? "bg-muted hover:bg-muted/80 text-muted-foreground/50 border-border hover:border-border/80"
                                    : "bg-emerald-500 hover:bg-emerald-600 text-white border-emerald-600 hover:border-emerald-700 dark:bg-emerald-600 dark:hover:bg-emerald-700",
                              )}
                            >
                              <div className="text-xl sm:text-3xl leading-none">
                                👍
                              </div>
                              <span className="text-[9px] sm:text-xs font-semibold text-center leading-tight">
                                強く同意
                              </span>
                            </button>
                            <button
                              type="button"
                              onClick={() => handleAnswer(1)}
                              disabled={isLoading}
                              className={cn(
                                "group relative flex flex-col items-center justify-center gap-1.5 sm:gap-2 px-1 sm:px-3 py-4 sm:py-5 border-2 rounded-lg transition-all shadow-sm hover:shadow-md active:scale-[0.98] disabled:opacity-50 disabled:cursor-not-allowed",
                                response?.value === 1
                                  ? "bg-green-500 hover:bg-green-600 text-white border-green-600 hover:border-green-700 dark:bg-green-500 dark:hover:bg-green-600"
                                  : response &&
                                      response.responseType === "scale"
                                    ? "bg-muted hover:bg-muted/80 text-muted-foreground/50 border-border hover:border-border/80"
                                    : "bg-green-500 hover:bg-green-600 text-white border-green-600 hover:border-green-700 dark:bg-green-500 dark:hover:bg-green-600",
                              )}
                            >
                              <div className="text-xl sm:text-3xl leading-none">
                                ✓
                              </div>
                              <span className="text-[9px] sm:text-xs font-semibold text-center leading-tight">
                                同意
                              </span>
                            </button>
                            <button
                              type="button"
                              onClick={() => handleAnswer(0)}
                              disabled={isLoading}
                              className={cn(
                                "group relative flex flex-col items-center justify-center gap-1.5 sm:gap-2 px-1 sm:px-3 py-4 sm:py-5 border-2 rounded-lg transition-all shadow-sm hover:shadow-md active:scale-[0.98] disabled:opacity-50 disabled:cursor-not-allowed",
                                response?.value === 0
                                  ? "bg-amber-500 hover:bg-amber-600 text-white border-amber-600 hover:border-amber-700 dark:bg-amber-500 dark:hover:bg-amber-600 dark:text-slate-900"
                                  : response &&
                                      response.responseType === "scale"
                                    ? "bg-muted hover:bg-muted/80 text-muted-foreground/50 border-border hover:border-border/80"
                                    : "bg-amber-500 hover:bg-amber-600 text-white border-amber-600 hover:border-amber-700 dark:bg-amber-500 dark:hover:bg-amber-600 dark:text-slate-900",
                              )}
                            >
                              <div className="text-xl sm:text-3xl leading-none">
                                🤔
                              </div>
                              <span className="text-[9px] sm:text-xs font-semibold text-center leading-tight">
                                {showAlternatives
                                  ? "わからない・自信がない▲"
                                  : "わからない・自信がない▼"}
                              </span>
                            </button>
                            <button
                              type="button"
                              onClick={() => handleAnswer(-1)}
                              disabled={isLoading}
                              className={cn(
                                "group relative flex flex-col items-center justify-center gap-1.5 sm:gap-2 px-1 sm:px-3 py-4 sm:py-5 border-2 rounded-lg transition-all shadow-sm hover:shadow-md active:scale-[0.98] disabled:opacity-50 disabled:cursor-not-allowed",
                                response?.value === -1
                                  ? "bg-rose-500 hover:bg-rose-600 text-white border-rose-600 hover:border-rose-700 dark:bg-rose-500 dark:hover:bg-rose-600"
                                  : response &&
                                      response.responseType === "scale"
                                    ? "bg-muted hover:bg-muted/80 text-muted-foreground/50 border-border hover:border-border/80"
                                    : "bg-rose-500 hover:bg-rose-600 text-white border-rose-600 hover:border-rose-700 dark:bg-rose-500 dark:hover:bg-rose-600",
                              )}
                            >
                              <div className="text-xl sm:text-3xl leading-none">
                                ✗
                              </div>
                              <span className="text-[9px] sm:text-xs font-semibold text-center leading-tight">
                                反対
                              </span>
                            </button>
                            <button
                              type="button"
                              onClick={() => handleAnswer(-2)}
                              disabled={isLoading}
                              className={cn(
                                "group relative flex flex-col items-center justify-center gap-1.5 sm:gap-2 px-1 sm:px-3 py-4 sm:py-5 border-2 rounded-lg transition-all shadow-sm hover:shadow-md active:scale-[0.98] disabled:opacity-50 disabled:cursor-not-allowed",
                                response?.value === -2
                                  ? "bg-red-600 hover:bg-red-700 text-white border-red-700 hover:border-red-800 dark:bg-red-700 dark:hover:bg-red-800"
                                  : response &&
                                      response.responseType === "scale"
                                    ? "bg-muted hover:bg-muted/80 text-muted-foreground/50 border-border hover:border-border/80"
                                    : "bg-red-600 hover:bg-red-700 text-white border-red-700 hover:border-red-800 dark:bg-red-700 dark:hover:bg-red-800",
                              )}
                            >
                              <div className="text-xl sm:text-3xl leading-none">
                                👎
                              </div>
                              <span className="text-[9px] sm:text-xs font-semibold text-center leading-tight">
                                強く反対
                              </span>
                            </button>
                          </div>

                          {showAlternatives && (
                            <div className="mt-6 space-y-3">
                              <div className="space-y-4 rounded-lg border border-border/60 bg-muted/30 p-4 animate-in slide-in-from-top-2 duration-200">
                                <div className="space-y-2">
                                  <p className="text-sm font-bold text-foreground">
                                    その他の選択肢
                                  </p>
                                  <button
                                    type="button"
                                    onClick={() =>
                                      handleSubmitResponse({
                                        responseType: "scale",
                                        value: 0,
                                      })
                                    }
                                    disabled={isLoading || isSubmittingFreeText}
                                    className={cn(
                                      "w-full px-4 py-3.5 text-left rounded-lg border text-sm font-semibold transition-all duration-200 shadow-sm hover:shadow-md active:scale-[0.98] disabled:opacity-50 disabled:cursor-not-allowed",
                                      response?.value === 0
                                        ? "border-amber-300 bg-card dark:bg-amber-950/20 hover:bg-amber-50 dark:hover:bg-amber-950/30 hover:border-amber-400 text-amber-700 dark:text-amber-400"
                                        : response &&
                                            response.responseType === "scale"
                                          ? "border-border bg-muted/50 hover:bg-muted hover:border-border/80 text-muted-foreground"
                                          : "border-amber-300 bg-card dark:bg-amber-950/20 hover:bg-amber-50 dark:hover:bg-amber-950/30 hover:border-amber-400 text-amber-700 dark:text-amber-400",
                                    )}
                                  >
                                    （自分はこの質問に対して）確信が持てない・情報を把握していない
                                  </button>
                                </div>

                                {isLoadingSuggestions &&
                                aiSuggestions.length === 0 ? (
                                  <div className="space-y-3">
                                    <div className="mb-3 flex items-center gap-2 text-xs text-muted-foreground">
                                      <Loader2 className="h-3 w-3 animate-spin" />
                                      <span>AIが選択肢を考えています...</span>
                                    </div>
                                    {[1, 2, 3].map((i) => (
                                      <div
                                        key={i}
                                        className="relative h-14 bg-gradient-to-r from-muted via-muted/50 to-muted rounded-lg overflow-hidden"
                                      >
                                        <div className="absolute inset-0 bg-gradient-to-r from-transparent via-white/30 to-transparent animate-shimmer" />
                                        <div className="px-4 py-3.5 opacity-20 text-sm">
                                          選択肢を生成しています...
                                        </div>
                                      </div>
                                    ))}
                                  </div>
                                ) : (
                                  <div className="space-y-3">
                                    {isLoadingSuggestions && (
                                      <div className="flex items-center gap-2 text-xs text-muted-foreground">
                                        <Loader2 className="h-3 w-3 animate-spin" />
                                        <span>残りの選択肢を生成中...</span>
                                      </div>
                                    )}
                                    {aiSuggestions.map((suggestion, index) => (
                                      <button
                                        key={suggestion}
                                        type="button"
                                        onClick={() =>
                                          handleSuggestionClick(suggestion)
                                        }
                                        disabled={
                                          isLoading || isSubmittingFreeText
                                        }
                                        className="w-full px-4 py-3.5 text-left rounded-lg border border-border bg-card hover:bg-accent hover:border-primary/30 text-sm text-foreground transition-all duration-200 shadow-sm hover:shadow-md active:scale-[0.98] disabled:opacity-50 disabled:cursor-not-allowed animate-fade-in"
                                        style={{
                                          animationDelay: `${index * 100}ms`,
                                        }}
                                      >
                                        {suggestion}
                                      </button>
                                    ))}
                                  </div>
                                )}

                                <div className="pt-3 border-t border-border/60 space-y-3">
                                  <div ref={freeTextSectionRef} />
                                  <div>
                                    <p className="text-sm font-bold text-foreground mb-1">
                                      自由記述で回答する
                                    </p>
                                    <p className="text-xs text-muted-foreground">
                                      選択肢に当てはまらない場合・質問の前提が間違っている場合はここに意見や補足を書いてください。
                                    </p>
                                  </div>
                                  <Textarea
                                    value={freeTextInput}
                                    onChange={(event) =>
                                      setFreeTextInput(event.target.value)
                                    }
                                    rows={4}
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
                            </div>
                          )}

                          {error && (
                            <p className="text-sm text-destructive mt-4">
                              {error}
                            </p>
                          )}
                        </div>
                      </CardContent>
                    )}
                  </Card>
                </div>
              );
            })}

            <div className="h-40 flex items-start justify-center pt-4">
              <div className="text-center">
                <p className="text-sm text-muted-foreground">質問は以上です</p>
              </div>
            </div>
          </main>
        )}

        {state === "ANSWERING" && (
          <div
            className={cn(
              "fixed bottom-6 right-6 z-50 transition-all duration-300",
              showScrollToActive
                ? "translate-y-0 opacity-100"
                : "translate-y-6 opacity-0 pointer-events-none",
            )}
          >
            <Button
              type="button"
              onClick={scrollToActive}
              className="rounded-full px-5 py-2.5 shadow-lg"
            >
              最新の質問へ戻る
              <MessageCircleQuestion className="h-4 w-4" />
            </Button>
          </div>
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

        {/* Individual Report - Show after finishing all questions */}
        {(state === "REFLECTION" || state === "COMPLETED") && (
          <>
            <Card className="mt-8">
              <CardHeader>
                <div className="flex flex-col gap-3">
                  <div className="flex items-center justify-between">
                    <CardTitle className="scroll-m-20 text-2xl font-semibold tracking-tight">
                      じぶんレポート
                    </CardTitle>
                  </div>
                  <CardDescription className="text-muted-foreground">
                    あなたの回答から生成された個別分析レポート
                  </CardDescription>
                  <div className="flex items-end gap-3 pt-2">
                    <div className="flex-1">
                      <ReportTasteSelect
                        value={selectedTaste}
                        options={INDIVIDUAL_REPORT_TASTE_OPTIONS}
                        onClick={() => setIsTasteModalOpen(true)}
                        disabled={isGeneratingReport}
                        iconWrapperClassName="h-5 w-5 text-base"
                      />
                    </div>
                    <Button
                      onClick={() => setIsTasteModalOpen(true)}
                      disabled={isGeneratingReport}
                      size="default"
                      className="shrink-0"
                    >
                      {individualReport ? "レポートを更新" : "レポートを生成"}
                    </Button>
                  </div>
                </div>
              </CardHeader>
              <CardContent>
                {error && (
                  <div className="mb-4 rounded-md border border-destructive/20 bg-destructive/10 p-3">
                    <p className="text-sm text-destructive">{error}</p>
                  </div>
                )}
                {isGeneratingReport && (
                  <div className="mb-6 flex flex-col items-center justify-center space-y-4 border-b pb-6 pt-8">
                    <Spinner className="h-10 w-10 text-primary" />
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
                  <div className="space-y-4">
                    <div className="flex justify-end">
                      <Button
                        type="button"
                        variant="outline"
                        size="sm"
                        onClick={handleCopyIndividualReport}
                        className="gap-1.5 text-xs"
                      >
                        <Copy className="h-3.5 w-3.5" />
                        {individualReportCopyStatus === "copied"
                          ? "コピー済み"
                          : individualReportCopyStatus === "error"
                            ? "コピー失敗"
                            : "Markdownをコピー"}
                      </Button>
                    </div>
                    <div
                      className={cn(
                        "markdown-body prose prose-slate dark:prose-invert max-w-none",
                        "prose-headings:scroll-m-20 prose-headings:tracking-tight",
                        "prose-h1:text-4xl prose-h1:font-extrabold prose-h1:text-balance",
                        "prose-h2:border-b prose-h2:pb-2 prose-h2:text-3xl prose-h2:font-semibold prose-h2:first:mt-0",
                        "prose-h3:text-2xl prose-h3:font-semibold",
                        "prose-h4:text-xl prose-h4:font-semibold",
                        "prose-p:leading-7 prose-p:[&:not(:first-child)]:mt-6",
                        "prose-blockquote:mt-6 prose-blockquote:border-l-2 prose-blockquote:pl-6 prose-blockquote:italic",
                        "prose-code:relative prose-code:rounded prose-code:bg-muted prose-code:px-[0.3rem] prose-code:py-[0.2rem] prose-code:font-mono prose-code:text-sm prose-code:font-semibold",
                        "prose-lead:text-xl prose-lead:text-muted-foreground",
                        isGeneratingReport && "opacity-60",
                      )}
                    >
                      <ReactMarkdown remarkPlugins={[remarkGfm]}>
                        {individualReport.contentMarkdown}
                      </ReactMarkdown>
                    </div>
                  </div>
                ) : !isGeneratingReport ? (
                  <div className="py-8 text-center">
                    <div className="mb-3 mx-auto flex h-12 w-12 items-center justify-center rounded-full bg-muted dark:bg-muted/50">
                      <FileText className="h-6 w-6 text-muted-foreground" />
                    </div>
                    <p className="text-sm text-muted-foreground">
                      回答を進めると、あなた専用の分析レポートがここに表示されます
                    </p>
                  </div>
                ) : null}
              </CardContent>
            </Card>

            {/* Taste Selection Modal */}
            <AlertDialog
              open={isTasteModalOpen}
              onOpenChange={setIsTasteModalOpen}
            >
              <AlertDialogContent className="max-w-2xl">
                <AlertDialogHeader>
                  <AlertDialogTitle className="text-2xl">
                    レポートのテイストを選択
                  </AlertDialogTitle>
                  <AlertDialogDescription>
                    レポートの文体やトーンを選択してください
                  </AlertDialogDescription>
                </AlertDialogHeader>
                <div className="grid gap-3 py-4">
                  {REPORT_TASTES.map((taste) => (
                    <button
                      key={taste.id}
                      type="button"
                      onClick={() => {
                        setSelectedTaste(taste.id);
                      }}
                      disabled={isGeneratingReport}
                      className={cn(
                        "group relative flex items-start gap-4 rounded-lg border-2 p-4 text-left transition-colors shadow-sm hover:border-primary/40 hover:bg-muted/60 disabled:opacity-50 disabled:cursor-not-allowed",
                        selectedTaste === taste.id
                          ? "border-primary/70 bg-primary/10 shadow-md"
                          : "border-border/60 bg-muted/40",
                      )}
                    >
                      <div className="text-3xl">{taste.emoji}</div>
                      <div className="flex-1 space-y-1">
                        <div className="flex items-center gap-2">
                          <p className="font-semibold text-foreground">
                            {taste.label}
                          </p>
                          {selectedTaste === taste.id && (
                            <span className="rounded-full bg-primary px-2 py-0.5 text-xs text-primary-foreground">
                              選択中
                            </span>
                          )}
                        </div>
                        <p className="text-sm text-muted-foreground">
                          {taste.description}
                        </p>
                      </div>
                    </button>
                  ))}
                </div>
                <div className="flex justify-end gap-2 border-t pt-4">
                  <Button
                    variant="outline"
                    onClick={() => setIsTasteModalOpen(false)}
                    disabled={isGeneratingReport}
                  >
                    キャンセル
                  </Button>
                  <Button
                    onClick={() => handleGenerateReport()}
                    disabled={isGeneratingReport}
                    isLoading={isGeneratingReport}
                  >
                    このテイストで生成
                  </Button>
                </div>
              </AlertDialogContent>
            </AlertDialog>
          </>
        )}
      </div>
    </div>
  );
}
