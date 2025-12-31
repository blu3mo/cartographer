"use client";

import axios from "axios";
import {
  ArrowUpRight,
  Loader2,
  MessageSquare,
  Pencil,
  Sparkles,
} from "lucide-react";
import { useRouter, useSearchParams } from "next/navigation";
import { Suspense, useCallback, useEffect, useRef, useState } from "react";

import { Button } from "@/components/ui/Button";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import {
  Field,
  FieldDescription,
  FieldError,
  FieldLabel,
} from "@/components/ui/field";
import { Input } from "@/components/ui/input";
import { Skeleton } from "@/components/ui/Skeleton";
import { Textarea } from "@/components/ui/textarea";
import { createAuthorizationHeader } from "@/lib/auth";
import { useUserId } from "@/lib/useUserId";

type SuggestionField = "backgroundInfo" | "purpose";

type Suggestion = {
  field: SuggestionField;
  message: string;
};

const FIELD_META: Record<
  SuggestionField,
  { label: string; elementId: string }
> = {
  backgroundInfo: { label: "背景情報", elementId: "backgroundInfo" },
  purpose: {
    label: "目的",
    elementId: "purpose",
  },
};

const inferFieldFromMessage = (message: string): SuggestionField => {
  const lower = message.toLowerCase();
  const includes = (keywords: string[]) =>
    keywords.some((keyword) => lower.includes(keyword));

  const backgroundKeywords = [
    "背景",
    "人数",
    "関係性",
    "役割",
    "状況",
    "経緯",
    "チーム",
    "メンバー",
    "コミュニケーション",
    "誰が",
  ];

  if (includes(backgroundKeywords)) {
    return "backgroundInfo";
  }

  return "purpose";
};

const renderSuggestionCard = (
  field: SuggestionField,
  suggestions: Suggestion[],
  _onClick: (field: SuggestionField) => void,
) => {
  const fieldSuggestions = suggestions.filter(
    (suggestion) => suggestion.field === field,
  );
  if (fieldSuggestions.length === 0) return null;

  return (
    <Card className="border-indigo-100 bg-indigo-50/40 dark:border-indigo-900/60 dark:bg-indigo-950/50">
      <CardContent className="pt-6">
        <div className="flex items-start gap-3">
          <div className="flex-1 space-y-3">
            <span className="inline-flex items-center gap-1 rounded-full bg-indigo-100 px-2.5 py-0.5 text-xs font-medium text-indigo-800 dark:bg-indigo-900/60 dark:text-indigo-100">
              <Sparkles className="h-3 w-3" />
              AI入力アシスト
            </span>
            {fieldSuggestions.length === 1 ? (
              <p className="text-sm leading-relaxed text-indigo-900 dark:text-indigo-100">
                {fieldSuggestions[0].message}
              </p>
            ) : (
              <ul className="list-disc list-inside space-y-2">
                {fieldSuggestions.map((suggestion, index) => (
                  <li
                    key={`${suggestion.field}-${index}`}
                    className="text-sm leading-relaxed text-indigo-900 dark:text-indigo-100"
                  >
                    {suggestion.message}
                  </li>
                ))}
              </ul>
            )}
          </div>
        </div>
      </CardContent>
    </Card>
  );
};

const buildGoalFromInputs = (purpose: string) => purpose;

function NewSessionContent() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const { userId, isLoading: userLoading } = useUserId();
  const [title, setTitle] = useState(searchParams.get("title") || "");
  const [backgroundInfo, setBackgroundInfo] = useState(
    searchParams.get("background") || "",
  );
  const [purpose, setPurpose] = useState(
    searchParams.get("purpose") || searchParams.get("topic") || "",
  );
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [titleError, setTitleError] = useState<string | null>(null);
  const [purposeError, setPurposeError] = useState<string | null>(null);
  const [suggestions, setSuggestions] = useState<Suggestion[]>([]);
  const [isSuggestionsLoading, setIsSuggestionsLoading] = useState(false);
  const [highlightedField, setHighlightedField] =
    useState<SuggestionField | null>(null);
  const lastFormStateRef = useRef<string>("");
  const highlightTimeoutRef = useRef<NodeJS.Timeout | null>(null);
  const suggestionAbortRef = useRef<AbortController | null>(null);
  const previewAbortRef = useRef<AbortController | null>(null);
  const lastPreviewStateRef = useRef<string>("");
  const [previewQuestions, setPreviewQuestions] = useState<string[]>([]);
  const [isPreviewLoading, setIsPreviewLoading] = useState(false);
  const [previewError, setPreviewError] = useState<string | null>(null);
  const [expandedQuestionIndex, setExpandedQuestionIndex] = useState<
    number | null
  >(null);
  const [questionFeedback, setQuestionFeedback] = useState<string>("");

  useEffect(() => {
    document.title = "新しいセッションを作成 - 倍速会議";
    return () => {
      document.title = "倍速会議 - 認識を可視化し、合意形成を促進する";
    };
  }, []);

  useEffect(() => {
    return () => {
      if (highlightTimeoutRef.current) {
        clearTimeout(highlightTimeoutRef.current);
      }
    };
  }, []);

  const fetchSuggestions = useCallback(async () => {
    const currentFormState = JSON.stringify({
      backgroundInfo,
      purpose,
    });

    if (currentFormState === lastFormStateRef.current) {
      return;
    }

    lastFormStateRef.current = currentFormState;

    if (!backgroundInfo.trim() && !purpose.trim()) {
      setSuggestions([]);
      setIsSuggestionsLoading(false);
      return;
    }

    try {
      if (suggestionAbortRef.current) {
        suggestionAbortRef.current.abort();
      }
      const controller = new AbortController();
      suggestionAbortRef.current = controller;

      setIsSuggestionsLoading(true);

      const response = await axios.post<{
        suggestions: Array<Suggestion | string>;
      }>(
        "/api/sessions/form-suggestions",
        {
          backgroundInfo,
          purpose,
        },
        { signal: controller.signal },
      );

      const normalizedSuggestions: Suggestion[] = (
        response.data.suggestions || []
      ).map((item) => {
        if (typeof item === "string") {
          return {
            field: inferFieldFromMessage(item),
            message: item,
          };
        }
        return {
          ...item,
          field: item.field ?? inferFieldFromMessage(item.message),
        };
      });

      setSuggestions(normalizedSuggestions);
      setIsSuggestionsLoading(false);
    } catch (err) {
      if ((err as { name?: string }).name === "CanceledError") {
        return;
      }
      console.error("Failed to fetch suggestions:", err);
      setIsSuggestionsLoading(false);
    }
  }, [backgroundInfo, purpose]);

  const fetchPreviewQuestions = useCallback(async () => {
    if (!userId) return;

    const currentPreviewState = JSON.stringify({
      title,
      purpose,
      backgroundInfo,
    });

    if (currentPreviewState === lastPreviewStateRef.current) {
      return;
    }

    lastPreviewStateRef.current = currentPreviewState;

    if (!purpose.trim()) {
      setPreviewQuestions([]);
      setPreviewError(null);
      return;
    }

    try {
      if (previewAbortRef.current) {
        previewAbortRef.current.abort();
      }
      const controller = new AbortController();
      previewAbortRef.current = controller;

      setIsPreviewLoading(true);
      setPreviewError(null);

      const goal = buildGoalFromInputs(purpose);

      const response = await axios.post(
        "/api/sessions/preview-questions",
        {
          title: title.trim(),
          goal,
          context: backgroundInfo.trim(),
        },
        {
          headers: createAuthorizationHeader(userId),
          signal: controller.signal,
        },
      );
      setPreviewQuestions(response.data.questions);
    } catch (err) {
      if ((err as { name?: string }).name === "CanceledError") {
        return;
      }
      console.error("Failed to generate preview:", err);
      setPreviewQuestions([]);
      setPreviewError("質問の生成に失敗しました。もう一度お試しください。");
    } finally {
      setIsPreviewLoading(false);
    }
  }, [backgroundInfo, purpose, title, userId]);

  useEffect(() => {
    const debounceHandle = setTimeout(() => {
      void fetchSuggestions();
      void fetchPreviewQuestions();
    }, 100);

    return () => {
      clearTimeout(debounceHandle);
      if (suggestionAbortRef.current) {
        suggestionAbortRef.current.abort();
      }
      if (previewAbortRef.current) {
        previewAbortRef.current.abort();
      }
    };
  }, [fetchSuggestions, fetchPreviewQuestions]);

  const handleSuggestionClick = (field: SuggestionField) => {
    const fieldMeta = FIELD_META[field];
    if (!fieldMeta) return;

    const target = document.getElementById(fieldMeta.elementId);
    if (
      target instanceof HTMLTextAreaElement ||
      target instanceof HTMLInputElement
    ) {
      target.focus();
    }

    if (highlightTimeoutRef.current) {
      clearTimeout(highlightTimeoutRef.current);
    }
    setHighlightedField(field);
    highlightTimeoutRef.current = setTimeout(() => {
      setHighlightedField(null);
    }, 2500);
  };

  const textareaClasses = (field: SuggestionField, hasError?: boolean) =>
    [
      "flex w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50 resize-y",
      highlightedField === field
        ? "ring-2 ring-indigo-400 border-indigo-400"
        : "",
      hasError ? "border-red-500 focus-visible:ring-red-500" : "",
    ]
      .filter(Boolean)
      .join(" ");

  const renderFieldAid = (field: SuggestionField, fallback: string) => {
    const hasSuggestions = suggestions.some(
      (suggestion) => suggestion.field === field,
    );
    const hasAnyInput = Boolean(backgroundInfo.trim() || purpose.trim());

    if (isSuggestionsLoading && hasAnyInput && !hasSuggestions) {
      return (
        <Card className="border-indigo-100 bg-indigo-50/40 dark:border-indigo-900/60 dark:bg-indigo-950/50">
          <CardContent className="pt-6">
            <div className="flex items-start gap-3">
              <div className="flex-1 space-y-3">
                <span className="inline-flex items-center gap-1 rounded-full bg-indigo-100 px-2.5 py-0.5 text-xs font-medium text-indigo-800 dark:bg-indigo-900/60 dark:text-indigo-100">
                  <Sparkles className="h-3 w-3" />
                  AI入力アシスト
                </span>
                <Skeleton className="h-4 w-full" />
                <Skeleton className="h-4 w-3/4" />
              </div>
            </div>
          </CardContent>
        </Card>
      );
    }

    if (!hasAnyInput || !hasSuggestions) {
      return <FieldDescription>{fallback}</FieldDescription>;
    }
    return renderSuggestionCard(field, suggestions, handleSuggestionClick);
  };

  const handleQuestionClick = (index: number) => {
    if (expandedQuestionIndex === index) {
      setExpandedQuestionIndex(null);
      setQuestionFeedback("");
    } else {
      setExpandedQuestionIndex(index);
      setQuestionFeedback("");
    }
  };

  const handleFeedbackSubmit = (question: string, index: number) => {
    if (!questionFeedback.trim()) return;

    const feedbackText = `[質問「${question}」へのフィードバック]\n${questionFeedback.trim()}\n\n`;
    setBackgroundInfo((prev) => prev + feedbackText);
    setQuestionFeedback("");
    setExpandedQuestionIndex(null);
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    if (!userId) {
      setError("User ID not available");
      return;
    }

    // バリデーション
    setTitleError(null);
    setPurposeError(null);
    setError(null);

    let hasError = false;
    let firstErrorField: string | null = null;

    if (!title.trim()) {
      setTitleError("セッションのタイトルを入力してください");
      hasError = true;
      if (!firstErrorField) firstErrorField = "title";
    }

    if (!purpose.trim()) {
      setPurposeError("倍速会議を使う目的を入力してください");
      hasError = true;
      if (!firstErrorField) firstErrorField = "purpose";
    }

    if (hasError && firstErrorField) {
      // エラーのあるフィールドまでスクロール
      setTimeout(() => {
        const element = document.getElementById(firstErrorField);
        if (element) {
          element.scrollIntoView({ behavior: "smooth", block: "center" });
          element.focus();
        }
      }, 100);
      return;
    }

    setIsSubmitting(true);
    const goal = buildGoalFromInputs(purpose);

    const payload: {
      title: string;
      context: string;
      goal: string;
      initialQuestions?: string[];
    } = {
      title: title.trim(),
      context: backgroundInfo.trim(),
      goal,
    };

    if (previewQuestions.length > 0) {
      payload.initialQuestions = previewQuestions;
    }

    try {
      const response = await axios.post("/api/sessions", payload, {
        headers: createAuthorizationHeader(userId),
      });

      const sessionId = response.data.session.id;
      const adminAccessToken = response.data.session.adminAccessToken;
      router.push(`/sessions/${sessionId}/${adminAccessToken}`);
    } catch (err) {
      console.error("Failed to create session:", err);
      setError("セッションの作成に失敗しました。もう一度お試しください。");
      setIsSubmitting(false);
      return;
    }

    setIsSubmitting(false);
  };

  if (userLoading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
      </div>
    );
  }

  const hasPreviewQuestions = previewQuestions.length > 0;

  return (
    <div className="min-h-screen bg-background">
      <div className="max-w-7xl mx-auto px-4 py-12 sm:px-6 lg:px-8">
        <div className="mb-8">
          <h1 className="text-3xl font-bold tracking-tight mb-2">
            新しいセッションを作成
          </h1>
          <p className="text-muted-foreground">
            なるべくたくさんの情報量があると、AIが生成する質問の質が上がります。社内チャットや、ドキュメントのコピペでも構いません。
          </p>
          <p className="text-muted-foreground mt-1">
            <a
              href="https://scrapbox.io/baisoku-kaigi/%E3%80%8C%E8%A3%9C%E8%B6%B3%E6%83%85%E5%A0%B1%E3%80%8D%E3%82%92%E3%81%9F%E3%81%8F%E3%81%95%E3%82%93%E6%9B%B8%E3%81%8F%E3%81%9F%E3%82%81%E3%81%AB%E9%9F%B3%E5%A3%B0%E5%85%A5%E5%8A%9B%E3%82%92%E6%B4%BB%E7%94%A8%E3%81%99%E3%82%8B"
              target="_blank"
              rel="noreferrer"
              className="inline-flex items-center gap-1 underline underline-offset-2"
            >
              背景情報の入力には、AI音声入力がおすすめです
              <ArrowUpRight className="h-3 w-3" aria-hidden="true" />
            </a>
          </p>
        </div>

        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          <div className="order-2 lg:order-1">
            {!previewError && isPreviewLoading && !hasPreviewQuestions && (
              <Card className="border-indigo-100 bg-indigo-50/40 dark:border-indigo-900/60 dark:bg-indigo-950/50 min-h-[600px] flex flex-col">
                <CardHeader className="pb-3">
                  <CardTitle className="text-base text-indigo-900 dark:text-indigo-100 flex items-center gap-2">
                    <Sparkles className="h-4 w-4" />
                    質問のプレビュー
                  </CardTitle>
                  <CardDescription>AIが質問を生成しています...</CardDescription>
                </CardHeader>
                <CardContent className="flex-1 overflow-hidden flex flex-col">
                  <div className="flex-1 space-y-3 overflow-y-auto pr-1">
                    {[1, 2, 3, 4, 5].map((i) => (
                      <div
                        key={i}
                        className="rounded-lg border border-border/60 bg-white dark:bg-slate-950/40 shadow-sm px-4 py-3"
                      >
                        <Skeleton className="h-4 w-full mb-2" />
                        <Skeleton className="h-4 w-3/4" />
                      </div>
                    ))}
                  </div>
                </CardContent>
              </Card>
            )}

            {previewError && (
              <Card className="border-destructive min-h-[600px] flex items-center justify-center">
                <CardContent className="pt-6">
                  <p className="text-sm text-destructive" role="alert">
                    {previewError}
                  </p>
                </CardContent>
              </Card>
            )}

            {!previewError && hasPreviewQuestions && (
              <Card className="border-indigo-100 bg-indigo-50/40 dark:border-indigo-900/60 dark:bg-indigo-950/50 min-h-[600px] flex flex-col">
                <CardHeader className="pb-3 relative">
                  <CardTitle className="text-base text-indigo-900 dark:text-indigo-100 flex items-center gap-2">
                    <Sparkles className="h-4 w-4" />
                    質問のプレビュー
                  </CardTitle>
                  <CardDescription>
                    参加者が回答する質問のプレビューです。質問をクリックすると、補足情報や修正点をフィードバックできます。
                  </CardDescription>
                  <div
                    className={`absolute right-6 top-6 flex items-center gap-2 text-xs text-muted-foreground transition-opacity ${
                      isPreviewLoading ? "opacity-100" : "opacity-0"
                    }`}
                    aria-live="polite"
                  >
                    <Loader2 className="h-3 w-3 animate-spin" />
                    <span>更新中...</span>
                  </div>
                </CardHeader>
                <CardContent className="flex-1 overflow-hidden flex flex-col">
                  <div className="flex-1 space-y-3 overflow-y-auto pr-1">
                    {previewQuestions.map((q, index) => {
                      const feedbackId = `question-feedback-${index}`;

                      return (
                        <div
                          key={`${q}-${index}`}
                          className="rounded-lg border border-border/60 bg-white dark:bg-slate-950/40 shadow-sm"
                        >
                          <button
                            type="button"
                            className="group flex w-full items-start gap-3 px-4 py-3 text-left transition-colors hover:bg-slate-50 dark:hover:bg-slate-900/50"
                            onClick={() => handleQuestionClick(index)}
                          >
                            <span className="text-sm leading-relaxed text-slate-900 dark:text-slate-100 flex-1">
                              {q}
                            </span>
                            <span
                              className={`flex items-center gap-1 rounded-md px-2 py-1 transition-all ${
                                expandedQuestionIndex === index
                                  ? "bg-indigo-100 text-indigo-700 dark:bg-indigo-900/70 dark:text-indigo-200"
                                  : "bg-muted text-slate-600 group-hover:bg-slate-200 dark:bg-slate-800/70 dark:text-slate-300 dark:group-hover:bg-slate-700/70"
                              }`}
                            >
                              <Pencil className="h-3 w-3" />
                              <span className="text-xs font-medium">補足</span>
                            </span>
                          </button>
                          {expandedQuestionIndex === index && (
                            <div className="border-t border-border/60 px-4 py-3 space-y-2">
                              <label
                                htmlFor={feedbackId}
                                className="text-xs text-muted-foreground"
                              >
                                この質問についての補足や修正点
                              </label>
                              <Textarea
                                id={feedbackId}
                                value={questionFeedback}
                                onChange={(e) =>
                                  setQuestionFeedback(e.target.value)
                                }
                                rows={3}
                                placeholder="例: この質問は前提が違います。実際には..."
                                onClick={(e) => e.stopPropagation()}
                              />
                              <div className="flex justify-end gap-2">
                                <Button
                                  type="button"
                                  variant="outline"
                                  size="sm"
                                  onClick={(e) => {
                                    e.stopPropagation();
                                    setExpandedQuestionIndex(null);
                                    setQuestionFeedback("");
                                  }}
                                >
                                  キャンセル
                                </Button>
                                <Button
                                  type="button"
                                  size="sm"
                                  onClick={(e) => {
                                    e.stopPropagation();
                                    handleFeedbackSubmit(q, index);
                                  }}
                                  disabled={!questionFeedback.trim()}
                                >
                                  追加
                                </Button>
                              </div>
                            </div>
                          )}
                        </div>
                      );
                    })}
                  </div>
                  <div className="mt-6 flex justify-end">
                    <Button
                      type="submit"
                      disabled={isSubmitting}
                      isLoading={isSubmitting}
                      onClick={handleSubmit}
                      className="w-full sm:w-auto"
                    >
                      セッションを作成
                    </Button>
                  </div>
                </CardContent>
              </Card>
            )}

            {!isPreviewLoading && !previewError && !hasPreviewQuestions && (
              <Card className="border-muted min-h-[600px] flex items-center justify-center">
                <CardContent className="pt-6">
                  <p className="text-sm text-muted-foreground text-center">
                    目的を入力すると、質問のプレビューが表示されます
                  </p>
                </CardContent>
              </Card>
            )}
          </div>

          <div className="order-1 lg:order-2 lg:sticky lg:top-6 lg:self-start">
            <Card className="min-h-[600px]">
              <CardContent className="pt-6">
                <form onSubmit={handleSubmit} className="space-y-6">
                  <Field data-invalid={!!titleError}>
                    <FieldLabel
                      htmlFor="title"
                      className="text-base font-semibold"
                    >
                      セッションのタイトル{" "}
                      <span className="text-red-500">*</span>
                    </FieldLabel>
                    <Input
                      type="text"
                      id="title"
                      value={title}
                      onChange={(e) => {
                        setTitle(e.target.value);
                        if (titleError) setTitleError(null);
                      }}
                      placeholder="例: 社内チャットツールの入れ替えに関する各メンバーの現状認識のすり合わせ"
                      aria-invalid={!!titleError}
                      className={
                        titleError
                          ? "border-red-500 focus-visible:ring-red-500"
                          : ""
                      }
                    />
                    {titleError ? (
                      <FieldError>{titleError}</FieldError>
                    ) : (
                      <FieldDescription>
                        それぞれの参加者が、何のために回答を収集しているのか分かりやすいタイトルをつけましょう
                      </FieldDescription>
                    )}
                  </Field>

                  <Field data-invalid={!!purposeError}>
                    <FieldLabel
                      htmlFor="purpose"
                      className="text-base font-semibold"
                    >
                      何をするために倍速会議を使うのですか？{" "}
                      <span className="text-red-500">*</span>
                    </FieldLabel>
                    <Textarea
                      id="purpose"
                      value={purpose}
                      onChange={(e) => {
                        setPurpose(e.target.value);
                        if (purposeError) setPurposeError(null);
                      }}
                      rows={6}
                      className={textareaClasses("purpose", !!purposeError)}
                      placeholder="例: 社内チャットツールの入れ替えを検討しているが、チーム内で認識のずれがありそう。導入前にメンバー間の認識差をなくし、切り替え計画とサポート体制を明確にしたい。現状の使い方、課題、懸念点、導入後の期待などをすり合わせたい。"
                      aria-invalid={!!purposeError}
                    />
                    {purposeError ? (
                      <FieldError>{purposeError}</FieldError>
                    ) : (
                      renderFieldAid(
                        "purpose",
                        "倍速会議を使う目的や、洗い出したい認識の内容を自由に記載してください。",
                      )
                    )}
                  </Field>

                  <Field>
                    <FieldLabel
                      htmlFor="backgroundInfo"
                      className="text-base font-semibold"
                    >
                      背景情報{" "}
                      <span className="ml-1.5 rounded bg-yellow-100 px-1.5 py-0.5 text-xs font-medium text-yellow-800 dark:bg-yellow-900/60 dark:text-yellow-100">
                        任意
                      </span>
                    </FieldLabel>
                    <Textarea
                      id="backgroundInfo"
                      value={backgroundInfo}
                      onChange={(e) => setBackgroundInfo(e.target.value)}
                      rows={4}
                      className={textareaClasses("backgroundInfo")}
                      placeholder="例: 社内チャットツールをSlackから新システムへ切り替える検討を開始。導入担当5名、移行時期は来月で、関係部署との調整に課題がある。高木（情シス）が全社導入を担当、青山（CS）はお客様対応で現行チャットが必須、西村（開発）はリリース準備と兼務。部署ごとに導入タイミングや懸念が異なるため、事前に認識合わせが必要..."
                    />
                    {renderFieldAid(
                      "backgroundInfo",
                      "認識のズレを感じた具体的なきっかけや、解決したい困りごとはありますか？今の状況や背景を少し詳しく何えると、セッションをよりスムーズに進めるための良いヒントになります。",
                    )}
                  </Field>

                  {error && (
                    <Card className="border-red-300 bg-red-50 dark:border-red-900/60 dark:bg-red-950/40">
                      <CardContent className="pt-6">
                        <div className="flex items-start gap-3">
                          <div className="flex-shrink-0 w-5 h-5 rounded-full bg-red-100 dark:bg-red-900/70 flex items-center justify-center">
                            <span className="text-red-600 dark:text-red-200 text-sm font-bold">
                              !
                            </span>
                          </div>
                          <p className="text-sm text-red-800 dark:text-red-100 font-medium">
                            {error}
                          </p>
                        </div>
                      </CardContent>
                    </Card>
                  )}
                </form>
              </CardContent>
            </Card>
          </div>
        </div>
      </div>
    </div>
  );
}

export default function NewSessionPage() {
  return (
    <Suspense
      fallback={
        <div className="min-h-screen flex items-center justify-center">
          <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
        </div>
      }
    >
      <NewSessionContent />
    </Suspense>
  );
}
