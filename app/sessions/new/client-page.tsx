"use client";

import axios from "axios";
import { ArrowUpRight, Loader2, Sparkles } from "lucide-react";
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
import { Input } from "@/components/ui/input";
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
    <Card className="border-blue-200 bg-blue-50/50">
      <CardContent className="pt-6">
        <div className="flex items-start gap-3">
          {/* <Bot className="h-5 w-5 text-blue-600 mt-0.5 flex-shrink-0" /> */}
          <div className="flex-1 space-y-3">
            {/* <p className="text-sm font-medium text-blue-900">
              AIアシスタント: {SUGGESTION_TITLES[field]}
            </p> */}
            <ul className="space-y-2">
              {fieldSuggestions.map((suggestion) => (
                <li key={`${suggestion.field}-${suggestion.message}`}>
                  {/* <button
                    type="button"
                    onClick={() => onClick(suggestion.field)}
                    className="w-full rounded-lg bg-blue-100/60 px-3 py-2 text-left transition hover:bg-blue-100 focus:outline-none focus-visible:ring-2 focus-visible:ring-blue-400"
                  > */}
                  <span className="inline-flex items-center rounded-full bg-white/80 px-2 py-0.5 text-xs font-medium text-blue-700">
                    {FIELD_META[suggestion.field]?.label ?? "参考"}
                  </span>
                  <span className="mt-1 block text-sm leading-relaxed text-blue-900">
                    {suggestion.message}
                  </span>
                  {/* </button> */}
                </li>
              ))}
            </ul>
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
  const [suggestions, setSuggestions] = useState<Suggestion[]>([]);
  const [highlightedField, setHighlightedField] =
    useState<SuggestionField | null>(null);
  const lastFormStateRef = useRef<string>("");
  const highlightTimeoutRef = useRef<NodeJS.Timeout | null>(null);
  const suggestionAbortRef = useRef<AbortController | null>(null);
  const [previewQuestions, setPreviewQuestions] = useState<string[]>([]);
  const [isPreviewLoading, setIsPreviewLoading] = useState(false);
  const [previewError, setPreviewError] = useState<string | null>(null);

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
      return;
    }

    try {
      if (suggestionAbortRef.current) {
        suggestionAbortRef.current.abort();
      }
      const controller = new AbortController();
      suggestionAbortRef.current = controller;

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
    } catch (err) {
      if ((err as { name?: string }).name === "CanceledError") {
        return;
      }
      console.error("Failed to fetch suggestions:", err);
    }
  }, [backgroundInfo, purpose]);

  useEffect(() => {
    const debounceHandle = setTimeout(() => {
      void fetchSuggestions();
    }, 350);

    return () => {
      clearTimeout(debounceHandle);
      if (suggestionAbortRef.current) {
        suggestionAbortRef.current.abort();
      }
    };
  }, [fetchSuggestions]);

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

  const textareaClasses = (field: SuggestionField) =>
    [
      "flex w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50 resize-y",
      highlightedField === field ? "ring-2 ring-blue-400 border-blue-400" : "",
    ]
      .filter(Boolean)
      .join(" ");

  const renderFieldAid = (field: SuggestionField, fallback: string) => {
    const hasSuggestions = suggestions.some(
      (suggestion) => suggestion.field === field,
    );
    if (!hasSuggestions) {
      return <p className="text-xs text-muted-foreground">{fallback}</p>;
    }
    return renderSuggestionCard(field, suggestions, handleSuggestionClick);
  };

  const handlePreview = useCallback(async () => {
    if (!userId) return;

    if (!title.trim() || !purpose.trim()) {
      setPreviewError("タイトルと目的を入力してください");
      return;
    }

    setIsPreviewLoading(true);
    setPreviewError(null);

    const goal = buildGoalFromInputs(purpose);

    try {
      const response = await axios.post(
        "/api/sessions/preview-questions",
        {
          title: title.trim(),
          goal,
          context: backgroundInfo.trim(),
        },
        {
          headers: createAuthorizationHeader(userId),
        },
      );
      setPreviewQuestions(response.data.questions);
    } catch (err) {
      console.error("Failed to generate preview:", err);
      setPreviewQuestions([]);
      setPreviewError("質問の生成に失敗しました。もう一度お試しください。");
    } finally {
      setIsPreviewLoading(false);
    }
  }, [backgroundInfo, purpose, title, userId]);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    if (!userId) {
      setError("User ID not available");
      return;
    }

    setIsSubmitting(true);
    setError(null);
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

  return (
    <div className="min-h-screen bg-background">
      <div className="max-w-2xl mx-auto px-4 py-12 sm:px-6 lg:px-8">
        <div className="mb-8">
          <h1 className="text-3xl font-bold tracking-tight mb-2">
            新しいセッションを作成
          </h1>
          <p className="text-muted-foreground">
            セッション情報をもとにAIが質問を生成します
          </p>
          <p className="text-sm text-muted-foreground mt-2">
            なるべくたくさんの情報量があると、AIが生成する質問の質が上がります。社内チャットや、ドキュメントのコピペでも構いません。
          </p>
          <p className="text-xs text-muted-foreground/80 mt-1">
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

        <Card>
          <CardContent className="pt-6">
            <form onSubmit={handleSubmit} className="space-y-6">
              <div className="space-y-2">
                <label htmlFor="title" className="text-base font-semibold">
                  セッションのタイトル
                </label>
                <Input
                  type="text"
                  id="title"
                  value={title}
                  onChange={(e) => setTitle(e.target.value)}
                  required
                  placeholder="例: 社内チャットツールの入れ替えに関する各メンバーの現状認識のすり合わせ"
                />
                <p className="text-xs text-muted-foreground">
                  それぞれの参加者が、何のために回答を収集しているのか分かりやすいタイトルをつけましょう
                </p>
              </div>

              <div className="space-y-2">
                <label htmlFor="purpose" className="text-base font-semibold">
                  何をするために倍速会議を使うのですか？
                </label>
                <textarea
                  id="purpose"
                  value={purpose}
                  onChange={(e) => setPurpose(e.target.value)}
                  required
                  rows={6}
                  className={textareaClasses("purpose")}
                  placeholder="例: 社内チャットツールの入れ替えを検討しているが、チーム内で認識のずれがありそう。導入前にメンバー間の認識差をなくし、切り替え計画とサポート体制を明確にしたい。現状の使い方、課題、懸念点、導入後の期待などをすり合わせたい。"
                />
                {renderFieldAid(
                  "purpose",
                  "倍速会議を使う目的や、洗い出したい認識の内容を自由に記載してください。",
                )}
              </div>

              <div className="space-y-3">
                <label
                  htmlFor="backgroundInfo"
                  className="text-base font-semibold"
                >
                  背景情報{" "}
                  <span className="ml-1.5 rounded bg-yellow-100 px-1.5 py-0.5 text-xs font-medium text-yellow-800">
                    任意
                  </span>
                </label>
                <textarea
                  id="backgroundInfo"
                  value={backgroundInfo}
                  onChange={(e) => setBackgroundInfo(e.target.value)}
                  rows={4}
                  className={textareaClasses("backgroundInfo")}
                  placeholder="例: 社内チャットツールをSlackから新システムへ切り替える検討を開始。導入担当5名、移行時期は来月で、関係部署との調整に課題がある。高木（情シス）が全社導入を担当、青山（CS）はお客様対応で現行チャットが必須、西村（開発）はリリース準備と兼務。部署ごとに導入タイミングや懸念が異なるため、事前に認識合わせが必要..."
                />
              </div>
              {renderSuggestionCard(
                "backgroundInfo",
                suggestions,
                handleSuggestionClick,
              )}

              <div className="flex flex-col gap-3 sm:flex-row sm:items-center sm:justify-between">
                <Button
                  type="button"
                  variant="outline"
                  onClick={handlePreview}
                  isLoading={isPreviewLoading}
                  disabled={isSubmitting}
                >
                  質問をプレビュー
                </Button>
                <Button
                  type="submit"
                  disabled={isSubmitting}
                  isLoading={isSubmitting}
                  className="w-full sm:w-auto"
                >
                  <Sparkles className="h-4 w-4" />
                  セッションを作成
                </Button>
              </div>

              {previewError && (
                <p className="text-sm text-destructive" role="alert">
                  {previewError}
                </p>
              )}

              {previewQuestions.length > 0 && (
                <Card className="border-indigo-100 bg-indigo-50/40">
                  <CardHeader className="pb-3">
                    <CardTitle className="text-base text-indigo-900">
                      生成された質問プレビュー
                    </CardTitle>
                    <CardDescription>
                      YES/NO
                      で回答される想定のステートメントです。内容だけ確認してください。
                    </CardDescription>
                  </CardHeader>
                  <CardContent>
                    <div className="max-h-72 space-y-3 overflow-y-auto pr-1">
                      {previewQuestions.map((q, _index) => (
                        <div
                          key={q}
                          className="rounded-lg border border-border/60 bg-white shadow-sm"
                        >
                          <div className="flex items-start gap-3 px-4 py-3">
                            {/* <span className="mt-0.5 inline-flex h-6 w-6 items-center justify-center rounded-full bg-slate-900 text-xs font-semibold text-white">
                              {index + 1}
                            </span> */}
                            <p className="text-sm leading-relaxed text-slate-900">
                              {q}
                            </p>
                          </div>
                        </div>
                      ))}
                    </div>
                  </CardContent>
                </Card>
              )}

              {error && (
                <Card className="border-destructive">
                  <CardContent className="pt-6">
                    <p className="text-sm text-destructive">{error}</p>
                  </CardContent>
                </Card>
              )}
            </form>
          </CardContent>
        </Card>
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
