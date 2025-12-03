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

type SuggestionField =
  | "backgroundInfo"
  | "recognitionFocus"
  | "recognitionPurpose";

type Suggestion = {
  field: SuggestionField;
  message: string;
};

const FIELD_META: Record<
  SuggestionField,
  { label: string; elementId: string }
> = {
  backgroundInfo: { label: "背景情報", elementId: "backgroundInfo" },
  recognitionFocus: {
    label: "洗い出したい認識",
    elementId: "recognitionFocus",
  },
  recognitionPurpose: {
    label: "洗い出す目的",
    elementId: "recognitionPurpose",
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

  const purposeKeywords = [
    "目的",
    "ために",
    "ため",
    "ゴール",
    "目標",
    "狙い",
    "意図",
    "理由",
    "目指",
    "最終的",
    "どうなって",
    "理想",
    "ビジョン",
    "状態",
    "達成",
    "成果",
    "実現",
  ];

  if (includes(backgroundKeywords)) {
    return "backgroundInfo";
  }

  if (includes(purposeKeywords)) {
    return "recognitionPurpose";
  }

  return "recognitionFocus";
};

const renderSuggestionCard = (
  field: SuggestionField,
  suggestions: Suggestion[],
  onClick: (field: SuggestionField) => void,
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
                  <button
                    type="button"
                    onClick={() => onClick(suggestion.field)}
                    className="w-full rounded-lg bg-blue-100/60 px-3 py-2 text-left transition hover:bg-blue-100 focus:outline-none focus-visible:ring-2 focus-visible:ring-blue-400"
                  >
                    <p className="text-sm leading-relaxed text-blue-900">
                      {suggestion.message}
                    </p>
                  </button>
                </li>
              ))}
            </ul>
          </div>
        </div>
      </CardContent>
    </Card>
  );
};

function SuggestionsLoader({
  onSuggestions,
}: {
  onSuggestions: (suggestions: Suggestion[]) => void;
}) {
  const searchParams = useSearchParams();
  const prompt = searchParams.get("prompt");
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (!prompt) return;

    const fetchSuggestions = async () => {
      setLoading(true);
      setError(null);
      try {
        const response = await axios.post("/api/sessions/form-suggestions", {
          prompt,
        });
        const messages = response.data?.suggestions ?? [];
        const parsed: Suggestion[] = (messages as string[]).map((message) => ({
          field: inferFieldFromMessage(message),
          message,
        }));
        onSuggestions(parsed);
      } catch (err) {
        console.error("Failed to load suggestions:", err);
        setError("提案の取得に失敗しました");
      } finally {
        setLoading(false);
      }
    };

    void fetchSuggestions();
  }, [prompt, onSuggestions]);

  if (!prompt) return null;
  if (loading) {
    return (
      <div className="flex items-center gap-2 text-sm text-blue-900">
        <Loader2 className="h-4 w-4 animate-spin" />
        提案を読み込んでいます…
      </div>
    );
  }
  if (error) {
    return (
      <p className="text-sm text-red-600">
        提案の読み込みに失敗しました。もう一度お試しください。
      </p>
    );
  }
  return null;
}

export default function NewSessionPage() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const { userId, isLoading } = useUserId();

  const [title, setTitle] = useState(searchParams.get("title") || "");
  const [goal, setGoal] = useState(searchParams.get("purpose") || "");
  const [context, setContext] = useState(searchParams.get("background") || "");
  const [creating, setCreating] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [suggestions, setSuggestions] = useState<Suggestion[]>([]);
  const formRef = useRef<HTMLFormElement>(null);

  const handleApplySuggestion = useCallback((field: SuggestionField) => {
    const meta = FIELD_META[field];
    const element = document.getElementById(
      meta.elementId,
    ) as HTMLTextAreaElement | null;
    element?.focus();
  }, []);

  const handleSubmit = useCallback(
    async (event: React.FormEvent<HTMLFormElement>) => {
      event.preventDefault();
      if (!userId) return;

      setCreating(true);
      setError(null);

      try {
        const response = await axios.post(
          "/api/sessions",
          { title, goal, context },
          {
            headers: createAuthorizationHeader(userId),
          },
        );

        const { id, adminAccessToken } = response.data.session;
        router.push(`/sessions/${id}/${adminAccessToken}`);
      } catch (err) {
        console.error("Failed to create session:", err);
        setError("セッションの作成に失敗しました。もう一度お試しください。");
      } finally {
        setCreating(false);
      }
    },
    [userId, title, goal, context, router],
  );

  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.metaKey && event.key === "Enter") {
        formRef.current?.requestSubmit();
      }
    };
    window.addEventListener("keydown", handleKeyDown);
    return () => window.removeEventListener("keydown", handleKeyDown);
  }, []);

  if (isLoading || !userId) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <Loader2 className="h-6 w-6 animate-spin text-muted-foreground" />
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gradient-to-b from-white via-slate-50 to-white">
      <div className="mx-auto max-w-4xl px-4 py-10 sm:px-6 lg:px-8">
        <div className="mb-10 space-y-2">
          <div className="inline-flex items-center gap-2 rounded-full bg-blue-50 px-3 py-1 text-xs font-medium text-blue-700">
            <Sparkles className="h-3.5 w-3.5" />
            倍速会議で合意形成をはじめる
          </div>
          <h1 className="text-3xl font-bold tracking-tight text-slate-900 sm:text-4xl">
            新しいセッションを作成
          </h1>
          <p className="text-base text-slate-600">
            背景や目的を簡潔に入力すると、参加者の認識を集めやすくなります。
          </p>
        </div>

        <form
          ref={formRef}
          onSubmit={handleSubmit}
          className="grid gap-6 lg:grid-cols-[2fr_1fr] lg:items-start"
        >
          <div className="space-y-6">
            <Card className="shadow-sm">
              <CardHeader className="pb-3">
                <CardTitle>セッション情報</CardTitle>
                <CardDescription>
                  参加者に共有される基本情報を入力してください。
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-4">
                <div className="space-y-2">
                  <label
                    className="text-sm font-medium text-slate-800"
                    htmlFor="title"
                  >
                    タイトル
                  </label>
                  <Input
                    id="title"
                    required
                    value={title}
                    onChange={(e) => setTitle(e.target.value)}
                    placeholder="例: 24Q1 プロダクト戦略レビュー"
                  />
                </div>
                <div className="space-y-2">
                  <label
                    className="text-sm font-medium text-slate-800"
                    htmlFor="recognitionPurpose"
                  >
                    目的（ゴール）
                  </label>
                  <textarea
                    id="recognitionPurpose"
                    required
                    value={goal}
                    onChange={(e) => setGoal(e.target.value)}
                    className="w-full rounded-md border border-slate-200 bg-white px-3 py-2 text-sm shadow-sm focus:border-blue-400 focus:outline-none focus:ring-2 focus:ring-blue-100"
                    placeholder="例: チームで「今期やるべきこと」を合意し、OKRに落とし込む"
                    rows={3}
                  />
                  <p className="text-xs text-slate-500">
                    参加者が回答するときの判断基準になります。
                  </p>
                </div>
                <div className="space-y-2">
                  <label
                    className="text-sm font-medium text-slate-800"
                    htmlFor="backgroundInfo"
                  >
                    背景・補足情報
                  </label>
                  <textarea
                    id="backgroundInfo"
                    value={context}
                    onChange={(e) => setContext(e.target.value)}
                    className="w-full rounded-md border border-slate-200 bg-white px-3 py-2 text-sm shadow-sm focus:border-blue-400 focus:outline-none focus:ring-2 focus:ring-blue-100"
                    placeholder="例: メンバー構成やこれまでの議論経緯など"
                    rows={5}
                  />
                  <p className="text-xs text-slate-500">
                    参加者が「補足情報」を読むことで、回答の前提が揃いやすくなります。
                  </p>
                </div>
              </CardContent>
            </Card>

            <div className="flex items-center justify-between">
              <div className="text-sm text-slate-500">
                ⌘ + Enter で作成できます
              </div>
              <Button type="submit" size="lg" disabled={creating}>
                {creating ? (
                  <>
                    <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                    作成中…
                  </>
                ) : (
                  <>
                    作成する
                    <ArrowUpRight className="ml-1.5 h-4 w-4" />
                  </>
                )}
              </Button>
            </div>

            {error && (
              <p className="text-sm text-red-600" role="alert">
                {error}
              </p>
            )}
          </div>

          <div className="space-y-4">
            <Card className="border-blue-100 bg-blue-50/60 shadow-sm">
              <CardHeader className="pb-3">
                <CardTitle className="text-base text-blue-900">
                  記入のヒント
                </CardTitle>
                <CardDescription className="text-sm text-blue-800">
                  下書きを貼り付けると、項目別に改善ポイントを提案します。
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-3">
                <Suspense fallback={null}>
                  <SuggestionsLoader onSuggestions={setSuggestions} />
                </Suspense>
                {[
                  "recognitionPurpose",
                  "recognitionFocus",
                  "backgroundInfo",
                ].map((field) =>
                  renderSuggestionCard(
                    field as SuggestionField,
                    suggestions,
                    handleApplySuggestion,
                  ),
                )}
              </CardContent>
            </Card>
          </div>
        </form>
      </div>
    </div>
  );
}
