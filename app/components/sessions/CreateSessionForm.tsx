"use client";

import axios from "axios";
import { Bot, Globe, Lock, Sparkles } from "lucide-react";
import { useCallback, useEffect, useRef, useState } from "react";

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

type SuggestionField =
  | "backgroundInfo"
  | "recognitionFocus"
  | "recognitionPurpose";

interface Suggestion {
  field: SuggestionField;
  message: string;
}

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

const buildGoalFromInputs = (focus: string, purpose: string) =>
  `【何の認識を洗い出しますか？】${focus}\n【何のために洗い出しますか？】${purpose}`;

export type CreatedSession = {
  id: string;
  title: string;
  context: string;
  goal: string;
  isPublic: boolean;
  hostUserId: string;
  createdAt: string;
  updatedAt: string;
  adminAccessToken?: string;
};

type CreateSessionFormProps = {
  userId: string | null;
  onSuccess?: (session: CreatedSession) => void | Promise<void>;
  submitButtonLabel?: string;
  className?: string;
};

type SessionCreateResponse = {
  session: CreatedSession;
};

export function CreateSessionForm({
  userId,
  onSuccess,
  submitButtonLabel = "セッションを作成",
  className,
}: CreateSessionFormProps) {
  const [title, setTitle] = useState("");
  const [backgroundInfo, setBackgroundInfo] = useState("");
  const [recognitionFocus, setRecognitionFocus] = useState("");
  const [recognitionPurpose, setRecognitionPurpose] = useState("");
  const [visibility, setVisibility] = useState<"public" | "private">("public");
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [suggestions, setSuggestions] = useState<Suggestion[]>([]);
  const [highlightedField, setHighlightedField] =
    useState<SuggestionField | null>(null);
  const lastFormStateRef = useRef<string>("");
  const pollingIntervalRef = useRef<NodeJS.Timeout | null>(null);
  const highlightTimeoutRef = useRef<NodeJS.Timeout | null>(null);

  const fetchSuggestions = useCallback(async () => {
    const currentFormState = JSON.stringify({
      backgroundInfo,
      recognitionFocus,
      recognitionPurpose,
    });

    if (currentFormState === lastFormStateRef.current) {
      return;
    }

    lastFormStateRef.current = currentFormState;

    if (
      !backgroundInfo.trim() &&
      !recognitionFocus.trim() &&
      !recognitionPurpose.trim()
    ) {
      setSuggestions([]);
      return;
    }

    try {
      const response = await axios.post<{ suggestions: Suggestion[] }>(
        "/api/sessions/form-suggestions",
        {
          backgroundInfo,
          recognitionFocus,
          recognitionPurpose,
        },
      );

      setSuggestions(response.data.suggestions || []);
    } catch (err) {
      console.error("Failed to fetch suggestions:", err);
    }
  }, [backgroundInfo, recognitionFocus, recognitionPurpose]);

  useEffect(() => {
    if (pollingIntervalRef.current) {
      clearInterval(pollingIntervalRef.current);
    }

    pollingIntervalRef.current = setInterval(() => {
      void fetchSuggestions();
    }, 5000);

    return () => {
      if (pollingIntervalRef.current) {
        clearInterval(pollingIntervalRef.current);
      }
    };
  }, [fetchSuggestions]);

  useEffect(() => {
    return () => {
      if (highlightTimeoutRef.current) {
        clearTimeout(highlightTimeoutRef.current);
      }
    };
  }, []);

  const handleSuggestionClick = (field: SuggestionField) => {
    const target = document.getElementById(FIELD_META[field].elementId);
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

  const resetForm = () => {
    setTitle("");
    setBackgroundInfo("");
    setRecognitionFocus("");
    setRecognitionPurpose("");
    setVisibility("public");
    setError(null);
    lastFormStateRef.current = "";
  };

  const handleSubmit = async (event: React.FormEvent) => {
    event.preventDefault();

    if (!userId) {
      setError("ユーザー情報を取得できませんでした");
      return;
    }

    setIsSubmitting(true);
    setError(null);
    const goal = buildGoalFromInputs(recognitionFocus, recognitionPurpose);

    try {
      const response = await axios.post<SessionCreateResponse>(
        "/api/sessions",
        {
          title: title.trim(),
          context: backgroundInfo.trim(),
          goal,
          isPublic: visibility === "public",
        },
        { headers: createAuthorizationHeader(userId) },
      );

      const createdSession = response.data.session;

      if (onSuccess) {
        await onSuccess(createdSession);
      }
      resetForm();
    } catch (err) {
      console.error("Failed to create session:", err);
      setError("セッションの作成に失敗しました。もう一度お試しください。");
    } finally {
      setIsSubmitting(false);
    }
  };

  return (
    <Card className={className}>
      <CardHeader>
        <CardTitle>セッション情報</CardTitle>
        <CardDescription>
          AIがより良い「質問」を生成出来るように、なるべく網羅的に情報を入力してください。
        </CardDescription>
      </CardHeader>
      <CardContent>
        <form onSubmit={handleSubmit} className="space-y-6">
          <div className="space-y-2">
            <label htmlFor="title" className="text-sm font-medium">
              タイトル
            </label>
            <Input
              type="text"
              id="title"
              value={title}
              onChange={(event) => setTitle(event.target.value)}
              required
              placeholder="例: 社内チャットツールの入れ替えに関する各メンバーの現状認識のすり合わせ"
            />
            <p className="text-xs text-muted-foreground">
              それぞれの参加者が、何のために回答を収集しているのか分かりやすいタイトルをつけましょう
            </p>
          </div>

          <div className="space-y-3">
            <span className="text-sm font-medium">公開設定</span>
            <div className="flex flex-col gap-3 sm:flex-row sm:items-stretch">
              <label className="flex flex-1 items-start gap-3 rounded-lg border border-input bg-muted px-4 py-3 text-sm shadow-sm transition hover:border-primary/60">
                <input
                  type="radio"
                  name="visibility"
                  value="public"
                  checked={visibility === "public"}
                  onChange={() => setVisibility("public")}
                  className="mt-0.5"
                />
                <span className="flex flex-col gap-1">
                  <span className="flex items-center gap-2 font-medium">
                    <Globe
                      className="h-4 w-4 text-primary"
                      aria-hidden="true"
                    />
                    公開セッション
                  </span>
                  <span className="text-xs text-muted-foreground">
                    Cartographerのトップページに表示されます。誰でもアクセスできるようになります。
                  </span>
                </span>
              </label>
              <label className="flex flex-1 items-start gap-3 rounded-lg border border-input bg-muted px-4 py-3 text-sm shadow-sm transition hover:border-primary/60">
                <input
                  type="radio"
                  name="visibility"
                  value="private"
                  checked={visibility === "private"}
                  onChange={() => setVisibility("private")}
                  className="mt-0.5"
                />
                <span className="flex flex-col gap-1">
                  <span className="flex items-center gap-2 font-medium">
                    <Lock className="h-4 w-4 text-primary" aria-hidden="true" />
                    非公開セッション
                  </span>
                  <span className="text-xs text-muted-foreground">
                    トップページには表示されず、URLを直接共有したメンバーだけがアクセスできます。
                  </span>
                </span>
              </label>
            </div>
          </div>

          <div className="space-y-2">
            <label htmlFor="backgroundInfo" className="text-sm font-medium">
              背景情報（任意）
            </label>
            <textarea
              id="backgroundInfo"
              value={backgroundInfo}
              onChange={(event) => setBackgroundInfo(event.target.value)}
              rows={4}
              className={textareaClasses("backgroundInfo")}
              placeholder="例: 社内チャットツールをSlackから新システムへ切り替える検討を開始。導入担当5名、移行時期は来月で、関係部署との調整に課題がある。高木（情シス）が全社導入を担当、青山（CS）はお客様対応で現行チャットが必須、西村（開発）はリリース準備と兼務。部署ごとに導入タイミングや懸念が異なるため、事前に認識合わせが必要..."
            />
            <p className="text-xs text-muted-foreground space-y-1">
              <span className="block">
                なるべくたくさんの情報量があると、AIが生成する質問の質が上がります。社内チャットや、ドキュメントのコピペでも構いません。
              </span>
              <span className="block text-[11px] text-muted-foreground/80"></span>
            </p>
          </div>

          <div className="space-y-2">
            <label htmlFor="recognitionFocus" className="text-sm font-medium">
              何の認識を洗い出しますか？（必須）
            </label>
            <textarea
              id="recognitionFocus"
              value={recognitionFocus}
              onChange={(event) => setRecognitionFocus(event.target.value)}
              required
              rows={4}
              className={textareaClasses("recognitionFocus")}
              placeholder="例: チャットツール入れ替えに向けた現状の使い方、課題、懸念点、導入後の期待"
            />
            <p className="text-xs text-muted-foreground">
              洗い出したいトピックや範囲を具体的に記載してください。
            </p>
          </div>

          <div className="space-y-2">
            <label htmlFor="recognitionPurpose" className="text-sm font-medium">
              何のために洗い出しますか？（必須）
            </label>
            <textarea
              id="recognitionPurpose"
              value={recognitionPurpose}
              onChange={(event) => setRecognitionPurpose(event.target.value)}
              required
              rows={4}
              className={textareaClasses("recognitionPurpose")}
              placeholder="例: 導入前にメンバー間の認識差をなくし、切り替え計画とサポート体制を明確にするため"
            />
            <p className="text-xs text-muted-foreground">
              洗い出しの目的や、きっかけとなるもやもや、その先に実現したいことを書いてください。
            </p>
          </div>

          {suggestions.length > 0 && (
            <Card className="border-blue-200 bg-blue-50/50">
              <CardContent className="pt-6">
                <div className="flex items-start gap-3">
                  <Bot className="h-5 w-5 text-blue-600 mt-0.5 flex-shrink-0" />
                  <div className="flex-1 space-y-3">
                    <p className="text-sm font-medium text-blue-900">
                      AIアシスタント: もっとこういう情報を書いてみませんか？
                    </p>
                    <ul className="space-y-2">
                      {suggestions.map((suggestion) => (
                        <li key={`${suggestion.field}-${suggestion.message}`}>
                          <button
                            type="button"
                            onClick={() =>
                              handleSuggestionClick(suggestion.field)
                            }
                            className="w-full rounded-lg bg-blue-100/60 px-3 py-2 text-left transition hover:bg-blue-100 focus:outline-none focus-visible:ring-2 focus-visible:ring-blue-400"
                          >
                            <span className="inline-flex items-center rounded-full bg-white/80 px-2 py-0.5 text-xs font-medium text-blue-700">
                              {FIELD_META[suggestion.field].label}
                            </span>
                            <span className="mt-1 block text-sm text-blue-900 leading-relaxed">
                              {suggestion.message}
                            </span>
                            <span className="mt-1 inline-flex items-center text-xs font-medium text-blue-700">
                              クリックして入力欄にジャンプ
                            </span>
                          </button>
                        </li>
                      ))}
                    </ul>
                  </div>
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

          <Button
            type="submit"
            disabled={isSubmitting}
            isLoading={isSubmitting}
            className="w-full"
          >
            <Sparkles className="h-4 w-4" />
            {submitButtonLabel}
          </Button>
        </form>
      </CardContent>
    </Card>
  );
}
