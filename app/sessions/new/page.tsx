"use client";

import axios from "axios";
import { Lightbulb, Loader2, Sparkles } from "lucide-react";
import { useRouter, useSearchParams } from "next/navigation";
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
import { useUserId } from "@/lib/useUserId";

const buildGoalFromInputs = (focus: string, purpose: string) =>
  `【何の認識を洗い出しますか？】${focus}\n【何のために洗い出しますか？】${purpose}`;

export default function NewSessionPage() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const { userId, isLoading: userLoading } = useUserId();
  const [title, setTitle] = useState(searchParams.get("title") || "");
  const [backgroundInfo, setBackgroundInfo] = useState(
    searchParams.get("background") || "",
  );
  const [recognitionFocus, setRecognitionFocus] = useState(
    searchParams.get("topic") || "",
  );
  const [recognitionPurpose, setRecognitionPurpose] = useState(
    searchParams.get("purpose") || "",
  );
  const [visibility, setVisibility] = useState<"public" | "private">(
    (searchParams.get("visibility") as "public" | "private") === "private"
      ? "private"
      : "public",
  );
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [suggestions, setSuggestions] = useState<string[]>([]);
  const lastFormStateRef = useRef<string>("");
  const pollingIntervalRef = useRef<NodeJS.Timeout | null>(null);

  useEffect(() => {
    document.title = "新しいセッションを作成 - Cartographer";
    return () => {
      document.title = "Cartographer - 認識を可視化し、合意形成を促進する";
    };
  }, []);

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
      const response = await axios.post("/api/sessions/form-suggestions", {
        backgroundInfo,
        recognitionFocus,
        recognitionPurpose,
      });

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
      fetchSuggestions();
    }, 5000);

    return () => {
      if (pollingIntervalRef.current) {
        clearInterval(pollingIntervalRef.current);
      }
    };
  }, [fetchSuggestions]);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    if (!userId) {
      setError("User ID not available");
      return;
    }

    setIsSubmitting(true);
    setError(null);
    const goal = buildGoalFromInputs(recognitionFocus, recognitionPurpose);

    try {
      const response = await axios.post(
        "/api/sessions",
        {
          title: title.trim(),
          context: backgroundInfo.trim(),
          goal,
          isPublic: visibility === "public",
        },
        { headers: createAuthorizationHeader(userId) },
      );

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
            チームの認識を可視化し、合意形成や新たな気づきにつなげていきましょう
          </p>
        </div>

        <Card>
          <CardHeader>
            <CardTitle>セッション情報</CardTitle>
            <CardDescription>
              いくつかの質問に答えると、みんなで話したいことが整理されます。
            </CardDescription>
          </CardHeader>
          <CardContent>
            <form onSubmit={handleSubmit} className="space-y-6">
              <div className="space-y-2">
                <label htmlFor="title" className="text-sm font-medium">
                  セッションのタイトル
                </label>
                <Input
                  type="text"
                  id="title"
                  value={title}
                  onChange={(e) => setTitle(e.target.value)}
                  required
                  placeholder="例: プロジェクトの現状認識"
                />
                <p className="text-xs text-muted-foreground">
                  セッションを識別しやすいタイトルを付けましょう
                </p>
              </div>

              <div className="space-y-3">
                <span className="text-sm font-medium">公開設定</span>
                <div className="flex flex-col gap-3 sm:flex-row">
                  <label className="flex items-start gap-3 rounded-lg border border-input bg-muted px-4 py-3 text-sm shadow-sm transition hover:border-primary/60">
                    <input
                      type="radio"
                      name="visibility"
                      value="public"
                      checked={visibility === "public"}
                      onChange={() => setVisibility("public")}
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
                      name="visibility"
                      value="private"
                      checked={visibility === "private"}
                      onChange={() => setVisibility("private")}
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
                <label htmlFor="backgroundInfo" className="text-sm font-medium">
                  背景情報（任意）
                </label>
                <textarea
                  id="backgroundInfo"
                  value={backgroundInfo}
                  onChange={(e) => setBackgroundInfo(e.target.value)}
                  rows={4}
                  className="flex w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50 resize-y"
                  placeholder="例: プロジェクトは3ヶ月前に立ち上がり、現在はプロダクトのリリース直前。開発チームは5名で、関係部署との連携が課題になっている。"
                />
                <p className="text-xs text-muted-foreground">
                  共有しておくと助かる背景や状況があればどうぞ。なくても問題ありません。
                </p>
              </div>

              <div className="space-y-2">
                <label
                  htmlFor="recognitionFocus"
                  className="text-sm font-medium"
                >
                  何の認識を洗い出しますか？
                </label>
                <textarea
                  id="recognitionFocus"
                  value={recognitionFocus}
                  onChange={(e) => setRecognitionFocus(e.target.value)}
                  required
                  rows={4}
                  className="flex w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50 resize-y"
                  placeholder="例: プロジェクトの現状、課題、今後の方向性について"
                />
                <p className="text-xs text-muted-foreground">
                  洗い出したいトピックや範囲を具体的に記載してください。
                </p>
              </div>

              <div className="space-y-2">
                <label
                  htmlFor="recognitionPurpose"
                  className="text-sm font-medium"
                >
                  何のために洗い出しますか？
                </label>
                <textarea
                  id="recognitionPurpose"
                  value={recognitionPurpose}
                  onChange={(e) => setRecognitionPurpose(e.target.value)}
                  required
                  rows={4}
                  className="flex w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50 resize-y"
                  placeholder="例: チーム全体で認識を合わせ、次のアクションを決めるため"
                />
                <p className="text-xs text-muted-foreground">
                  洗い出しの目的や、きっかけとなるもやもや、その先に実現したいことを書いてください。
                </p>
              </div>

              {suggestions.length > 0 && (
                <Card className="border-blue-200 bg-blue-50/50">
                  <CardContent className="pt-6">
                    <div className="flex items-start gap-3">
                      <Lightbulb className="h-5 w-5 text-blue-600 mt-0.5 flex-shrink-0" />
                      <div className="flex-1 space-y-3">
                        <p className="text-sm font-medium text-blue-900">
                          もっとこういう情報を書いてみませんか？
                        </p>
                        <ul className="space-y-2">
                          {suggestions.map((suggestion) => (
                            <li
                              key={suggestion}
                              className="text-sm text-blue-800 leading-relaxed"
                            >
                              {suggestion}
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
                セッションを作成
              </Button>
            </form>
          </CardContent>
        </Card>
      </div>
    </div>
  );
}
