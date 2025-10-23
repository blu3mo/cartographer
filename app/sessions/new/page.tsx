"use client";

import { useState } from "react";
import { useRouter } from "next/navigation";
import { useUserId } from "@/lib/useUserId";
import { createAuthorizationHeader } from "@/lib/auth";
import axios from "axios";
import { Button } from "@/components/ui/Button";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import { Loader2, Sparkles } from "lucide-react";

export default function NewSessionPage() {
  const router = useRouter();
  const { userId, isLoading: userLoading } = useUserId();
  const [title, setTitle] = useState("");
  const [whatToClarify, setWhatToClarify] = useState("");
  const [purpose, setPurpose] = useState("");
  const [backgroundInfo, setBackgroundInfo] = useState("");
  const [visibility, setVisibility] = useState<"public" | "private">("public");
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    if (!userId) {
      setError("User ID not available");
      return;
    }

    setIsSubmitting(true);
    setError(null);

    try {
      // Combine fields into context
      const contextSections = [
        `【何の認識を洗い出すか】\n${whatToClarify}`,
        `【何のために洗い出すか】\n${purpose}`,
      ];

      if (backgroundInfo.trim().length > 0) {
        contextSections.push(`【背景情報】\n${backgroundInfo}`);
      }

      const context = contextSections.join("\n\n");

      const response = await axios.post(
        "/api/sessions",
        { title, context, isPublic: visibility === "public" },
        { headers: createAuthorizationHeader(userId) },
      );

      const sessionId = response.data.session.id;
      router.push(`/sessions/${sessionId}/admin`);
    } catch (err) {
      console.error("Failed to create session:", err);
      setError("セッションの作成に失敗しました。もう一度お試しください。");
      setIsSubmitting(false);
    }
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
            チームの認識を可視化し、合意形成を促進しましょう
          </p>
        </div>

        <Card>
          <CardHeader>
            <CardTitle>セッション情報</CardTitle>
            <CardDescription>
              セッションの目的と範囲を明確にしてください
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
                  placeholder="例: プロジェクトXの現状認識"
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
                <label htmlFor="whatToClarify" className="text-sm font-medium">
                  何の認識を洗い出すか
                </label>
                <textarea
                  id="whatToClarify"
                  value={whatToClarify}
                  onChange={(e) => setWhatToClarify(e.target.value)}
                  required
                  rows={4}
                  className="flex w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50 resize-none"
                  placeholder="例: プロジェクトXの現状、課題、今後の方向性について"
                />
                <p className="text-xs text-muted-foreground">
                  洗い出したいトピックや範囲を具体的に記載してください
                </p>
              </div>

              <div className="space-y-2">
                <label htmlFor="purpose" className="text-sm font-medium">
                  何のために洗い出すか
                </label>
                <textarea
                  id="purpose"
                  value={purpose}
                  onChange={(e) => setPurpose(e.target.value)}
                  required
                  rows={4}
                  className="flex w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50 resize-none"
                  placeholder="例: チーム全体で認識を合わせ、次のアクションを決めるため"
                />
                <p className="text-xs text-muted-foreground">
                  このセッションの目的やゴールを明確にしましょう
                </p>
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
                  className="flex w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50 resize-none"
                  placeholder="例: プロジェクトXは3ヶ月前に立ち上がり、現在はMVPのリリース直前。開発チームは5名で、関係部署との連携が課題になっている。"
                />
                <p className="text-xs text-muted-foreground">
                  セッションの背景となる経緯や関係者情報など、AIに共有したいコンテキストを記載してください
                </p>
              </div>

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
