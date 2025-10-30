"use client";

import axios from "axios";
import { ArrowDown, Loader2, Sparkles, Wand2 } from "lucide-react";
import { useRouter } from "next/navigation";
import { useEffect, useRef, useState } from "react";

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

export default function NewSessionPage() {
  const router = useRouter();
  const { userId, isLoading: userLoading } = useUserId();
  const [title, setTitle] = useState("");
  const [keyStakeholders, setKeyStakeholders] = useState("");
  const [
    participantPerspectiveFocus,
    setParticipantPerspectiveFocus,
  ] = useState("");
  const [insightTargets, setInsightTargets] = useState<string[]>([]);
  const [decisionOutcome, setDecisionOutcome] = useState("");
  const [discussionTrigger, setDiscussionTrigger] = useState("");
  const [backgroundInfo, setBackgroundInfo] = useState("");
  const [goal, setGoal] = useState("");
  const [isGoalGenerating, setIsGoalGenerating] = useState(false);
  const [goalError, setGoalError] = useState<string | null>(null);
  const [hasGeneratedGoal, setHasGeneratedGoal] = useState(false);
  const [isGoalStale, setIsGoalStale] = useState(false);
  const [visibility, setVisibility] = useState<"public" | "private">("public");
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const goalRequestControllerRef = useRef<AbortController | null>(null);

  const insightOptions = [
    {
      value: "agreement",
      label: "合意点を見つけたい",
      description: "みんながどこまで同じ考えを持っているか知りたいときに。",
    },
    {
      value: "difference",
      label: "相違点を見極めたい",
      description: "意見が分かれている領域を把握しておきたい場合に。",
    },
    {
      value: "unknown",
      label: "わかっていない点をあぶり出したい",
      description: "まだ話せていない論点や疑問点を明確にしたいときに。",
    },
  ] as const;

  const handleInsightToggle = (value: string) => {
    setInsightTargets((prev) => {
      const alreadySelected = prev.includes(value);
      const next = alreadySelected
        ? prev.filter((item) => item !== value)
        : [...prev, value];
      if (hasGeneratedGoal) {
        setIsGoalStale(true);
      }
      return next;
    });
    if (goalError) {
      setGoalError(null);
    }
  };

  useEffect(() => {
    document.title = "新しいセッションを作成 - Cartographer";
    return () => {
      document.title = "Cartographer - 認識を可視化し、合意形成を促進する";
    };
  }, []);

  const canGenerateGoal =
    title.trim().length > 0 &&
    keyStakeholders.trim().length > 0 &&
    insightTargets.length > 0 &&
    decisionOutcome.trim().length > 0 &&
    discussionTrigger.trim().length > 0;

  const markGoalAsStale = () => {
    if (hasGeneratedGoal) {
      setIsGoalStale(true);
    }
  };

  const handleGenerateGoal = async () => {
    if (isGoalGenerating) {
      goalRequestControllerRef.current?.abort();
      setGoalError(null);
      return;
    }

    if (!userId) {
      setGoalError("User ID not available");
      return;
    }

    if (!canGenerateGoal) {
      setGoalError(
        "タイトルと必須の質問（関わる人・知りたいポイント・決めたいこと・今話したい理由）を埋めてから試してくださいね。",
      );
      return;
    }

    const controller = new AbortController();
    goalRequestControllerRef.current = controller;

    setIsGoalGenerating(true);
    setGoalError(null);

    try {
      const payload = {
        title: title.trim(),
        participants: keyStakeholders.trim(),
        perspectiveFocus:
          participantPerspectiveFocus.trim().length > 0
            ? participantPerspectiveFocus.trim()
            : "特になし",
        insightTargets,
        decision: decisionOutcome.trim(),
        trigger: discussionTrigger.trim(),
        background: backgroundInfo,
      };

      const response = await axios.post(
        "/api/sessions/goal",
        payload,
        {
          headers: createAuthorizationHeader(userId),
          signal: controller.signal,
        },
      );

      const generatedGoal = response.data.goal;
      if (
        typeof generatedGoal === "string" &&
        generatedGoal.trim().length > 0
      ) {
        setGoal(generatedGoal.trim());
        setHasGeneratedGoal(true);
        setIsGoalStale(false);
      } else {
        setGoalError("ゴールの生成結果が不正です。もう一度お試しください。");
      }
    } catch (err) {
      if (axios.isCancel(err)) {
        return;
      }
      console.error("Failed to generate session goal:", err);
      setGoalError("ゴールの生成に失敗しました。もう一度お試しください。");
    } finally {
      if (goalRequestControllerRef.current === controller) {
        goalRequestControllerRef.current = null;
        setIsGoalGenerating(false);
      }
    }
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    if (!userId) {
      setError("User ID not available");
      return;
    }

    if (goal.trim().length === 0) {
      setGoalError("ゴールを生成または入力してください。");
      return;
    }

    setIsSubmitting(true);
    setError(null);
    setGoalError(null);

    try {
      const response = await axios.post(
        "/api/sessions",
        {
          title: title.trim(),
          context: backgroundInfo.trim(),
          goal: goal.trim(),
          isPublic: visibility === "public",
        },
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
                  onChange={(e) => {
                    setTitle(e.target.value);
                    markGoalAsStale();
                  }}
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
                <label htmlFor="backgroundInfo" className="text-sm font-medium">
                  背景情報（任意）
                </label>
                <textarea
                  id="backgroundInfo"
                  value={backgroundInfo}
                  onChange={(e) => {
                    setBackgroundInfo(e.target.value);
                    markGoalAsStale();
                  }}
                  rows={4}
                  className="flex w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50 resize-none"
                  placeholder="例: プロジェクトXは3ヶ月前に立ち上がり、現在はプロダクトのリリース直前。開発チームは5名で、関係部署との連携が課題になっている。"
                />
                <p className="text-xs text-muted-foreground">
                  共有しておくと助かる背景や状況があればどうぞ。なくても問題ありません。
                </p>
              </div>

              <div className="rounded-lg border border-dashed border-input/60 bg-muted/30 px-4 py-3 mt-12">
                <p className="text-sm font-medium">
                  まず、<span className="font-semibold">「話し合いの基本設定」</span>について教えてください。
                </p>
              </div>

              <div className="space-y-2">
                <label htmlFor="participants" className="text-sm font-medium">
                  主にどんな人が参加者ですか？
                </label>
                <textarea
                  id="participants"
                  value={keyStakeholders}
                  onChange={(e) => {
                    setKeyStakeholders(e.target.value);
                    markGoalAsStale();
                  }}
                  rows={4}
                  className="flex w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50 resize-none"
                  placeholder="例: 開発チームのリーダー陣、プロダクトマネージャー"
                />
                <p className="text-xs text-muted-foreground">
                  どんなメンバーに回答者として参加してもらいたいかを教えてください。「部署全員」などざっくりでも大丈夫です。
                </p>
              </div>

              <div className="rounded-lg border border-dashed border-input/60 bg-muted/30 px-4 py-3">
                <p className="text-sm font-medium">
                  次に、<span className="font-semibold">「参加者の現状や理想について、どんなことを整理したいのか」</span>教えてください。
                </p>
              </div>

              <div className="space-y-2">
                <label
                  htmlFor="perspectiveFocus"
                  className="text-sm font-medium"
                >
                  参加者から見た視点について、どんなことを聞いてみたい・整理したいですか？
                </label>
                <textarea
                  id="perspectiveFocus"
                  value={participantPerspectiveFocus}
                  onChange={(e) => {
                    setParticipantPerspectiveFocus(e.target.value);
                    markGoalAsStale();
                  }}
                  rows={4}
                  className="flex w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50 resize-none"
                  placeholder="例: 今感じている課題、現状と理想のギャップ"
                />
                <p className="text-xs text-muted-foreground">
                  現状で気になっている点や、目指したい未来像について参加者に聞いてみたいテーマがあれば教えてください。
                </p>
              </div>

              <div className="space-y-3">
                <span className="text-sm font-medium">
                  今回の話し合いで特に知りたいのはどれですか？
                </span>
                <p className="text-xs text-muted-foreground">
                  気になるものを1つ以上選んでください。迷っている場合はすべて選んでもOKです。
                </p>
                <div className="grid gap-3 sm:grid-cols-3">
                  {insightOptions.map((option) => {
                    const checked = insightTargets.includes(option.value);
                    return (
                      <label
                        key={option.value}
                        className="flex h-full cursor-pointer flex-col justify-between gap-2 rounded-lg border border-input bg-muted px-4 py-3 text-sm shadow-sm transition hover:border-primary/60"
                      >
                        <span className="flex items-center gap-2">
                          <input
                            type="checkbox"
                            value={option.value}
                            checked={checked}
                            onChange={() => handleInsightToggle(option.value)}
                            className="h-4 w-4 rounded border-input text-primary focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
                          />
                          <span className="font-medium">{option.label}</span>
                        </span>
                        <span className="text-xs text-muted-foreground">
                          {option.description}
                        </span>
                      </label>
                    );
                  })}
                </div>
                {goalError && insightTargets.length === 0 && (
                  <p className="text-xs text-destructive">
                    少なくとも1つは選択してください。
                  </p>
                )}
              </div>

              <div className="rounded-lg border border-dashed border-input/60 bg-muted/30 px-4 py-3">
                <p className="text-sm font-medium">
                  最後に、<span className="font-semibold">「なぜ今、考え方を整理したいのか」</span>を共有してください。
                </p>
                {/* <p className="text-xs text-muted-foreground">
                  このセッションでどうなっていると嬉しいかをイメージしながら書いてみましょう。
                </p> */}
              </div>

              <div className="space-y-2">
                <label htmlFor="decision" className="text-sm font-medium">
                  この整理を通じて、どんなことを決めたり、どんな状態を作れると嬉しいですか？
                </label>
                <textarea
                  id="decision"
                  value={decisionOutcome}
                  onChange={(e) => {
                    setDecisionOutcome(e.target.value);
                    markGoalAsStale();
                  }}
                  rows={4}
                  className="flex w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50 resize-none"
                  placeholder="例: 次の四半期に向けた開発の優先順位が決まる/議論すべき論点が明確になる"
                />
                <p className="text-xs text-muted-foreground">
                  会話を終えたあとに「ここまで決められたら安心」という状態を書いてください。モヤモヤを解消したいだけなら、その旨をぜひ書いてください。
                </p>
              </div>

              <div className="space-y-2">
                <label htmlFor="trigger" className="text-sm font-medium">
                  今このタイミングで参加者の認識を整理したいのは、どんな背景やモヤモヤがあるからですか？
                </label>
                <textarea
                  id="trigger"
                  value={discussionTrigger}
                  onChange={(e) => {
                    setDiscussionTrigger(e.target.value);
                    markGoalAsStale();
                  }}
                  rows={4}
                  className="flex w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50 resize-none"
                  placeholder="例: メンバー間で言うことがバラバラで作業が進んでいないため"
                />
                <p className="text-xs text-muted-foreground">
                  セッションのきっかけになった出来事や課題感を簡単に共有してください。
                </p>
              </div>

              <div className="flex justify-center py-4" aria-hidden="true">
                <ArrowDown className="h-7 w-7 text-muted-foreground" />
              </div>

              <div className="space-y-2">
                <div className="flex flex-col gap-2 sm:flex-row sm:items-center sm:justify-between">
                  <label htmlFor="sessionGoal" className="text-sm font-medium">
                    セッションゴール
                  </label>
                  <Button
                    type="button"
                    variant="outline"
                    onClick={handleGenerateGoal}
                    disabled={!canGenerateGoal && !isGoalGenerating}
                    className="w-full sm:w-auto gap-2"
                  >
                    {isGoalGenerating ? (
                      <>
                        <Loader2 className="h-4 w-4 animate-spin text-muted-foreground" />
                        生成中...
                        <span className="text-xs text-muted-foreground">
                          （クリックで停止）
                        </span>
                      </>
                    ) : (
                      <>
                        <Wand2 className="h-4 w-4" />
                        {hasGeneratedGoal ? "ゴールを再生成" : "ゴールを生成"}
                      </>
                    )}
                  </Button>
                </div>
                <p className="text-xs text-muted-foreground">
                  フォームの内容をもとに、AIが詳細なゴール説明を生成します。必要に応じて手動で編集できます。
                </p>
                <textarea
                  id="sessionGoal"
                  value={goal}
                  onChange={(e) => {
                    const value = e.target.value;
                    setGoal(value);
                    setGoalError(null);
                    if (isGoalStale) {
                      setIsGoalStale(false);
                    }
                    if (value.trim().length > 0) {
                      setHasGeneratedGoal(true);
                    } else {
                      setHasGeneratedGoal(false);
                    }
                  }}
                  rows={20}
                  className="flex w-full rounded-md border border-input bg-transparent px-3 py-2 text-sm shadow-sm placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50 resize-none"
                  placeholder="まずフォームを埋めてゴールを生成してください。"
                />
                {isGoalStale && (
                  <p className="text-xs text-amber-600">
                    フォームの内容が編集されたため、ゴールが古くなっている可能性があります。必要に応じて再生成してください。
                  </p>
                )}
                {goalError && (
                  <p className="text-xs text-destructive">{goalError}</p>
                )}
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
                disabled={isSubmitting || goal.trim().length === 0}
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
