import { type NextRequest, NextResponse } from "next/server";

import { getUserIdFromRequest } from "@/lib/auth";
import { callLLM } from "@/lib/llm";
import { supabase } from "@/lib/supabase";

type StatementRow = {
  text: string;
};

type PastResponse = {
  statement_id: string;
  response_type: "scale" | "free_text";
  value: number | null;
  text_response: string | null;
  statements?:
    | { text?: string | null }
    | Array<{ text?: string | null }>
    | null;
};

export async function GET(
  request: NextRequest,
  { params }: { params: Promise<{ sessionId: string; statementId: string }> },
) {
  try {
    const { sessionId, statementId } = await params;
    const userId = getUserIdFromRequest(request);

    if (!userId) {
      return NextResponse.json(
        { error: "Unauthorized: Missing user ID" },
        { status: 401 },
      );
    }

    // Verify participant
    const { data: participant, error: participantError } = await supabase
      .from("participants")
      .select("user_id")
      .eq("user_id", userId)
      .eq("session_id", sessionId)
      .maybeSingle();

    if (participantError) {
      console.error("Failed to verify participant:", participantError);
      return NextResponse.json(
        { error: "Internal server error" },
        { status: 500 },
      );
    }

    if (!participant) {
      return NextResponse.json(
        { error: "Unauthorized: Not a participant in this session" },
        { status: 401 },
      );
    }

    // Get current statement text
    const { data: statement, error: statementError } = await supabase
      .from("statements")
      .select("text")
      .eq("id", statementId)
      .eq("session_id", sessionId)
      .maybeSingle();

    if (statementError) {
      console.error("Failed to load statement:", statementError);
      return NextResponse.json(
        { error: "Internal server error" },
        { status: 500 },
      );
    }

    if (!statement) {
      return NextResponse.json(
        { error: "Statement not found" },
        { status: 404 },
      );
    }

    // Get user's past responses in this session
    const { data: responses, error: responsesError } = await supabase
      .from("responses")
      .select(
        `
        statement_id,
        response_type,
        value,
        text_response,
        statements!inner(text)
      `,
      )
      .eq("participant_user_id", userId)
      .eq("session_id", sessionId)
      .order("created_at", { ascending: false })
      .limit(10); // Get last 10 responses for context

    if (responsesError) {
      console.error("Failed to load responses:", responsesError);
      return NextResponse.json(
        { error: "Internal server error" },
        { status: 500 },
      );
    }

    // Generate AI suggestions using LLM
    const suggestions = await generateSuggestions(
      (statement as StatementRow).text,
      responses ?? [],
    );

    return NextResponse.json({ suggestions });
  } catch (error) {
    console.error("Get suggestions error:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}

async function generateSuggestions(
  currentStatementText: string,
  pastResponses: PastResponse[],
): Promise<string[]> {
  // Build context from past responses
  let contextText = "";
  if (pastResponses.length > 0) {
    const responsesText = pastResponses
      .map((r) => {
        const statementText = Array.isArray(r.statements)
          ? (r.statements[0]?.text ?? "不明な質問")
          : (r.statements?.text ?? "不明な質問");
        if (r.response_type === "free_text") {
          const text = (r.text_response ?? "").trim();
          const content = text.length > 0 ? text : "（未入力）";
          return `- 「${statementText}」 → [自由記述] ${content}`;
        }

        const valueLabel =
          r.value === 2
            ? "Strong Yes"
            : r.value === 1
              ? "Yes"
              : r.value === 0
                ? "わからない"
                : r.value === -1
                  ? "No"
                  : "Strong No";
        return `- 「${statementText}」 → ${valueLabel}`;
      })
      .join("\n");

    contextText = `\n\n**過去の回答履歴:**\n${responsesText}`;
  } else {
    contextText =
      "\n\n**過去の回答履歴:**\nまだ回答はありません。これが最初の質問です。";
  }

  const prompt = `あなたはユーザーの回答を支援するアシスタントです。

**現在の質問:**
「${currentStatementText}」
${contextText}

この質問に対して、ユーザーが「YesでもNoでもない」と感じた場合に選択できる、具体的で多様な回答の選択肢を3つ生成してください。

**選択肢の生成ルール:**
1. それぞれ40-50文字以内の文章にしてください
2. ユーザーの過去の回答を踏まえて、YesでもNoでもない場合に考えていることを推測して候補を作ってください。（e.g. そもそも前提が間違っている、条件によって答えが変わる、などのパターンがありえる）
3. パッと読んでスタンスが理解できるような、シンプルで読みやすい文章にしてください。

**出力形式:**
3つの選択肢のみを、1行につき1つずつ出力してください。番号や記号は不要です。

では、3つの選択肢を生成してください:`;

  const messages = [{ role: "user" as const, content: prompt }];

  try {
    const response = await callLLM(messages, "anthropic/claude-sonnet-4.5");
    const suggestions = response
      .split("\n")
      .map((line) => line.trim())
      .filter((line) => line.length > 0 && line.length <= 50) // Max 50 chars for safety
      .slice(0, 3); // Ensure exactly 3 suggestions

    // Fallback if LLM doesn't return valid suggestions
    if (suggestions.length < 3) {
      return [
        "状況によって賛成できる",
        "一部には賛成だが全体には反対",
        "今は判断できない",
      ];
    }

    return suggestions;
  } catch (error) {
    console.error("Failed to generate suggestions:", error);
    // Return fallback suggestions
    return [
      "状況によって賛成できる",
      "一部には賛成だが全体には反対",
      "今は判断できない",
    ];
  }
}
