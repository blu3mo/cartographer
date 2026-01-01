export const dynamic = "force-dynamic";

import { type NextRequest, NextResponse } from "next/server";

import { getUserIdFromRequest } from "@/lib/auth";
import { callLLMStreaming } from "@/lib/llm";
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

    // Generate AI suggestions using LLM streaming
    const encoder = new TextEncoder();
    const stream = new ReadableStream({
      async start(controller) {
        try {
          await generateSuggestionsStreaming(
            (statement as StatementRow).text,
            responses ?? [],
            (suggestion: string) => {
              controller.enqueue(
                encoder.encode(`data: ${JSON.stringify({ suggestion })}\n\n`),
              );
            },
          );
          controller.enqueue(encoder.encode("data: [DONE]\n\n"));
          controller.close();
        } catch (error) {
          console.error("Streaming error:", error);
          controller.error(error);
        }
      },
    });

    return new Response(stream, {
      headers: {
        "Content-Type": "text/event-stream",
        "Cache-Control": "no-cache",
        Connection: "keep-alive",
      },
    });
  } catch (error) {
    console.error("Get suggestions error:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}

async function generateSuggestionsStreaming(
  currentStatementText: string,
  pastResponses: PastResponse[],
  onSuggestion: (suggestion: string) => void,
): Promise<void> {
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
                ? "わからない・自信がない"
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
    let fullText = "";

    // Use Gemini 3 Flash for faster response times
    await callLLMStreaming(
      messages,
      (chunk: string) => {
        fullText += chunk;

        // Check if we have complete lines
        const lines = fullText.split("\n");
        const completeLines = lines.slice(0, -1); // All but the last (potentially incomplete) line
        fullText = lines[lines.length - 1] || ""; // Keep the incomplete line

        // Send each complete line as a suggestion
        for (const line of completeLines) {
          const trimmed = line.trim();
          if (trimmed.length > 0 && trimmed.length <= 50) {
            onSuggestion(trimmed);
          }
        }
      },
      "google/gemini-3-flash-preview",
    );

    // Handle any remaining text
    if (fullText.trim().length > 0 && fullText.trim().length <= 50) {
      onSuggestion(fullText.trim());
    }
  } catch (error) {
    console.error("Failed to generate suggestions:", error);
    // Return fallback suggestions
    onSuggestion("状況によって賛成できる");
    onSuggestion("一部には賛成だが全体には反対");
    onSuggestion("今は判断できない");
  }
}
