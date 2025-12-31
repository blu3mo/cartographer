import axios from "axios";

const OPENROUTER_API_URL = "https://openrouter.ai/api/v1/chat/completions";
const MODEL = "google/gemini-3-flash-preview";

interface LLMMessage {
  role: "system" | "user" | "assistant";
  content: string;
}

export function buildSessionBrief(
  goal?: string | null,
  context?: string | null,
): string {
  const sections: string[] = [];
  const trimmedGoal = goal?.trim();
  const trimmedContext = context?.trim();

  if (trimmedGoal) {
    sections.push(`## Goal\n${trimmedGoal}`);
  }

  if (trimmedContext) {
    sections.push(`## Background\n${trimmedContext}`);
  }

  if (sections.length === 0) {
    return "## Goal\n(未設定)";
  }

  return sections.join("\n\n");
}

export async function callLLM(
  messages: LLMMessage[],
  model: string = MODEL,
): Promise<string> {
  const apiKey = process.env.OPENROUTER_API_KEY;
  if (!apiKey) {
    throw new Error("OPENROUTER_API_KEY is not set");
  }

  // Log input
  console.log("=== LLM Input ===");
  console.log("Model:", model);
  messages.forEach((msg, index) => {
    console.log(`\n[Message ${index + 1}]`);
    console.log(`Role: ${msg.role}`);
    console.log(msg.content);
  });
  console.log("\n================\n");

  try {
    const response = await axios.post(
      OPENROUTER_API_URL,
      {
        model,
        messages,
      },
      {
        headers: {
          Authorization: `Bearer ${apiKey}`,
          "Content-Type": "application/json",
          "HTTP-Referer": "https://cartographer.app",
          "X-Title": "Cartographer",
        },
        timeout: 300000, // 300 second timeout
      },
    );

    const output = response.data.choices[0].message.content;

    // Log output
    console.log("=== LLM Output ===");
    console.log(output);
    console.log("==================\n");

    return output;
  } catch (error) {
    if (axios.isAxiosError(error)) {
      console.error("LLM API Error:", {
        status: error.response?.status,
        statusText: error.response?.statusText,
        data: error.response?.data,
      });

      if (error.response?.status === 429) {
        throw new Error("Rate limit exceeded. Please try again in a moment.");
      }
    }
    console.error("LLM API Error:", error);
    throw new Error("Failed to call LLM API");
  }
}

export async function callLLMStreaming(
  messages: LLMMessage[],
  onChunk: (chunk: string) => void,
  model: string = MODEL,
): Promise<void> {
  const apiKey = process.env.OPENROUTER_API_KEY;
  if (!apiKey) {
    throw new Error("OPENROUTER_API_KEY is not set");
  }

  // Log input
  console.log("=== LLM Streaming Input ===");
  console.log("Model:", model);
  messages.forEach((msg, index) => {
    console.log(`\n[Message ${index + 1}]`);
    console.log(`Role: ${msg.role}`);
    console.log(msg.content);
  });
  console.log("\n================\n");

  try {
    const response = await fetch(OPENROUTER_API_URL, {
      method: "POST",
      headers: {
        Authorization: `Bearer ${apiKey}`,
        "Content-Type": "application/json",
        "HTTP-Referer": "https://cartographer.app",
        "X-Title": "Cartographer",
      },
      body: JSON.stringify({
        model,
        messages,
        stream: true,
      }),
    });

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
          if (data === "[DONE]") continue;

          try {
            const parsed = JSON.parse(data);
            const content = parsed.choices?.[0]?.delta?.content;
            if (content) {
              onChunk(content);
            }
          } catch (e) {
            // Skip invalid JSON
            console.warn("Failed to parse streaming chunk:", e);
          }
        }
      }
    }

    console.log("=== LLM Streaming Complete ===\n");
  } catch (error) {
    console.error("LLM Streaming Error:", error);
    throw new Error("Failed to call LLM streaming API");
  }
}

type IndividualReportResponse = {
  statementText: string;
  responseType: "scale" | "free_text";
  value: number | null;
  textResponse?: string;
};

export async function generateIndividualReport(input: {
  sessionTitle: string;
  context: string;
  responses: IndividualReportResponse[];
  userName: string;
  taste?: string;
}): Promise<string> {
  const responsesText = input.responses
    .map((r) => {
      if (r.responseType === "free_text") {
        const text = (r.textResponse ?? "").trim();
        const content = text.length > 0 ? text : "（自由記述：未入力）";
        return `- "${r.statementText}" → [自由記述] ${content}`;
      }

      const valueLabel =
        r.value === 2
          ? "強く同意"
          : r.value === 1
            ? "同意"
            : r.value === 0
              ? "わからない・自信がない"
              : r.value === -1
                ? "反対"
                : "強く反対";
      return `- "${r.statementText}" → ${valueLabel}`;
    })
    .join("\n");

  // Define tone based on taste
  const toneInstructions: Record<string, string> = {
    neutral: "客観的で中立的なトーンで、バランスの取れた分析を提供してください。",
    encouraging:
      "励ましとポジティブなフィードバックを重視し、建設的で前向きな表現を使ってください。",
    analytical:
      "詳細で論理的な分析を提供し、深い洞察と具体的な観察を含めてください。",
    casual:
      "親しみやすくフレンドリーなトーンで、くだけた表現を使いながらも敬意を保ってください。",
  };

  const toneInstruction =
    toneInstructions[input.taste || "neutral"] || toneInstructions.neutral;

  const prompt = `あなたは思慮深いコーチまたはカウンセラーです。

**セッションタイトル:**
${input.sessionTitle}

**セッションのコンテキスト:**
${input.context}

**参加者の名前:**
${input.userName}

**この参加者の回答履歴:**
${responsesText}

この参加者の回答パターンから、${input.userName}さんがこのテーマに対してどのような認識を持っているかを分析し、本人向けのフィードバックレポートをMarkdown形式で作成してください。

**レポート作成の指示:**
1. 特徴的な回答や、他の人と意見が異なりそうな点を優しく指摘してください
2. 自己理解を深める手助けをしてください
3. ${toneInstruction}
4. Markdown形式で見やすく構造化してください

Markdownのみを出力し、他の説明文は含めないでください。`;

  const messages: LLMMessage[] = [{ role: "user", content: prompt }];

  const response = await callLLM(messages);
  return response.trim();
}
