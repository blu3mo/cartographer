import axios from "axios";

const OPENROUTER_API_URL = "https://openrouter.ai/api/v1/chat/completions";
const MODEL = "google/gemini-2.5-pro";

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

export async function callLLM(messages: LLMMessage[]): Promise<string> {
  const apiKey = process.env.OPENROUTER_API_KEY;
  if (!apiKey) {
    throw new Error("OPENROUTER_API_KEY is not set");
  }

  // Log input
  console.log("=== LLM Input ===");
  console.log("Model:", MODEL);
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
        model: MODEL,
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

interface ResponseWithStatement {
  statementText: string;
  value: number;
}

export async function generateIndividualReport(
  sessionTitle: string,
  context: string,
  responses: ResponseWithStatement[],
  userName: string,
): Promise<string> {
  const responsesText = responses
    .map((r) => {
      const valueLabel =
        r.value === 2
          ? "強く同意"
          : r.value === 1
            ? "同意"
            : r.value === 0
              ? "わからない"
              : r.value === -1
                ? "反対"
                : "強く反対";
      return `- "${r.statementText}" → ${valueLabel}`;
    })
    .join("\n");

  const prompt = `あなたは思慮深いコーチまたはカウンセラーです。

**セッションタイトル:**
${sessionTitle}

**セッションのコンテキスト:**
${context}

**参加者の名前:**
${userName}

**この参加者の回答履歴:**
${responsesText}

この参加者の回答パターンから、${userName}さんがこのテーマに対してどのような認識を持っているかを分析し、本人向けのフィードバックレポートをMarkdown形式で作成してください。

**レポート作成の指示:**
1. 特徴的な回答や、他の人と意見が異なりそうな点を優しく指摘してください
2. 自己理解を深める手助けをしてください
3. ポジティブで建設的なトーンを保ってください
4. Markdown形式で見やすく構造化してください

Markdownのみを出力し、他の説明文は含めないでください。`;

  const messages: LLMMessage[] = [{ role: "user", content: prompt }];

  const response = await callLLM(messages);
  return response.trim();
}
