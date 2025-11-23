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
          ? "Strong Yes"
          : r.value === 1
            ? "Yes"
            : r.value === 0
              ? "わからない"
              : r.value === -1
                ? "No"
                : "Strong No";
      return `- "${r.statementText}" → ${valueLabel}`;
    })
    .join("\n");

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
3. ポジティブで建設的なトーンを保ってください
4. Markdown形式で見やすく構造化してください

Markdownのみを出力し、他の説明文は含めないでください。`;

  const messages: LLMMessage[] = [{ role: "user", content: prompt }];

  const response = await callLLM(messages);
  return response.trim();
}
