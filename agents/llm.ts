import axios from "axios";

type Role = "system" | "user" | "assistant";

interface LLMMessage {
  role: Role;
  content: string;
}

const OPENROUTER_API_URL = "https://openrouter.ai/api/v1/chat/completions";
const MODEL = "google/gemini-2.5-pro";

async function callLLM(
  messages: LLMMessage[],
  options?: { temperature?: number },
): Promise<string> {
  const apiKey = process.env.OPENROUTER_API_KEY;
  if (!apiKey) {
    throw new Error("OPENROUTER_API_KEY is not set");
  }

  console.log("[LLM] request", {
    model: MODEL,
    messageCount: messages.length,
  });
  messages.forEach((message, index) => {
    console.log(`[LLM] message #${index + 1} (${message.role})`);
    console.log(message.content);
  });

  const response = await axios.post(
    OPENROUTER_API_URL,
    {
      model: MODEL,
      messages,
      temperature: options?.temperature ?? 0.7,
    },
    {
      headers: {
        Authorization: `Bearer ${apiKey}`,
        "Content-Type": "application/json",
        "HTTP-Referer": "https://cartographer.app",
        "X-Title": "Cartographer-Agent",
      },
      timeout: 45000,
    },
  );

  const content = response.data.choices[0].message.content;
  console.log("[LLM] response length", content?.length ?? 0);
  return content;
}

function extractJsonArray(text: string): string[] | null {
  const match = text.match(/\[[\s\S]*\]/);
  if (!match) {
    return null;
  }

  try {
    const parsed = JSON.parse(match[0]);
    if (
      Array.isArray(parsed) &&
      parsed.every((item) => typeof item === "string")
    ) {
      return parsed;
    }
    return null;
  } catch {
    return null;
  }
}

export async function generatePlanMarkdown(input: {
  sessionTitle: string;
  context: string;
  latestAnalysisMarkdown?: string;
  recentUserMessages?: string[];
}): Promise<string> {
  const messagesSection =
    input.recentUserMessages && input.recentUserMessages.length > 0
      ? `\n**最新のUser Message:**\n${input.recentUserMessages
        .map((message, index) => `${index + 1}. ${message}`)
        .join("\n")}\n`
      : "";

  const analysisSection = input.latestAnalysisMarkdown
    ? `\n**最新のSurvey Analysis:**\n${input.latestAnalysisMarkdown}\n`
    : "";

  const prompt = `あなたは組織内のリサーチを指揮するコンサルタントです。

以下の情報を基に、「ここからどのように調査を進めていくか」「次に何をすべきか」をMarkdown形式で簡潔に整理してください。
- 目的達成に向けた長期的なリサーチの道筋を立てる。
- 次のSurveyで調査したい問いや仮説、観点を箇条書きで示す
- Agentやホストが参照すべき具体的な実行ステップを示す
過去の洞察からの学びも活かしてください。

**セッションタイトル:** ${input.sessionTitle}

**コンテキスト:**
${input.context}
${messagesSection}${analysisSection}
Markdownのみを出力してください。`;

  try {
    const response = await callLLM([{ role: "user", content: prompt }]);
    return response.trim();
  } catch (error) {
    console.error("[LLM] Plan generation failed:", error);
    throw new Error("Plan generation failed");
  }
}

export async function generateSurveyStatements(input: {
  sessionTitle: string;
  context: string;
  planMarkdown?: string;
  latestAnalysisMarkdown?: string;
}): Promise<string[]> {
  const planSection = input.planMarkdown
    ? `\n**最新のPlan:**\n${input.planMarkdown}\n`
    : "";
  const analysisSection = input.latestAnalysisMarkdown
    ? `\n**最新のSurvey Analysis:**\n${input.latestAnalysisMarkdown}\n`
    : "";

  const prompt = `あなたは探求的リサーチを設計するアナリストです。

以下の情報をもとに、YES/NOで回答できる挑戦的なステートメントを10個、JSON配列形式で生成してください。
- 参加者の立場や利害が分かれそうなポイントを捉える
- 未解決の問い、重要だが議論されていない視点を掘り起こす
- 1文で完結し、曖昧さが少ない断定形にする

**セッションタイトル:** ${input.sessionTitle}

**コンテキスト:**
${input.context}
${planSection}${analysisSection}
["ステートメント1", "ステートメント2", ...] というJSON配列のみを出力してください。`;

  try {
    const response = await callLLM([{ role: "user", content: prompt }]);
    const parsed = extractJsonArray(response);
    if (!parsed) {
      throw new Error("LLM response was not valid JSON array");
    }
    return parsed;
  } catch (error) {
    console.error("[LLM] Survey statement generation failed:", error);
    throw new Error("Survey statement generation failed");
  }
}

export interface StatementStat {
  text: string;
  totalCount: number;
  distribution: {
    strongYes: number;
    yes: number;
    dontKnow: number;
    no: number;
    strongNo: number;
  };
}

export async function generateSurveyAnalysisMarkdown(input: {
  sessionTitle: string;
  context: string;
  totalParticipants: number;
  statements: StatementStat[];
}): Promise<string> {
  const statementsText = input.statements
    .map((statement, index) => {
      const dist = statement.distribution;
      return `${index + 1}. "${statement.text}" (回答人数: ${statement.totalCount}人)
- Strong Yes: ${dist.strongYes}%
- Yes: ${dist.yes}%
- わからない: ${dist.dontKnow}%
- No: ${dist.no}%
- Strong No: ${dist.strongNo}%`;
    })
    .join("\n\n");

  const prompt = `あなたは組織に寄り添うリサーチャーです。

以下の回答状況をもとに、Markdown形式のSurvey Analysisを作成してください。
- 合意が形成されている点 / 意見が割れている点 / まだ不明な点をそれぞれ指摘する
- 客観的なデータに基づきつつ、示唆に富んだ解説を加える

**セッションタイトル:** ${input.sessionTitle}

**コンテキスト:**
${input.context}

**総回答者数:** ${input.totalParticipants}人

**ステートメントと回答状況:**
${statementsText}

Markdownのみを出力してください。`;

  try {
    const response = await callLLM([{ role: "user", content: prompt }]);
    return response.trim();
  } catch (error) {
    console.error("[LLM] Survey analysis generation failed:", error);
    throw new Error("Survey analysis generation failed");
  }
}
