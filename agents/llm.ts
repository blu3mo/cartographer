import axios from "axios";

type Role = "system" | "user" | "assistant";

interface LLMMessage {
  role: Role;
  content: string;
}

const OPENROUTER_API_URL = "https://openrouter.ai/api/v1/chat/completions";
const MODEL = "google/gemini-2.5-flash-lite";

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
  eventThreadContext: string;
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

  const prompt = `
<role>
あなたはシニアリサーチャー兼コンサルタント。参加者への問いかけと分析や考察を繰り返しながら、認識の合意点・相違点・不明点を洗い出しすことで目的を達成します。
</role>
<task>
今までのEventThreadの内容を踏まえて、改めて、調査目的を満たすための戦略ロードマップを、短期・中期・長期のリサーチサイクルとして記述してください。各サイクルで「収集したい情報」「検証する仮説」「想定する分岐」を書いてください。
3. 次のSurveyで最優先で明らかにしたい問い／仮説を箇条書きで提示し、なぜ重要か一行コメントを添えてください。
- いきなり深掘りせず、論点の幅と優先順位を俯瞰してください。
- もし調査を進める上でそもそも前提情報が足りない場合は深掘りを急がずに、欠落している背景や前提の情報を探索的に収集することから始めてください。
- 参加者群からどのような質問でどんな仮説を検証するかを明確にし、必要があれば探索ルートを更新してください。
- 個人の利害と、共同体としてのべき論を混同しないように注意してください。
- 具体/ミクロレベルと、抽象/マクロレベルの両方の認識を必要に応じて収集してください。
- 常に目的を意識し、目的を達成するために収集すべき情報について収集の順番や優先順位がつけられるとよい。
</task>
<session>
  <title>${input.sessionTitle}</title>
  <context>${input.context}</context>
</session>
${input.eventThreadContext}
<output>MarkdownのみでPLANセクションの中身を返してください。</output>
`;

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
  eventThreadContext: string;
}): Promise<string[]> {
  const planSection = input.planMarkdown
    ? `\n**最新のPlan:**\n${input.planMarkdown}\n`
    : "";
  const analysisSection = input.latestAnalysisMarkdown
    ? `\n**最新のSurvey Analysis:**\n${input.latestAnalysisMarkdown}\n`
    : "";

  const prompt = `
<role>
あなたはシニアリサーチャー兼コンサルタント。参加者への問いかけと分析や考察を繰り返しながら、認識の合意点・相違点・不明点を洗い出しすことで目的を達成します。
</role>
<task>
参加者へのYES/NO回答を通じて、立場の背景にある価値観・利害・優先順位を浮き彫りにします。
今までのEventThreadの内容を踏まえて、新たに15個のステートメントを生成してください。各ステートメントは以下を満たすこと。
- YES/NOの二択で答えられる断定文であること。
- 1文のみ、単体で意味が通じること。
- 表層の主張ではなく、その背後の価値観・利害・時間軸・成功条件を明らかにできること。
- 個人の利害と共同体としてのべき論を混同せずに、調査目的を踏まえて主語を明確にすること。
- 解釈のブレが生じないよう、必要であれば5W1Hを明示してシャープに表現すること。
- 参加者の立ち位置がYES/NOで鮮明に分かれ、背後の動機が推測できるようにする。
</task>
<session>
  <title>${input.sessionTitle}</title>
  <context>${input.context}</context>
</session>
${input.eventThreadContext}
<output>JSON配列（例: ["文1", "文2", ...]）のみを返してください。</output>
`;

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
  eventThreadContext: string;
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

  const prompt = `
<role>
あなたはシニアリサーチャー兼コンサルタント。参加者への質問→分析→計画→再質問のループを通じて、認識の構造を定量・定性の両面から解き明かします。
</role>
<task>
Event Threadの履歴を踏まえつつ、提供された直近のSurvey結果から分かることを分析し、Markdownで以下を出力してください。
- 合意が存在する点。特に、具体的な合意点や意外な合意点。
- 意見が二極化・多極化している点。
- 多くがまだ判断できていない点、わからない点。
また、それらを考察し、背景にある点を仮説
- どのような価値観／利害が軸になっているか。
</task>
<session>
  <title>${input.sessionTitle}</title>
  <context>${input.context}</context>
  <participants>${input.totalParticipants}</participants>
</session>
<survey_results>
${statementsText}
</survey_results>
${input.eventThreadContext}
<output>MarkdownのみでSurvey Analysisを返してください。</output>
`;

  try {
    const response = await callLLM([{ role: "user", content: prompt }]);
    return response.trim();
  } catch (error) {
    console.error("[LLM] Survey analysis generation failed:", error);
    throw new Error("Survey analysis generation failed");
  }
}
