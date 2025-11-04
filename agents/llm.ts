import axios from "axios";

type Role = "system" | "user" | "assistant";

interface LLMMessage {
  role: Role;
  content: string;
}

export type ParticipantReflectionInput = {
  text: string;
  name?: string;
  submittedAt?: string;
};

const OPENROUTER_API_URL = "https://openrouter.ai/api/v1/chat/completions";
const MODEL = "google/gemini-2.5-pro";

async function callLLM(
  messages: LLMMessage[],
  options?: { temperature?: number; reasoning_max_tokens?: number },
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

  const requestBody: {
    model: string;
    messages: LLMMessage[];
    temperature: number;
    reasoning?: { max_tokens: number };
  } = {
    model: MODEL,
    messages,
    temperature: options?.temperature ?? 0.7,
  };

  if (options?.reasoning_max_tokens !== undefined) {
    requestBody.reasoning = { max_tokens: options.reasoning_max_tokens };
  }

  const response = await axios.post(OPENROUTER_API_URL, requestBody, {
    headers: {
      Authorization: `Bearer ${apiKey}`,
      "Content-Type": "application/json",
      "HTTP-Referer": "https://cartographer.app",
      "X-Title": "Cartographer-Agent",
    },
    timeout: 45000,
  });

  const data = response.data;
  const choices = Array.isArray(data?.choices) ? data.choices : null;
  if (!choices || choices.length === 0) {
    console.error("[LLM] Unexpected response payload", data);
    throw new Error("LLM response was missing choices");
  }

  const choice = choices[0];
  const content = choice?.message?.content;
  if (typeof content !== "string") {
    console.error("[LLM] Unexpected choice message", choice);
    throw new Error("LLM response was missing message content");
  }

  console.log("[LLM] response length", content.length);
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
  sessionGoal: string;
  initialContext: string;
  eventThreadContext: string;
  latestAnalysisMarkdown?: string;
  recentUserMessages?: string[];
  participantCount?: number;
  participantReflections?: ParticipantReflectionInput[];
}): Promise<string> {
  const participantsLabel =
    typeof input.participantCount === "number"
      ? String(input.participantCount)
      : "unknown";
  const reflectionsSection =
    input.participantReflections && input.participantReflections.length > 0
      ? `<participant_reflections>
${input.participantReflections
  .map((reflection) => {
    const nameAttribute =
      reflection.name && reflection.name.length > 0
        ? ` name="${reflection.name}"`
        : "";
    const timestampAttribute =
      reflection.submittedAt && reflection.submittedAt.length > 0
        ? ` submitted_at="${reflection.submittedAt}"`
        : "";
    return `<reflection${nameAttribute}${timestampAttribute}>${reflection.text}</reflection>`;
  })
  .join("\n")}
</participant_reflections>`
      : "";

  const prompt = `
<role>
あなたはシニアリサーチャー兼コンサルタント。参加者への問いかけと分析や考察を繰り返しながら、認識の合意点・相違点・不明点を洗い出すことで、調査目的を達成します。
</role>
<task>
今までのEventThreadの内容を踏まえて、改めて、調査目的を満たすための道筋を大まかに描いた上で、まず今どんな認識を参加者全員から収集したいか具体的に記述してください。
- 参加者に対してこの後yes/noで答えられる質問を投げかけるので、それらの質問を通じてどんな情報を集めるべきか考察して方針を立ててください。（具体的な質問は作らなくてよいです）
- もし調査を進める上でそもそも前提情報が足りない場合は深掘りを急がずに、欠落している背景や前提の情報を探索的に収集することから始めてください。
- 個人の利害と、共同体としてのべき論を混同しないように注意してください。
- 具体レベルと、抽象レベルの両方の認識を必要に応じて収集してください。
</task>
<session>
  <title>${input.sessionTitle}</title>
  <goal>${input.sessionGoal}</goal>
  <participants>${participantsLabel}</participants>
</session>
<context>
  ${input.eventThreadContext}
  <initial_context>${input.initialContext}</initial_context>
  ${reflectionsSection}
</context>
<output>MarkdownのみでPLANセクションの中身を返してください。前置きなく、本文のみを生成してください。</output>
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
  sessionGoal: string;
  initialContext: string;
  eventThreadContext: string;
  planMarkdown?: string;
  latestAnalysisMarkdown?: string;
  participantCount?: number;
}): Promise<string[]> {
  const participantsLabel =
    typeof input.participantCount === "number"
      ? String(input.participantCount)
      : "unknown";

  const prompt = `
<role>
あなたはシニアリサーチャー兼コンサルタント。参加者への問いかけと分析や考察を繰り返しながら、認識の合意点・相違点・不明点を洗い出します。
</role>
<task>
ステートメントに対する全参加者のYES/NO回答を通じて、認識・解釈・価値観・利害・優先順位などを浮き彫りにし、収集したい認識の情報を収集します。
今までのEventThreadの内容を踏まえて、新たに15個のステートメントを生成してください。それらに対して参加者全員がYES/NOで回答します。
各ステートメントは以下を満たすこと。
- YES/NOの二択で答えられる断定文であること。
- 1文のみ、単体で意味が通じること。
- 表層の主張ではなく、その背後の価値観・利害・時間軸・成功条件を明らかにできること。
- 解釈のブレが生じないよう、必要であれば5W1Hを明示してシャープに表現すること。
- 参加者の立ち位置がYES/NOで鮮明に分かれ、背後の動機が推測できるようにする。
- 今後も質問を繰り返すので、今回だけで調査目的を達成する必要はない。深掘りを急がずに、まず今集めるべき情報を集めてほしい。
</task>
<session>
  <title>${input.sessionTitle}</title>
  <goal>${input.sessionGoal}</goal>
  <participants>${participantsLabel}</participants>
</session>
<context>
  ${input.eventThreadContext}
  <initial_context>${input.initialContext}</initial_context>
</context>
<output>JSON配列（例: ["文1", "文2", ...]）のみを返してください。</output>
`;

  try {
    const response = await callLLM([{ role: "user", content: prompt }], {
      reasoning_max_tokens: 1,
    });
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
  participantResponses: Array<{
    participantId: string;
    participantName: string;
    value: number;
  }>;
}

export async function generateSurveyAnalysisMarkdown(input: {
  sessionTitle: string;
  sessionGoal: string;
  initialContext: string;
  totalParticipants: number;
  statements: StatementStat[];
  eventThreadContext: string;
}): Promise<string> {
  const formatValue = (value: number) => {
    switch (value) {
      case 2:
        return "Strong Yes";
      case 1:
        return "Yes";
      case 0:
        return "わからない";
      case -1:
        return "No";
      case -2:
        return "Strong No";
      default:
        return `Unknown (${value})`;
    }
  };

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

  const participantMap = new Map<
    string,
    {
      name: string;
      responses: string[];
    }
  >();

  input.statements.forEach((statement, index) => {
    statement.participantResponses.forEach((response) => {
      const key =
        response.participantId && response.participantId !== "unknown"
          ? response.participantId
          : `unknown:${response.participantName}`;
      let entry = participantMap.get(key);
      if (!entry) {
        entry = { name: response.participantName, responses: [] };
        participantMap.set(key, entry);
      }
      entry.responses.push(
        `${index + 1}. "${statement.text}": ${formatValue(response.value)}`,
      );
    });
  });

  const participantDetailsText =
    participantMap.size > 0
      ? Array.from(participantMap.values())
          .sort((a, b) => a.name.localeCompare(b.name))
          .map((entry) => {
            const lines = entry.responses.map((response) => `  ${response}`);
            return `${entry.name}:\n${lines.join("\n")}`;
          })
          .join("\n\n")
      : "  (回答なし)";

  const surveyResultsText = `${statementsText}\n\n参加者別回答:\n${participantDetailsText}`;

  const prompt = `
<role>
あなたはシニアリサーチャー兼コンサルタント。参加者への問いかけと分析や考察を繰り返しながら、認識の合意点・相違点・不明点を洗い出しすことで目的を達成します。
</role>
<task>
Event Threadの履歴を踏まえつつ、提供された直近のSurvey結果から分かることを分析し、Markdownで以下を出力してください。
- 合意が存在する点。特に、具体的な合意点や意外な合意点。
- 意見が二極化・多極化している点。
- 多くがまだ判断できていない点、わからない点。
- 集団の傾向、クラスタなど（バイネームの分析）
また、それらからわかることを論理的に考察し、仮説を立てる。
</task>
<session>
  <title>${input.sessionTitle}</title>
  <goal>${input.sessionGoal}</goal>
  <participants>${input.totalParticipants}</participants>
</session>
<context>
  ${input.eventThreadContext}
  <initial_context>${input.initialContext}</initial_context>
</context>
<survey_results>
${surveyResultsText}
</survey_results>
<output>MarkdownのみでSurvey Analysisを返してください。前置きなく、本文のみを生成してください。</output>
`;

  try {
    const response = await callLLM([{ role: "user", content: prompt }]);
    return response.trim();
  } catch (error) {
    console.error("[LLM] Survey analysis generation failed:", error);
    throw new Error("Survey analysis generation failed");
  }
}
