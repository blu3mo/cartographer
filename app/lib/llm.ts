import axios from 'axios';

const OPENROUTER_API_URL = 'https://openrouter.ai/api/v1/chat/completions';
const MODEL = 'google/gemini-2.5-pro';

interface LLMMessage {
  role: 'system' | 'user' | 'assistant';
  content: string;
}

export async function callLLM(messages: LLMMessage[]): Promise<string> {
  const apiKey = process.env.OPENROUTER_API_KEY;
  if (!apiKey) {
    throw new Error('OPENROUTER_API_KEY is not set');
  }

  // Log input
  console.log('=== LLM Input ===');
  console.log('Model:', MODEL);
  messages.forEach((msg, index) => {
    console.log(`\n[Message ${index + 1}]`);
    console.log(`Role: ${msg.role}`);
    console.log(msg.content);
  });
  console.log('\n================\n');

  try {
    const response = await axios.post(
      OPENROUTER_API_URL,
      {
        model: MODEL,
        messages,
      },
      {
        headers: {
          'Authorization': `Bearer ${apiKey}`,
          'Content-Type': 'application/json',
          'HTTP-Referer': 'https://cartographer.app',
          'X-Title': 'Cartographer',
        },
        timeout: 30000, // 30 second timeout
      }
    );

    const output = response.data.choices[0].message.content;

    // Log output
    console.log('=== LLM Output ===');
    console.log(output);
    console.log('==================\n');

    return output;
  } catch (error) {
    if (axios.isAxiosError(error)) {
      console.error('LLM API Error:', {
        status: error.response?.status,
        statusText: error.response?.statusText,
        data: error.response?.data,
      });

      if (error.response?.status === 429) {
        throw new Error('Rate limit exceeded. Please try again in a moment.');
      }
    }
    console.error('LLM API Error:', error);
    throw new Error('Failed to call LLM API');
  }
}

// Fallback statements if LLM fails
const DEFAULT_STATEMENTS = [
  'Statement Generation Failed'
];

export async function generateInitialStatements(sessionTitle: string, context: string): Promise<string[]> {
  const prompt = `あなたは優れたファシリテータです。

以下のテーマと目的に基づき、参加者の視点を引き出すための、示唆に富む10個のステートメント（短い断定文）を生成してください。賛否をYES/NOで答えられる形式にしてください。YES/NOの回答を通じて、参加者の立場やその背後にある価値観や利害や優先順位がわかるステートメントが望ましいです。

セッションタイトル:
${sessionTitle}

テーマとコンテキスト:
${context}

JSON配列形式で、以下のように出力してください:
["ステートメント1", "ステートメント2", ...]

JSON配列のみを出力し、他の説明文は含めないでください。`;

  const messages: LLMMessage[] = [
    { role: 'user', content: prompt },
  ];

  try {
    const response = await callLLM(messages);

    // Extract JSON array from response
    const jsonMatch = response.match(/\[[\s\S]*\]/);
    if (!jsonMatch) {
      console.warn('Failed to parse LLM response, using default statements');
      return DEFAULT_STATEMENTS;
    }

    return JSON.parse(jsonMatch[0]);
  } catch (error) {
    console.error('Failed to generate statements with LLM, using defaults:', error);
    return DEFAULT_STATEMENTS;
  }
}

interface StatementWithResponses {
  text: string;
  responses: {
    strongYes: number;
    yes: number;
    dontKnow: number;
    no: number;
    strongNo: number;
    totalCount: number;
  };
  agreementScore?: number;
}

interface IndividualResponse {
  name: string;
  responses: Array<{
    statementText: string;
    value: number;
  }>;
}

export async function generateSituationAnalysisReport(
  sessionTitle: string,
  context: string,
  statements: StatementWithResponses[],
  totalParticipants: number,
  individualResponses: IndividualResponse[] | null = null
): Promise<string> {
  // Format statements for the prompt (already sorted by agreement score)
  // Filter out statements with 0 responses
  const statementsWithResponses = statements.filter(s => s.responses.totalCount > 0);

  const statementsText = statementsWithResponses
    .map((s, i) => {
      return `${i + 1}. "${s.text}" (回答人数: ${s.responses.totalCount}人)
   - Strong Yes: ${s.responses.strongYes}%
   - Yes: ${s.responses.yes}%
   - わからない: ${s.responses.dontKnow}%
   - No: ${s.responses.no}%
   - Strong No: ${s.responses.strongNo}%`;
    })
    .join('\n\n');

  // Format individual responses if applicable
  let individualResponsesText = '';
  if (individualResponses && individualResponses.length > 0) {
    individualResponsesText = '\n\n**個別回答データ（参加者ごと）:**\n\n';
    individualResponses.forEach((participant) => {
      individualResponsesText += `**${participant.name}:**\n`;
      participant.responses.forEach((r) => {
        const valueLabel =
          r.value === 2 ? 'Strong Yes' :
            r.value === 1 ? 'Yes' :
              r.value === 0 ? 'わからない' :
                r.value === -1 ? 'No' :
                  'Strong No';
        individualResponsesText += `- "${r.statementText}" → ${valueLabel}\n`;
      });
      individualResponsesText += '\n';
    });
  }

  const prompt = `あなたは鋭い洞察力を持つ組織コンサルタントまたは社会調査アナリストです。

以下の情報に基づき、現状の分析レポートをMarkdown形式で作成してください。

**セッションタイトル:**
${sessionTitle}

**セッションのコンテキスト:**
${context}

**総回答者数:** ${totalParticipants}人

**ステートメントと回答状況（合意度の高い順）:**
${statementsText}${individualResponsesText}

**レポート作成の指示:**
1.「合意が形成されている点」「意見が対立している点」「多くの人がまだ分かっていない点」を明確に指摘してください。もし特に意外な合意点があれば指摘してください。
2. 目的を達成するために、次に参加者群に対する調査によって「明らかにすべき不明点」「検証すべき仮説」を提案してください。本質的に掘るべき点を厳選して挙げてください。
3. データに基づいた客観的な分析と、深い考察や議論を行ってください
4. Markdown形式で見やすく構造化してください（見出し、箇条書きなどを活用）
${individualResponses ? '\n5. 個別回答データも提供されているので、それらから分かる「似た考え方を持つグループ」や「対立する考え方の軸」があれば言及してください' : ''}

Markdownのみを出力し、他の説明文は含めないでください。`;

  const messages: LLMMessage[] = [
    { role: 'user', content: prompt },
  ];

  const response = await callLLM(messages);
  return response.trim();
}

export async function generateNewStatements(
  sessionTitle: string,
  context: string,
  existingStatements: StatementWithResponses[],
  latestReport?: string
): Promise<string[]> {
  const statementsText = existingStatements
    .map((s, i) => {
      const total = s.responses.totalCount;
      return `${i + 1}. "${s.text}" (回答者数: ${total}人)`;
    })
    .join('\n');

  const reportSection = latestReport
    ? `\n**最新の現状分析レポート:**\n${latestReport}\n`
    : '';

  const prompt = `あなたは次の議論をデザインする戦略的なリサーチャーです。

**セッションタイトル:**
${sessionTitle}

**セッションのコンテキスト:**
${context}

**既存のステートメント一覧:**
${statementsText}
${reportSection}
既存の回答状況と分析レポートを踏まえ、議論をさらに深めるための新たなステートメントを5つ生成してください。
特に、意見が割れている点や、まだ誰も分かっていない点を掘り下げるような、新たな仮説を検証するための問いを設計してください。

JSON配列形式で、以下のように出力してください:
["ステートメント1", "ステートメント2", ...]

JSON配列のみを出力し、他の説明文は含めないでください。`;

  const messages: LLMMessage[] = [
    { role: 'user', content: prompt },
  ];

  try {
    const response = await callLLM(messages);

    // Extract JSON array from response
    const jsonMatch = response.match(/\[[\s\S]*\]/);
    if (!jsonMatch) {
      console.warn('Failed to parse LLM response for new statements');
      return DEFAULT_STATEMENTS;
    }

    return JSON.parse(jsonMatch[0]);
  } catch (error) {
    console.error('Failed to generate new statements with LLM:', error);
    return DEFAULT_STATEMENTS;
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
  userName: string
): Promise<string> {
  const responsesText = responses
    .map((r) => {
      const valueLabel =
        r.value === 2 ? 'Strong Yes' :
          r.value === 1 ? 'Yes' :
            r.value === 0 ? 'わからない' :
              r.value === -1 ? 'No' :
                'Strong No';
      return `- "${r.statementText}" → ${valueLabel}`;
    })
    .join('\n');

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

  const messages: LLMMessage[] = [
    { role: 'user', content: prompt },
  ];

  const response = await callLLM(messages);
  return response.trim();
}
