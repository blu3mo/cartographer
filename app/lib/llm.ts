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

export async function generateInitialStatements(context: string): Promise<string[]> {
  const prompt = `あなたは優れたファシリテーターです。

以下のテーマと目的に基づき、参加者の多様な視点を引き出すための、示唆に富む10個のステートメント（短い断定文）を生成してください。
YES/NOで答えやすい形式にしてください。

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
}

export async function generateSituationAnalysisReport(
  context: string,
  statements: StatementWithResponses[]
): Promise<string> {
  // Format statements for the prompt
  const statementsText = statements
    .map((s, i) => {
      const total = s.responses.totalCount;
      return `${i + 1}. "${s.text}"
   - Strong Yes: ${s.responses.strongYes.toFixed(1)}% (${Math.round((s.responses.strongYes / 100) * total)}人)
   - Yes: ${s.responses.yes.toFixed(1)}% (${Math.round((s.responses.yes / 100) * total)}人)
   - わからない: ${s.responses.dontKnow.toFixed(1)}% (${Math.round((s.responses.dontKnow / 100) * total)}人)
   - No: ${s.responses.no.toFixed(1)}% (${Math.round((s.responses.no / 100) * total)}人)
   - Strong No: ${s.responses.strongNo.toFixed(1)}% (${Math.round((s.responses.strongNo / 100) * total)}人)
   - 回答者数: ${total}人`;
    })
    .join('\n\n');

  const prompt = `あなたは鋭い洞察力を持つ組織コンサルタントまたは社会調査アナリストです。

以下の情報に基づき、現状の分析レポートをMarkdown形式で作成してください。

**セッションのコンテキスト:**
${context}

**ステートメントと回答状況:**
${statementsText}

**レポート作成の指示:**
1. 特に「合意が形成されている点」「意見が対立している点」「多くの人がまだ分かっていない、確信が持てない点」を明確に指摘してください
2. 目的に対して次に行うべきアクションや、検証すべき仮説を提案してください
3. データに基づいた客観的な分析を行ってください
4. Markdown形式で見やすく構造化してください（見出し、箇条書きなどを活用）

Markdownのみを出力し、他の説明文は含めないでください。`;

  const messages: LLMMessage[] = [
    { role: 'user', content: prompt },
  ];

  const response = await callLLM(messages);
  return response.trim();
}

export async function generateNewStatements(
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
  context: string,
  responses: ResponseWithStatement[]
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

**セッションのコンテキスト:**
${context}

**この参加者の回答履歴:**
${responsesText}

この参加者の回答パターンから、彼/彼女がこのテーマに対してどのような認識を持っているかを分析し、本人向けのフィードバックレポートをMarkdown形式で作成してください。

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
