import axios from 'axios';

const OPENROUTER_API_URL = 'https://openrouter.ai/api/v1/chat/completions';
const MODEL = 'google/gemini-2.0-flash-exp:free';

interface LLMMessage {
  role: 'system' | 'user' | 'assistant';
  content: string;
}

export async function callLLM(messages: LLMMessage[]): Promise<string> {
  const apiKey = process.env.OPENROUTER_API_KEY;
  if (!apiKey) {
    throw new Error('OPENROUTER_API_KEY is not set');
  }

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

    return response.data.choices[0].message.content;
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
  '現在の取り組みは正しい方向に進んでいる',
  '目標が明確に定義されている',
  'チーム内のコミュニケーションは十分である',
  '必要なリソースが揃っている',
  '優先順位が適切に設定されている',
  '全員が共通の理解を持っている',
  '現状に満足している',
  '変更が必要だと感じている',
  '今後の見通しは明るい',
  '改善の余地がある',
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
