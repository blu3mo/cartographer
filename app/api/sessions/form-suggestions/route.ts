import axios from "axios";
import { type NextRequest, NextResponse } from "next/server";

const OPENROUTER_API_URL = "https://openrouter.ai/api/v1/chat/completions";
const MODEL = "google/gemini-3-flash-preview";

interface FormSuggestionRequest {
  backgroundInfo: string;
  purpose: string;
}

export async function POST(request: NextRequest) {
  try {
    const body: FormSuggestionRequest = await request.json();
    const { backgroundInfo, purpose } = body;

    if (!backgroundInfo.trim() && !purpose.trim()) {
      return NextResponse.json({ suggestions: [] });
    }

    const apiKey = process.env.OPENROUTER_API_KEY;
    if (!apiKey) {
      throw new Error("OPENROUTER_API_KEY is not set");
    }

    const prompt = `あなたは優しく親切なファシリテーターです。ユーザーが認識を洗い出すセッションを作成しようとしています。以下のフォーム入力内容を見て、情報が不足している場合に、具体的で読みやすい質問を各項目で提案してください。

**現在の入力内容:**

【何をするために倍速会議を使うのですか？】
${purpose.trim() || "（未入力）"}

【背景情報】
${backgroundInfo.trim() || "（未入力）"}

**指摘の観点:**
- 目的: 倍速会議を使う目的は何か、洗い出したいのは現状認識なのか・課題なのか・理想像なのか・価値観なのか、具体的にどんなトピックなのか、それを整理して見出したいのは合意点なのか・相違点なのか・何を誰もわかっていないのかなのか、認識を洗い出して整理したいと思ったきっかけとなった出来事や感情や困りごとはなんなのか、認識を洗い出すことを通じて何をしたいのか、集団がどうなったらいいのか
- 背景情報: どんな集団なのか（人数、役割、関係性）、どんな問題の背景があるのか、どんな状況なのか

**出力形式:**
もし情報が十分に書かれていて、特に追加で聞くべきことがない場合は、空の配列を返してください: []

もし情報が不足している場合は、具体的で読みやすい質問を1〜3個、JSON配列形式で出力してください:
["質問1", "質問2", "質問3"]

質問は「〜について教えてください」「〜はどうですか？」のような優しい口調で、具体的に何を書けばいいかイメージしやすい形にしてください。抽象的な質問は避けてください。

質問はそれぞれ50〜100文字程度になるように調整してください。

JSON配列のみを出力し、他の説明文は含めないでください。`;

    console.log("=== Form Suggestion LLM Input ===");
    console.log("Model:", MODEL);
    console.log(prompt);
    console.log("=================================\n");

    const response = await axios.post(
      OPENROUTER_API_URL,
      {
        model: MODEL,
        messages: [{ role: "user", content: prompt }],
      },
      {
        headers: {
          Authorization: `Bearer ${apiKey}`,
          "Content-Type": "application/json",
          "HTTP-Referer": "https://cartographer.app",
          "X-Title": "Cartographer",
        },
        timeout: 15000, // 15 second timeout
      },
    );

    const output = response.data.choices[0].message.content;

    console.log("=== Form Suggestion LLM Output ===");
    console.log(output);
    console.log("==================================\n");

    // Extract JSON array from response
    const jsonMatch = output.match(/\[[\s\S]*?\]/);
    if (!jsonMatch) {
      console.warn("Failed to parse LLM response, returning empty suggestions");
      return NextResponse.json({ suggestions: [] });
    }

    const suggestions = JSON.parse(jsonMatch[0]);

    return NextResponse.json({ suggestions });
  } catch (error) {
    console.error("Form suggestion error:", error);

    if (axios.isAxiosError(error)) {
      console.error("LLM API Error:", {
        status: error.response?.status,
        statusText: error.response?.statusText,
        data: error.response?.data,
      });

      if (error.response?.status === 429) {
        return NextResponse.json(
          { error: "Rate limit exceeded" },
          { status: 429 },
        );
      }
    }

    return NextResponse.json(
      { error: "Failed to generate suggestions" },
      { status: 500 },
    );
  }
}
