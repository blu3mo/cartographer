import { type NextRequest, NextResponse } from "next/server";

import { getUserIdFromRequest } from "@/lib/auth";
import { buildSessionBrief, callLLM } from "@/lib/llm";
import { supabase } from "@/lib/supabase";

type ChatMessage = {
  role: "assistant" | "user";
  content: string;
};

type ReflectionRow = {
  id: string;
  participant_user_id: string;
  session_id: string;
  text: string;
  created_at: string;
  submitted_at: string;
};

type SessionRow = {
  id: string;
  title: string;
  goal: string;
  context: string;
};

type ResponseRow = {
  statement_id: string;
  value: number;
  statement?: { text?: string; order_index?: number } | null;
};

type EventRow = {
  id: string;
  type: string;
  payload: Record<string, unknown> | null;
  created_at: string;
  order_index: number | string | null;
};

const FALLBACK_OPTIONS = [
  "もう少し詳しく話したいことがある",
  "別の観点を話してみたい",
  "ここまでで十分伝えられたと思う",
  "大丈夫です、次へ進みたい",
];

function mapReflection(row: ReflectionRow) {
  return {
    id: row.id,
    text: row.text,
    createdAt: row.created_at,
    submittedAt: row.submitted_at,
  };
}

async function ensureParticipant(sessionId: string, userId: string) {
  const { data: participant, error } = await supabase
    .from("participants")
    .select("user_id")
    .eq("user_id", userId)
    .eq("session_id", sessionId)
    .maybeSingle();

  if (error) {
    throw new Error("participant_verification_failed");
  }

  if (!participant) {
    throw new Error("participant_not_found");
  }
}

async function fetchSession(sessionId: string): Promise<SessionRow | null> {
  const { data, error } = await supabase
    .from("sessions")
    .select("id, title, goal, context")
    .eq("id", sessionId)
    .maybeSingle();

  if (error) {
    console.error("Failed to fetch session for reflection chat:", error);
    return null;
  }

  return data as SessionRow | null;
}

function formatResponseLabel(value: number) {
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
      return "Unknown";
  }
}

async function fetchParticipantResponses(
  sessionId: string,
  userId: string,
): Promise<ResponseRow[]> {
  const { data, error } = await supabase
    .from("responses")
    .select(
      `
        statement_id,
        value,
        statement:statements (
          text,
          order_index
        )
      `,
    )
    .eq("session_id", sessionId)
    .eq("participant_user_id", userId);

  if (error) {
    console.error("Failed to fetch responses for reflection chat:", error);
    return [];
  }

  return (data ?? []) as ResponseRow[];
}

function buildResponseSummary(responses: ResponseRow[]): string {
  if (!responses || responses.length === 0) {
    return "(回答はまだありません)";
  }

  return responses
    .sort((a, b) => {
      const orderA = a.statement?.order_index ?? 0;
      const orderB = b.statement?.order_index ?? 0;
      return orderA - orderB;
    })
    .map((response) => {
      const text = response.statement?.text ?? "(文言不明)";
      const label = formatResponseLabel(response.value);
      return `- 「${text}」 → ${label}`;
    })
    .join("\n");
}

async function fetchEventThreadContext(sessionId: string): Promise<string> {
  const { data: thread, error: threadError } = await supabase
    .from("event_threads")
    .select("id")
    .eq("session_id", sessionId)
    .maybeSingle();

  if (threadError) {
    console.error("Failed to fetch event thread for reflection chat:", threadError);
    return "(イベント履歴の取得に失敗しました)";
  }

  if (!thread) {
    return "(イベント履歴はまだありません)";
  }

  const { data: events, error: eventsError } = await supabase
    .from("events")
    .select("id, type, payload, created_at, order_index")
    .eq("thread_id", thread.id)
    .order("order_index", { ascending: true })
    .limit(80);

  if (eventsError || !events) {
    console.error("Failed to fetch events for reflection chat:", eventsError);
    return "(イベント履歴の取得に失敗しました)";
  }

  const statementIds = new Set<string>();
  events.forEach((event) => {
    const payload = (event.payload ?? {}) as { statementIds?: string[] };
    if (Array.isArray(payload.statementIds)) {
      payload.statementIds.forEach((id) => {
        if (typeof id === "string") {
          statementIds.add(id);
        }
      });
    }
  });

  let statementsById: Record<string, { id: string; text: string }> = {};
  if (statementIds.size > 0) {
    const { data: statements, error: statementsError } = await supabase
      .from("statements")
      .select("id, text")
      .in("id", Array.from(statementIds));

    if (statementsError) {
      console.error("Failed to fetch statements for reflection chat:", statementsError);
    } else if (statements) {
      statementsById = Object.fromEntries(
        statements.map((statement) => [
          statement.id,
          { id: statement.id, text: statement.text },
        ]),
      );
    }
  }

  const lines: string[] = ["<event_thread_history>"];
  (events as EventRow[]).forEach((event) => {
    lines.push(
      `  <${event.type} created_at="${event.created_at.slice(0, 16).replace("T", " ")}">`,
    );
    const payload = (event.payload ?? {}) as {
      markdown?: string;
      statementIds?: string[];
    };

    if (Array.isArray(payload.statementIds) && payload.statementIds.length > 0) {
      lines.push(`    <statements count="${payload.statementIds.length}">`);
      payload.statementIds.forEach((id, idx) => {
        const text = statementsById[id]?.text ?? "(テキスト不明)";
        lines.push(`      <statement index="${idx + 1}">${text}</statement>`);
      });
      lines.push("    </statements>");
    }

    if (payload.markdown && typeof payload.markdown === "string") {
      const trimmed = payload.markdown.trim();
      if (trimmed.length > 0) {
        lines.push("    <content>");
        lines.push(
          trimmed
            .split("\n")
            .map((line) => `      ${line}`)
            .join("\n"),
        );
        lines.push("    </content>");
      }
    }

    lines.push(`  </${event.type}>`);
  });
  lines.push("</event_thread_history>");

  return lines.join("\n");
}

function buildChatTranscript(messages: ChatMessage[]): string {
  if (!messages || messages.length === 0) {
    return "";
  }

  return messages
    .map((message, index) => {
      const speaker = message.role === "assistant" ? "AI" : "You";
      return `${index + 1}. ${speaker}: ${message.content.trim()}`;
    })
    .join("\n");
}

function buildPrompt(input: {
  session: SessionRow;
  responseSummary: string;
  eventThreadContext: string;
  chatTranscript: string;
}): { system: string; user: string } {
  const system = `あなたは思慮深く親切なファシリテーターです。プレッシャーをかけず、丁寧に追加で知りたい観点を確認します。
- 短い日本語で、敬体で話す
- 感謝や共感を一言添える
- 一度に質問は1つだけ
- 相手が強く感じている思いや、まだ聞けていない主張・懸念を引き出すことを意識する`;

  const sessionBrief = buildSessionBrief(input.session.goal, input.session.context);
  const user = `
<session_title>${input.session.title}</session_title>
<session_brief>
${sessionBrief}
</session_brief>

<participant_responses>
${input.responseSummary}
</participant_responses>

${input.eventThreadContext}

<chat_transcript>
${input.chatTranscript || "(まだ会話はありません)"}
</chat_transcript>

指示:
- 次のメッセージとして、穏やかに問いかけを1-2文で返してください。強く感じている点、まだ触れていない主張や懸念、背景の理由を伝えやすい聞き方にしてください。抽象的に聞くのではなく、状況・具体例・理由・誰に対して/どんな場面で等をはっきり尋ねてください。
- ユーザーが選べる候補を3〜4個、40文字以内の具体的な日本語フレーズで用意してください。丁寧で、気軽に答えられる内容にしてください。
- 候補には「ここまでで大丈夫」「別の観点を話したい」などの選択肢を含めてもよいです。
- 自由入力を歓迎する一言も添えてください。
- 出力はJSONのみ。例:
{
  "assistant_message": "返答",
  "options": ["候補1", "候補2", "候補3", "候補4"]
}`;

  return { system, user };
}

function parseLLMResponse(raw: string): { assistant: string; options: string[] } {
  const extractJson = (text: string) => {
    const fenced = text.match(/```json\\s*([\\s\\S]*?)```/i);
    if (fenced && fenced[1]) {
      return fenced[1];
    }
    const firstBrace = text.indexOf("{");
    const lastBrace = text.lastIndexOf("}");
    if (firstBrace !== -1 && lastBrace > firstBrace) {
      return text.slice(firstBrace, lastBrace + 1);
    }
    return text;
  };

  try {
    const cleaned = extractJson(raw);
    const parsed = JSON.parse(cleaned);
    const assistant =
      typeof parsed?.assistant_message === "string"
        ? parsed.assistant_message.trim()
        : "";
    const options = Array.isArray(parsed?.options)
      ? parsed.options.filter((opt: unknown) => typeof opt === "string").slice(0, 6)
      : [];

    if (assistant.length > 0 && options.length > 0) {
      return { assistant, options };
    }
  } catch (error) {
    console.error("Failed to parse reflection chat JSON:", error);
  }

  return {
    assistant: "よければ、今のテーマで気になることや伝えておきたいことを教えてください。",
    options: FALLBACK_OPTIONS,
  };
}

async function upsertReflectionRecord(input: {
  sessionId: string;
  userId: string;
  text: string;
  reflectionId?: string | null;
}): Promise<ReflectionRow | null> {
  const timestamp = new Date().toISOString();

  if (input.reflectionId) {
    const { data, error } = await supabase
      .from("participant_reflections")
      .update({
        text: input.text,
        submitted_at: timestamp,
      })
      .eq("id", input.reflectionId)
      .eq("participant_user_id", input.userId)
      .eq("session_id", input.sessionId)
      .select("id, participant_user_id, session_id, text, created_at, submitted_at")
      .maybeSingle();

    if (error) {
      console.error("Failed to update reflection record:", error);
      return null;
    }

    if (data) {
      return data as ReflectionRow;
    }
  }

  const { data, error: insertError } = await supabase
    .from("participant_reflections")
    .insert({
      participant_user_id: input.userId,
      session_id: input.sessionId,
      text: input.text,
      submitted_at: timestamp,
    })
    .select("id, participant_user_id, session_id, text, created_at, submitted_at")
    .single();

  if (insertError || !data) {
    console.error("Failed to insert reflection record:", insertError);
    return null;
  }

  return data as ReflectionRow;
}

export async function POST(
  request: NextRequest,
  { params }: { params: Promise<{ sessionId: string }> },
) {
  try {
    const { sessionId } = await params;
    const userId = getUserIdFromRequest(request);

    if (!userId) {
      return NextResponse.json(
        { error: "Unauthorized: Missing user ID" },
        { status: 401 },
      );
    }

    const body = await request.json();
    const { messages, reflectionId, finalize } = body as {
      messages?: ChatMessage[];
      reflectionId?: string;
      finalize?: boolean;
    };

    if (!Array.isArray(messages)) {
      return NextResponse.json(
        { error: "Invalid payload: messages must be an array" },
        { status: 400 },
      );
    }

    try {
      await ensureParticipant(sessionId, userId);
    } catch (error) {
      if ((error as Error).message === "participant_not_found") {
        return NextResponse.json(
          { error: "Participant not found in this session" },
          { status: 404 },
        );
      }
      console.error("Failed to verify participant:", error);
      return NextResponse.json(
        { error: "Failed to process reflection chat" },
        { status: 500 },
      );
    }

    const session = await fetchSession(sessionId);
    if (!session) {
      return NextResponse.json(
        { error: "Session not found" },
        { status: 404 },
      );
    }

    const chatTranscript = buildChatTranscript(messages);

    if (finalize) {
      const reflection = await upsertReflectionRecord({
        sessionId,
        userId,
        text: chatTranscript,
        reflectionId: reflectionId ?? null,
      });

      if (!reflection) {
        return NextResponse.json(
          { error: "Failed to store reflection" },
          { status: 500 },
        );
      }

      return NextResponse.json({
        assistantMessage: null,
        options: [],
        reflection: mapReflection(reflection),
      });
    }

    const [responses, eventThreadContext] = await Promise.all([
      fetchParticipantResponses(sessionId, userId),
      fetchEventThreadContext(sessionId),
    ]);

    const responseSummary = buildResponseSummary(responses);
    const { system, user } = buildPrompt({
      session,
      responseSummary,
      eventThreadContext,
      chatTranscript,
    });

    let assistantMessage: string;
    let options: string[];

    try {
      const llmResponse = await callLLM(
        [
          { role: "system", content: system },
          { role: "user", content: user },
        ],
        { model: "google/gemini-2.5-flash" },
      );
      const parsed = parseLLMResponse(llmResponse);
      assistantMessage = parsed.assistant;
      options = parsed.options;
    } catch (error) {
      console.error("LLM call failed for reflection chat:", error);
      assistantMessage =
        "よければ、このテーマで伝えておきたいことを教えてください。簡単で大丈夫です。";
      options = FALLBACK_OPTIONS;
    }

    const updatedMessages: ChatMessage[] = [
      ...messages,
      { role: "assistant", content: assistantMessage },
    ];
    const updatedTranscript = buildChatTranscript(updatedMessages);

    const reflection = await upsertReflectionRecord({
      sessionId,
      userId,
      text: updatedTranscript,
      reflectionId: reflectionId ?? null,
    });

    if (!reflection) {
      return NextResponse.json(
        { error: "Failed to store reflection" },
        { status: 500 },
      );
    }

    return NextResponse.json({
      assistantMessage,
      options,
      reflection: mapReflection(reflection),
    });
  } catch (error) {
    console.error("Failed to process reflection chat:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}
