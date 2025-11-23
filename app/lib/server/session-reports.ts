import { callLLM } from "@/lib/llm";
import { supabase } from "@/lib/supabase";

type ResponseValue = -2 | -1 | 0 | 1 | 2;

function getResponseLabel(value: ResponseValue): string {
  switch (value) {
    case 2:
      return "強く同意";
    case 1:
      return "同意";
    case 0:
      return "わからない";
    case -1:
      return "反対";
    case -2:
      return "強く反対";
    default:
      return "Unknown";
  }
}

export type SessionReportStatus =
  | "pending"
  | "generating"
  | "completed"
  | "failed";

export type SessionReportRecord = {
  id: string;
  sessionId: string;
  version: number;
  status: SessionReportStatus;
  requestMarkdown: string;
  contentMarkdown: string | null;
  createdBy: string;
  model: string;
  errorMessage: string | null;
  tokenUsage: Record<string, unknown> | null;
  promptSnapshot: Record<string, unknown> | null;
  createdAt: string;
  updatedAt: string;
  completedAt: string | null;
};

type SessionRow = {
  id: string;
  title: string;
  context: string;
  goal: string;
};

type StatementRow = {
  id: string;
  text: string;
  order_index: number;
};

type ResponseRow = {
  statement_id: string;
  participant_user_id: string;
  value: ResponseValue;
};

type ParticipantRow = {
  user_id: string;
  name: string;
};

type TimelineStatement = {
  id: string;
  text: string;
  orderIndex: number;
};

type TimelineEntry = {
  id: string;
  type: string;
  createdAt: string;
  progress: number;
  agentId: string | null;
  userId: string | null;
  markdown: string | null;
  payloadSummary: string | null;
  statements: TimelineStatement[];
};

type ParticipantSummary = {
  userId: string;
  name: string;
  answeredCount: number;
  totalStatements: number;
  responses: {
    statementId: string;
    statementNumber: number;
    statementText: string;
    value: ResponseValue;
    valueLabel: string;
  }[];
};

type StatementStat = {
  statementId: string;
  statementNumber: number;
  text: string;
  percentages: {
    strongYes: number;
    yes: number;
    dontKnow: number;
    no: number;
    strongNo: number;
  };
  totalResponses: number;
};

export type SessionReportPromptSnapshot = {
  session: SessionRow;
  participantSummaries: ParticipantSummary[];
  statementStats: StatementStat[];
  eventTimeline: TimelineEntry[];
};

function mapReportRow(row: {
  id: string;
  session_id: string;
  version: number;
  status: SessionReportStatus;
  request_markdown: string;
  content_markdown: string | null;
  created_by: string;
  model: string;
  error_message: string | null;
  token_usage: Record<string, unknown> | null;
  prompt_snapshot: Record<string, unknown> | null;
  created_at: string;
  updated_at: string;
  completed_at: string | null;
}): SessionReportRecord {
  return {
    id: row.id,
    sessionId: row.session_id,
    version: row.version,
    status: row.status,
    requestMarkdown: row.request_markdown,
    contentMarkdown: row.content_markdown,
    createdBy: row.created_by,
    model: row.model,
    errorMessage: row.error_message,
    tokenUsage: row.token_usage,
    promptSnapshot: row.prompt_snapshot,
    createdAt: row.created_at,
    updatedAt: row.updated_at,
    completedAt: row.completed_at,
  };
}

async function getNextReportVersion(sessionId: string): Promise<number> {
  const { data, error } = await supabase
    .from("session_reports")
    .select("version")
    .eq("session_id", sessionId)
    .order("version", { ascending: false })
    .limit(1)
    .maybeSingle();

  if (error && error.code !== "PGRST116") {
    throw new Error(
      `Failed to compute next session report version: ${error.message}`,
    );
  }

  if (!data) {
    return 1;
  }

  return (data.version ?? 0) + 1;
}

export async function listSessionReports(
  sessionId: string,
): Promise<SessionReportRecord[]> {
  const { data, error } = await supabase
    .from("session_reports")
    .select(
      "id, session_id, version, status, request_markdown, content_markdown, created_by, model, error_message, token_usage, prompt_snapshot, created_at, updated_at, completed_at",
    )
    .eq("session_id", sessionId)
    .order("created_at", { ascending: false });

  if (error) {
    throw new Error(`Failed to list session reports: ${error.message}`);
  }

  return (data ?? []).map(mapReportRow);
}

export async function getSessionReportById(
  reportId: string,
): Promise<SessionReportRecord | null> {
  const { data, error } = await supabase
    .from("session_reports")
    .select(
      "id, session_id, version, status, request_markdown, content_markdown, created_by, model, error_message, token_usage, prompt_snapshot, created_at, updated_at, completed_at",
    )
    .eq("id", reportId)
    .maybeSingle();

  if (error) {
    if (error.code === "PGRST116") {
      return null;
    }
    throw new Error(`Failed to load session report: ${error.message}`);
  }

  return data ? mapReportRow(data) : null;
}

export async function createSessionReportRecord(options: {
  sessionId: string;
  userId: string;
  requestMarkdown?: string;
}): Promise<SessionReportRecord> {
  const version = await getNextReportVersion(options.sessionId);
  const trimmedRequest = options.requestMarkdown?.trim() ?? "";
  const { data, error } = await supabase
    .from("session_reports")
    .insert({
      session_id: options.sessionId,
      version,
      status: "pending",
      request_markdown: trimmedRequest,
      created_by: options.userId,
    })
    .select(
      "id, session_id, version, status, request_markdown, content_markdown, created_by, model, error_message, token_usage, prompt_snapshot, created_at, updated_at, completed_at",
    )
    .single();

  if (error || !data) {
    throw new Error(
      `Failed to create session report record: ${error?.message ?? "unknown error"}`,
    );
  }

  return mapReportRow(data);
}

async function fetchSessionRow(sessionId: string): Promise<SessionRow> {
  const { data, error } = await supabase
    .from("sessions")
    .select("id, title, context, goal")
    .eq("id", sessionId)
    .single();

  if (error || !data) {
    throw new Error(
      `Failed to fetch session context: ${error?.message ?? "not found"}`,
    );
  }

  return data;
}

async function fetchStatements(sessionId: string): Promise<StatementRow[]> {
  const { data, error } = await supabase
    .from("statements")
    .select("id, text, order_index")
    .eq("session_id", sessionId)
    .order("order_index", { ascending: true });

  if (error) {
    throw new Error(`Failed to load statements: ${error.message}`);
  }

  return data ?? [];
}

async function fetchResponses(sessionId: string): Promise<ResponseRow[]> {
  const { data, error } = await supabase
    .from("responses")
    .select("statement_id, participant_user_id, value")
    .eq("session_id", sessionId);

  if (error) {
    throw new Error(`Failed to load responses: ${error.message}`);
  }

  return data ?? [];
}

async function fetchParticipants(sessionId: string): Promise<ParticipantRow[]> {
  const { data, error } = await supabase
    .from("participants")
    .select("user_id, name")
    .eq("session_id", sessionId)
    .order("created_at", { ascending: true });

  if (error) {
    throw new Error(`Failed to load participants: ${error.message}`);
  }

  return data ?? [];
}

async function fetchTimelineEntries(
  sessionId: string,
  statementsById: Map<string, StatementRow>,
): Promise<TimelineEntry[]> {
  const { data: thread, error: threadError } = await supabase
    .from("event_threads")
    .select("id")
    .eq("session_id", sessionId)
    .maybeSingle();

  if (threadError && threadError.code !== "PGRST116") {
    throw new Error(
      `Failed to lookup event thread: ${threadError.message ?? "unknown error"}`,
    );
  }

  if (!thread) {
    return [];
  }

  const { data: events, error: eventsError } = await supabase
    .from("events")
    .select(
      "id, type, agent_id, user_id, progress, payload, order_index, created_at",
    )
    .eq("thread_id", thread.id)
    .order("order_index", { ascending: true });

  if (eventsError) {
    throw new Error(`Failed to fetch event timeline: ${eventsError.message}`);
  }

  return (events ?? []).map((event) => {
    const payload = (event.payload ?? {}) as Record<string, unknown>;
    const markdown =
      typeof payload.markdown === "string"
        ? (payload.markdown as string)
        : null;
    const statementIds = Array.isArray(
      (payload as { statementIds?: string[] }).statementIds,
    )
      ? ((payload as { statementIds?: string[] }).statementIds as string[])
      : [];
    const statements = statementIds
      .map((id) => statementsById.get(id))
      .filter((stmt): stmt is StatementRow => Boolean(stmt))
      .map((stmt) => ({
        id: stmt.id,
        text: stmt.text,
        orderIndex: stmt.order_index ?? 0,
      }));

    const payloadSummary = buildPayloadSummary(payload);

    return {
      id: event.id,
      type: event.type,
      createdAt: event.created_at,
      progress: Number(event.progress ?? 0),
      agentId: event.agent_id,
      userId: event.user_id,
      markdown,
      payloadSummary,
      statements,
    };
  });
}

function buildPayloadSummary(payload: Record<string, unknown>): string | null {
  const cloned = { ...payload };

  if (typeof cloned.markdown === "string") {
    delete cloned.markdown;
  }

  if (
    Array.isArray((cloned as { statementIds?: string[] }).statementIds) &&
    ((cloned as { statementIds?: string[] }).statementIds as string[]).length >
      0
  ) {
    delete cloned.statementIds;
  }

  const keys = Object.keys(cloned);

  if (keys.length === 0) {
    return null;
  }

  const summary = JSON.stringify(cloned, null, 2);
  if (summary.length <= 1200) {
    return summary;
  }
  return `${summary.slice(0, 1197)}...`;
}

function buildParticipantSummaries(
  participants: ParticipantRow[],
  responses: ResponseRow[],
  statementsById: Map<string, StatementRow>,
  totalStatements: number,
): ParticipantSummary[] {
  const participantMap = new Map<string, ParticipantSummary>();

  participants.forEach((participant) => {
    participantMap.set(participant.user_id, {
      userId: participant.user_id,
      name: participant.name,
      answeredCount: 0,
      totalStatements,
      responses: [],
    });
  });

  responses.forEach((response) => {
    const participant = participantMap.get(response.participant_user_id);
    if (!participant) return;
    const statement = statementsById.get(response.statement_id);
    if (!statement) return;
    participant.responses.push({
      statementId: statement.id,
      statementNumber: (statement.order_index ?? 0) + 1,
      statementText: statement.text,
      value: response.value,
      valueLabel: getResponseLabel(response.value),
    });
  });

  participantMap.forEach((participant) => {
    participant.responses.sort((a, b) => a.statementNumber - b.statementNumber);
    participant.answeredCount = participant.responses.length;
  });

  return Array.from(participantMap.values());
}

function buildStatementStats(
  statements: StatementRow[],
  responses: ResponseRow[],
): StatementStat[] {
  const responseMap = new Map<string, ResponseValue[]>();

  responses.forEach((response) => {
    const list = responseMap.get(response.statement_id) ?? [];
    list.push(response.value);
    responseMap.set(response.statement_id, list);
  });

  return statements.map((statement) => {
    const list = responseMap.get(statement.id) ?? [];
    const totalResponses = list.length;
    const counts = {
      strongYes: list.filter((v) => v === 2).length,
      yes: list.filter((v) => v === 1).length,
      dontKnow: list.filter((v) => v === 0).length,
      no: list.filter((v) => v === -1).length,
      strongNo: list.filter((v) => v === -2).length,
    };

    const toPercent = (value: number) =>
      totalResponses === 0
        ? 0
        : Math.round((value / totalResponses) * 1000) / 10;

    return {
      statementId: statement.id,
      statementNumber: (statement.order_index ?? 0) + 1,
      text: statement.text,
      percentages: {
        strongYes: toPercent(counts.strongYes),
        yes: toPercent(counts.yes),
        dontKnow: toPercent(counts.dontKnow),
        no: toPercent(counts.no),
        strongNo: toPercent(counts.strongNo),
      },
      totalResponses,
    };
  });
}

function formatParticipantSummaries(
  participants: ParticipantSummary[],
): string {
  if (participants.length === 0) {
    return "参加者の回答はまだありません。";
  }

  return participants
    .map((participant) => {
      const header = `### ${participant.name} (${participant.answeredCount}/${participant.totalStatements})`;
      if (participant.responses.length === 0) {
        return `${header}\n- まだ回答はありません。`;
      }
      const lines = participant.responses.map(
        (response) =>
          `- [${response.valueLabel}] #${response.statementNumber} ${response.statementText}`,
      );
      return `${header}\n${lines.join("\n")}`;
    })
    .join("\n\n");
}

function formatStatementStats(stats: StatementStat[]): string {
  if (stats.length === 0) {
    return "ステートメントが未設定です。";
  }

  return stats
    .map((stat) => {
      const header = `### #${stat.statementNumber} ${stat.text}`;
      if (stat.totalResponses === 0) {
        return `${header}\n- まだ回答がありません。`;
      }
      return `${header}
- 強く同意: ${stat.percentages.strongYes}%
- 同意: ${stat.percentages.yes}%
- わからない: ${stat.percentages.dontKnow}%
- 反対: ${stat.percentages.no}%
- 強く反対: ${stat.percentages.strongNo}%`;
    })
    .join("\n\n");
}

function indentBlock(text: string, indent = 2): string {
  const pad = " ".repeat(indent);
  return text
    .split("\n")
    .map((line) => `${pad}${line}`)
    .join("\n");
}

function formatEventTimeline(events: TimelineEntry[]): string {
  if (events.length === 0) {
    return "イベントはまだ登録されていません。";
  }

  return events
    .map((event, index) => {
      const timestamp = new Date(event.createdAt).toLocaleString("ja-JP", {
        month: "short",
        day: "numeric",
        hour: "2-digit",
        minute: "2-digit",
        hour12: false,
      });
      const parts = [
        `${index + 1}. [${timestamp}] ${event.type.toUpperCase()}`,
        `   - Agent: ${event.agentId ?? "system"}`,
        `   - User: ${event.userId ?? "n/a"}`,
        `   - Progress: ${Math.round(event.progress * 100)}%`,
      ];
      if (event.statements.length > 0) {
        const statementText = event.statements
          .map((statement) => `#${statement.orderIndex + 1} ${statement.text}`)
          .join(" / ");
        parts.push(`   - Statements: ${statementText}`);
      }
      if (event.markdown) {
        parts.push(`   - Markdown:\n${indentBlock(event.markdown, 6)}`);
      }
      if (event.payloadSummary) {
        parts.push(`   - Payload:\n${indentBlock(event.payloadSummary, 6)}`);
      }
      return parts.join("\n");
    })
    .join("\n\n");
}

function buildPromptSections(snapshot: SessionReportPromptSnapshot): {
  participantSection: string;
  statementSection: string;
  timelineSection: string;
  sessionSection: string;
} {
  const participantSection = `## 参加者ごとの回答まとめ\n${formatParticipantSummaries(snapshot.participantSummaries)}`;
  const statementSection = `## ステートメント別の回答比率\n${formatStatementStats(snapshot.statementStats)}`;
  const timelineSection = `## Event Thread の記録（時系列）\n${formatEventTimeline(snapshot.eventTimeline)}`;
  const sessionContextLines = [
    `- タイトル: ${snapshot.session.title}`,
    `- ゴール: ${snapshot.session.goal || "(未設定)"}`,
    `- コンテキスト: ${snapshot.session.context || "(未設定)"}`,
  ].join("\n");
  const sessionSection = `## セッション情報\n${sessionContextLines}`;

  return {
    participantSection,
    statementSection,
    timelineSection,
    sessionSection,
  };
}

function buildPromptMessages(options: {
  snapshot: SessionReportPromptSnapshot;
  requestMarkdown: string;
}): { system: string; user: string } {
  const sections = buildPromptSections(options.snapshot);
  const adminSection = options.requestMarkdown
    ? `## Admin Priority Request\n${options.requestMarkdown}`
    : "";

  const requirements = `## レポート作成の指示
Part 1: インサイトまとめ
- 認識の合意点、相違点、誰も確信を持てていない点を整理してください。
- 特に意外性のある発見を優先的に抽出してください。
- 参加者単位の分析や、似た傾向を持つ人たちのクラスタも示してください。

Part 2:
- ここまでのインサイトや情報を踏まえ、セッションのGoal達成に役立つ考察やレポートの構成を考え、書いてください。

フォーマット:
- GFM準拠のMarkdownのみを出力してください。
- タイトルやセクション見出しを活用し、読みやすい構造にしてください。
- 必ず日本語で記述し、簡潔かつ洞察的なトーンを保ってください。`;

  const userMessage = [
    adminSection,
    sections.sessionSection,
    sections.participantSection,
    sections.statementSection,
    sections.timelineSection,
    requirements,
  ]
    .filter(Boolean)
    .join("\n\n");

  const systemMessage =
    "あなたは組織開発に長けたファシリテーターです。提供されたデータから高品質な分析レポートを作成し、Markdownで日本語出力のみを返してください。";

  return {
    system: systemMessage,
    user: userMessage,
  };
}

export async function generateSessionReport(reportId: string): Promise<void> {
  const { data: reportRow, error: reportError } = await supabase
    .from("session_reports")
    .select(
      "id, session_id, status, request_markdown, created_by, version, prompt_snapshot",
    )
    .eq("id", reportId)
    .maybeSingle();

  if (reportError || !reportRow) {
    console.error("Session report not found:", reportError);
    return;
  }

  if (reportRow.status === "completed") {
    return;
  }

  await supabase
    .from("session_reports")
    .update({
      status: "generating",
      updated_at: new Date().toISOString(),
      error_message: null,
    })
    .eq("id", reportId);

  try {
    const session = await fetchSessionRow(reportRow.session_id);
    const statements = await fetchStatements(reportRow.session_id);
    const statementsById = new Map(
      statements.map((statement) => [statement.id, statement]),
    );
    const responses = await fetchResponses(reportRow.session_id);
    const participants = await fetchParticipants(reportRow.session_id);
    const timeline = await fetchTimelineEntries(
      reportRow.session_id,
      statementsById,
    );

    const snapshot: SessionReportPromptSnapshot = {
      session,
      participantSummaries: buildParticipantSummaries(
        participants,
        responses,
        statementsById,
        statements.length,
      ),
      statementStats: buildStatementStats(statements, responses),
      eventTimeline: timeline,
    };

    const prompt = buildPromptMessages({
      snapshot,
      requestMarkdown: reportRow.request_markdown ?? "",
    });

    const content = await callLLM([
      { role: "system", content: prompt.system },
      { role: "user", content: prompt.user },
    ]);

    await supabase
      .from("session_reports")
      .update({
        status: "completed",
        content_markdown: content,
        prompt_snapshot: snapshot,
        updated_at: new Date().toISOString(),
        completed_at: new Date().toISOString(),
      })
      .eq("id", reportId);
  } catch (error) {
    console.error("Failed to generate session report:", error);
    await supabase
      .from("session_reports")
      .update({
        status: "failed",
        error_message:
          error instanceof Error
            ? error.message
            : "Unknown error while generating report",
        updated_at: new Date().toISOString(),
      })
      .eq("id", reportId);
  }
}

export async function triggerSessionReportGeneration(
  reportId: string,
): Promise<void> {
  try {
    await generateSessionReport(reportId);
  } catch (error) {
    console.error("Session report generation task failed:", error);
  }
}
