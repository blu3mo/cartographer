import type { SupabaseClient } from "@supabase/supabase-js";

import {
  generatePlanMarkdown,
  generateSurveyAnalysisMarkdown,
  generateSurveyStatements,
  type StatementStat,
} from "./llm";
import type {
  AgentInstanceRow,
  AgentRunResult,
  EventThreadRow,
  PtolemyState,
  SessionRow,
  ThreadEventType,
} from "./types";
import { truncate } from "./utils";

type StatePayload = {
  lastPlanEventId?: string;
  surveyEventId?: string;
  statementIds?: string[];
};

type ThreadContext = {
  thread: EventThreadRow;
  session: SessionRow;
};

export class PtolemyAgent {
  constructor(private readonly supabase: SupabaseClient) {}

  async run(instance: AgentInstanceRow): Promise<AgentRunResult> {
    this.log(instance, "tick", { state: instance.state });
    switch (instance.state as PtolemyState) {
      case "CREATING_PLAN":
        return this.handleCreatingPlan(instance);
      case "WAITING_SURVEY":
        return this.handleWaitingForProceed(instance, "CREATING_SURVEY");
      case "CREATING_SURVEY":
        return this.handleCreatingSurvey(instance);
      case "COLLECTING_SURVEY":
        return this.handleCollectingSurvey(instance);
      case "WAITING_ANALYSIS":
        return this.handleWaitingForProceed(instance, "CREATING_ANALYSIS");
      case "CREATING_ANALYSIS":
        return this.handleCreatingAnalysis(instance);
      case "WAITING_PLAN":
        return this.handleWaitingForProceed(instance, "CREATING_PLAN");
      default:
        this.warn(instance, "unknown state, idling");
        return { status: "idle" };
    }
  }

  private async getThreadContext(
    threadId: string,
  ): Promise<ThreadContext | null> {
    const { data: thread, error: threadError } = await this.supabase
      .from("event_threads")
      .select("id, session_id, should_proceed")
      .eq("id", threadId)
      .single();

    if (threadError || !thread) {
      console.error("[Ptolemy] Failed to load thread context:", threadError);
      return null;
    }

    const { data: session, error: sessionError } = await this.supabase
      .from("sessions")
      .select("id, title, context")
      .eq("id", thread.session_id)
      .single();

    if (sessionError || !session) {
      console.error("[Ptolemy] Failed to load session context:", sessionError);
      return null;
    }

    return { thread, session };
  }

  private async handleCreatingPlan(
    instance: AgentInstanceRow,
  ): Promise<AgentRunResult> {
    this.log(instance, "creating plan event");
    const context = await this.getThreadContext(instance.thread_id);
    if (!context) return { status: "waiting" };

    const eventId = await this.createEvent({
      threadId: context.thread.id,
      type: "plan",
      agentId: instance.id,
      progress: 0,
      payload: { markdown: "" },
    });

    const [latestAnalysis, userMessages] = await Promise.all([
      this.getLatestEventMarkdown(context.thread.id, "survey_analysis"),
      this.getRecentUserMessages(context.thread.id),
    ]);

    let markdown: string;
    try {
      markdown = await generatePlanMarkdown({
        sessionTitle: context.session.title,
        context: context.session.context,
        latestAnalysisMarkdown: latestAnalysis,
        recentUserMessages: userMessages,
      });
    } catch (error) {
      const message = this.stringifyError(error);
      this.warn(instance, "plan generation failed", { error: message });
      await this.supabase
        .from("events")
        .update({
          payload: { markdown: "", error: message },
          progress: 0,
          updated_at: new Date().toISOString(),
        })
        .eq("id", eventId);
      return { status: "idle" };
    }

    await this.supabase
      .from("events")
      .update({
        payload: { markdown },
        progress: 1,
        updated_at: new Date().toISOString(),
      })
      .eq("id", eventId);
    this.log(instance, "plan markdown ready", { eventId });

    await this.transition(instance.id, "WAITING_SURVEY", {
      lastPlanEventId: eventId,
    });
    this.log(instance, "transitioned to WAITING_SURVEY", {
      planEventId: eventId,
    });

    return { status: "transitioned" };
  }

  private async handleWaitingForProceed(
    instance: AgentInstanceRow,
    nextState: PtolemyState,
  ): Promise<AgentRunResult> {
    const context = await this.getThreadContext(instance.thread_id);
    if (!context) return { status: "waiting" };

    if (!context.thread.should_proceed) {
      this.log(instance, "waiting for shouldProceed toggle");
      return { status: "waiting" };
    }

    await this.transition(
      instance.id,
      nextState,
      (instance.state_payload ?? {}) as StatePayload,
    );
    return { status: "transitioned" };
  }

  private async handleCreatingSurvey(
    instance: AgentInstanceRow,
  ): Promise<AgentRunResult> {
    this.log(instance, "creating survey event & statements");
    const context = await this.getThreadContext(instance.thread_id);
    if (!context) return { status: "waiting" };

    const surveyEventId = await this.createEvent({
      threadId: context.thread.id,
      type: "survey",
      agentId: instance.id,
      progress: 0,
      payload: { statementIds: [] },
    });

    const [planMarkdown, latestAnalysisMarkdown] = await Promise.all([
      this.getLatestEventMarkdown(context.thread.id, "plan"),
      this.getLatestEventMarkdown(context.thread.id, "survey_analysis"),
    ]);

    let statementTexts: string[];
    try {
      statementTexts = await generateSurveyStatements({
        sessionTitle: context.session.title,
        context: context.session.context,
        planMarkdown,
        latestAnalysisMarkdown,
      });
    } catch (error) {
      const message = this.stringifyError(error);
      this.warn(instance, "survey statement generation failed", {
        error: message,
      });
      await this.supabase
        .from("events")
        .update({
          payload: { statementIds: [], error: message },
          progress: 0,
          updated_at: new Date().toISOString(),
        })
        .eq("id", surveyEventId);
      return { status: "idle" };
    }

    const baseIndex = await this.getLastStatementOrderIndex(context.session.id);
    const preparedStatements = statementTexts.slice(0, 10);
    const statementsPayload = preparedStatements.map((text, index) => ({
      session_id: context.session.id,
      text,
      order_index: baseIndex + index + 1,
    }));

    if (statementsPayload.length === 0) {
      const message = "No statements generated for survey";
      this.warn(instance, message);
      await this.supabase
        .from("events")
        .update({
          payload: { statementIds: [], error: message },
          progress: 0,
          updated_at: new Date().toISOString(),
        })
        .eq("id", surveyEventId);
      return { status: "idle" };
    }

    const { data: insertedStatements, error: statementsError } =
      await this.supabase
        .from("statements")
        .insert(statementsPayload)
        .select("id");

    if (statementsError || !insertedStatements) {
      console.error(
        "[Ptolemy] Failed to insert survey statements:",
        statementsError,
      );
      return { status: "waiting" };
    }

    const statementIds = insertedStatements.map((row) => row.id);

    await this.supabase
      .from("events")
      .update({
        payload: { statementIds },
        progress: 0.5,
        updated_at: new Date().toISOString(),
      })
      .eq("id", surveyEventId);
    this.log(instance, "survey statements ready", {
      statementCount: statementIds.length,
    });

    await this.transition(instance.id, "COLLECTING_SURVEY", {
      surveyEventId,
      statementIds,
    });
    this.log(instance, "transitioned to COLLECTING_SURVEY", {
      surveyEventId,
      statementCount: statementIds.length,
    });

    return { status: "transitioned" };
  }

  private async handleCollectingSurvey(
    instance: AgentInstanceRow,
  ): Promise<AgentRunResult> {
    const payload = (instance.state_payload ?? {}) as StatePayload;
    if (!payload.statementIds?.length || !payload.surveyEventId) {
      console.warn("[Ptolemy] Missing survey context in state payload");
      return { status: "waiting" };
    }

    const context = await this.getThreadContext(instance.thread_id);
    if (!context) return { status: "waiting" };

    const participantCount = await this.getParticipantCount(context.session.id);
    if (participantCount === 0) {
      this.log(instance, "no participants yet, keep waiting");
      return { status: "waiting" };
    }

    const { data: responses, error } = await this.supabase
      .from("responses")
      .select("statement_id")
      .in("statement_id", payload.statementIds);

    if (error) {
      console.error("[Ptolemy] Failed to load responses:", error);
      return { status: "waiting" };
    }

    const counts = new Map<string, number>();
    (responses ?? []).forEach((row) => {
      counts.set(row.statement_id, (counts.get(row.statement_id) ?? 0) + 1);
    });

    const thresholdReached = payload.statementIds.every((id) => {
      const ratio = (counts.get(id) ?? 0) / participantCount;
      return ratio >= 0.8;
    });

    if (!thresholdReached) {
      this.log(instance, "still collecting responses", {
        participantCount,
        coverageByStatement: payload.statementIds.map((id) => ({
          id: truncate(id),
          ratio: Number(((counts.get(id) ?? 0) / participantCount).toFixed(2)),
        })),
      });
      return { status: "waiting" };
    }

    await this.supabase
      .from("events")
      .update({
        progress: 1,
        updated_at: new Date().toISOString(),
      })
      .eq("id", payload.surveyEventId);

    await this.transition(instance.id, "WAITING_ANALYSIS", {
      statementIds: payload.statementIds,
    });
    this.log(instance, "transitioned to WAITING_ANALYSIS");

    return { status: "transitioned" };
  }

  private async handleCreatingAnalysis(
    instance: AgentInstanceRow,
  ): Promise<AgentRunResult> {
    this.log(instance, "creating survey analysis");
    const payload = (instance.state_payload ?? {}) as StatePayload;
    const statementIds = payload.statementIds ?? [];
    const context = await this.getThreadContext(instance.thread_id);
    if (!context) return { status: "waiting" };
    if (statementIds.length === 0) {
      this.warn(instance, "no statements to analyze yet");
      return { status: "waiting" };
    }

    const analysisEventId = await this.createEvent({
      threadId: context.thread.id,
      type: "survey_analysis",
      agentId: instance.id,
      progress: 0,
      payload: { statementIds, markdown: "" },
    });

    const { data: statements, error: statementsError } = await this.supabase
      .from("statements")
      .select("id, text")
      .in("id", statementIds);

    if (statementsError) {
      console.error("[Ptolemy] Failed to load statements:", statementsError);
      return { status: "waiting" };
    }

    const { data: responses, error: responsesError } = await this.supabase
      .from("responses")
      .select("statement_id, value")
      .in("statement_id", statementIds);

    if (responsesError) {
      console.error(
        "[Ptolemy] Failed to load responses for analysis:",
        responsesError,
      );
      return { status: "waiting" };
    }

    const participantCount = await this.getParticipantCount(context.session.id);
    const stats = this.buildStatementStats(statements ?? [], responses ?? []);

    let markdown: string;
    try {
      markdown = await generateSurveyAnalysisMarkdown({
        sessionTitle: context.session.title,
        context: context.session.context,
        totalParticipants: participantCount,
        statements: stats,
      });
    } catch (error) {
      const message = this.stringifyError(error);
      this.warn(instance, "survey analysis generation failed", {
        error: message,
      });
      await this.supabase
        .from("events")
        .update({
          payload: { statementIds, error: message },
          progress: 0,
          updated_at: new Date().toISOString(),
        })
        .eq("id", analysisEventId);
      return { status: "idle" };
    }

    await this.supabase
      .from("events")
      .update({
        payload: { statementIds, markdown },
        progress: 1,
        updated_at: new Date().toISOString(),
      })
      .eq("id", analysisEventId);

    await this.transition(instance.id, "WAITING_PLAN");
    this.log(instance, "transitioned to WAITING_PLAN", { analysisEventId });

    return { status: "transitioned" };
  }

  private async createEvent(input: {
    threadId: string;
    type: ThreadEventType;
    agentId?: string;
    progress?: number;
    payload?: Record<string, unknown>;
  }): Promise<string> {
    const { data, error } = await this.supabase
      .from("events")
      .insert({
        thread_id: input.threadId,
        type: input.type,
        agent_id: input.agentId ?? null,
        progress: input.progress ?? 0,
        payload: input.payload ?? {},
      })
      .select("id")
      .single();

    if (error || !data) {
      throw new Error(
        `Failed to create ${input.type} event: ${error?.message ?? "unknown error"}`,
      );
    }

    return data.id;
  }

  private async transition(
    agentId: string,
    nextState: PtolemyState,
    payload: StatePayload = {},
  ) {
    await this.supabase
      .from("agent_instances")
      .update({
        state: nextState,
        state_payload: payload,
        updated_at: new Date().toISOString(),
      })
      .eq("id", agentId);
  }

  private async getLastStatementOrderIndex(sessionId: string) {
    const { data, error } = await this.supabase
      .from("statements")
      .select("order_index")
      .eq("session_id", sessionId)
      .order("order_index", { ascending: false })
      .limit(1)
      .maybeSingle();

    if (error && error.code !== "PGRST116") {
      console.error("[Ptolemy] Failed to fetch last statement order:", error);
    }

    return data?.order_index ?? -1;
  }

  private async getParticipantCount(sessionId: string): Promise<number> {
    const { error, count } = await this.supabase
      .from("participants")
      .select("user_id", { count: "exact", head: true })
      .eq("session_id", sessionId);

    if (error) {
      console.error("[Ptolemy] Failed to count participants:", error);
      return 0;
    }

    return count ?? 0;
  }

  private async getLatestEventMarkdown(
    threadId: string,
    type: ThreadEventType,
  ): Promise<string | undefined> {
    const { data, error } = await this.supabase
      .from("events")
      .select("payload")
      .eq("thread_id", threadId)
      .eq("type", type)
      .order("created_at", { ascending: false })
      .limit(1)
      .maybeSingle();

    if (error && error.code !== "PGRST116") {
      console.error("[Ptolemy] Failed to load latest event markdown:", error);
      return undefined;
    }

    const payload = (data?.payload ?? {}) as { markdown?: string };
    return typeof payload.markdown === "string" ? payload.markdown : undefined;
  }

  private async getRecentUserMessages(
    threadId: string,
    limit = 3,
  ): Promise<string[]> {
    const { data, error } = await this.supabase
      .from("events")
      .select("payload")
      .eq("thread_id", threadId)
      .eq("type", "user_message")
      .order("created_at", { ascending: false })
      .limit(limit);

    if (error) {
      console.error("[Ptolemy] Failed to load recent user messages:", error);
      return [];
    }

    const messages =
      data
        ?.map((row) => {
          const payload = row.payload as { markdown?: string } | null;
          return typeof payload?.markdown === "string"
            ? payload.markdown
            : null;
        })
        .filter((message): message is string => Boolean(message)) ?? [];

    return messages.reverse();
  }

  private buildStatementStats(
    statements: Array<{ id: string; text: string }>,
    responses: Array<{ statement_id: string; value: number }>,
  ): StatementStat[] {
    const responseMap = new Map<string, number[]>();
    responses.forEach((response) => {
      const list = responseMap.get(response.statement_id) ?? [];
      list.push(response.value);
      responseMap.set(response.statement_id, list);
    });

    return statements.map((statement) => {
      const statementResponses = responseMap.get(statement.id) ?? [];
      const totalCount = statementResponses.length;
      const counts = {
        strongYes: 0,
        yes: 0,
        dontKnow: 0,
        no: 0,
        strongNo: 0,
      };

      statementResponses.forEach((value) => {
        switch (value) {
          case 2:
            counts.strongYes++;
            break;
          case 1:
            counts.yes++;
            break;
          case 0:
            counts.dontKnow++;
            break;
          case -1:
            counts.no++;
            break;
          case -2:
            counts.strongNo++;
            break;
          default:
            break;
        }
      });

      const toPercent = (count: number) =>
        totalCount > 0 ? Math.round((count / totalCount) * 1000) / 10 : 0;

      return {
        text: statement.text,
        totalCount,
        distribution: {
          strongYes: toPercent(counts.strongYes),
          yes: toPercent(counts.yes),
          dontKnow: toPercent(counts.dontKnow),
          no: toPercent(counts.no),
          strongNo: toPercent(counts.strongNo),
        },
      };
    });
  }

  private log(
    instance: AgentInstanceRow,
    message: string,
    meta?: Record<string, unknown>,
  ) {
    const prefix = `[Ptolemy:${truncate(instance.id)}:${instance.state}]`;
    if (meta) {
      console.log(prefix, message, meta);
    } else {
      console.log(prefix, message);
    }
  }

  private warn(
    instance: AgentInstanceRow,
    message: string,
    meta?: Record<string, unknown>,
  ) {
    const prefix = `[Ptolemy:${truncate(instance.id)}:${instance.state}]`;
    if (meta) {
      console.warn(prefix, message, meta);
    } else {
      console.warn(prefix, message);
    }
  }

  private stringifyError(error: unknown) {
    if (error instanceof Error) {
      return error.message;
    }
    if (typeof error === "string") {
      return error;
    }
    return JSON.stringify(error);
  }
}
