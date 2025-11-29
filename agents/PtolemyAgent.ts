import type { SupabaseClient } from "@supabase/supabase-js";

import {
  generatePlanMarkdown,
  generateSurveyAnalysisMarkdown,
  generateSurveyStatements,
  type ParticipantReflectionInput,
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
  constructor(private supabase: SupabaseClient) {}

  setSupabaseClient(supabase: SupabaseClient) {
    this.supabase = supabase;
  }

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
      .select("id, title, context, goal")
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

    const [
      latestAnalysis,
      userMessages,
      eventThreadContext,
      participantCount,
      participantReflections,
    ] = await Promise.all([
      this.getLatestEventMarkdown(context.thread.id, "survey_analysis"),
      this.getRecentUserMessages(context.thread.id),
      this.getEventThreadContext(context.thread.id),
      this.getParticipantCount(context.session.id),
      this.getParticipantReflections(context.session.id),
    ]);

    let markdown: string;
    try {
      markdown = await generatePlanMarkdown({
        sessionTitle: context.session.title,
        sessionGoal: context.session.goal,
        initialContext: context.session.context,
        latestAnalysisMarkdown: latestAnalysis,
        recentUserMessages: userMessages,
        eventThreadContext,
        participantCount,
        participantReflections,
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

    const [
      planMarkdown,
      latestAnalysisMarkdown,
      eventThreadContext,
      participantCount,
    ] = await Promise.all([
      this.getLatestEventMarkdown(context.thread.id, "plan"),
      this.getLatestEventMarkdown(context.thread.id, "survey_analysis"),
      this.getEventThreadContext(context.thread.id),
      this.getParticipantCount(context.session.id),
    ]);

    let statementTexts: string[];
    try {
      statementTexts = await generateSurveyStatements({
        sessionTitle: context.session.title,
        sessionGoal: context.session.goal,
        initialContext: context.session.context,
        planMarkdown,
        latestAnalysisMarkdown,
        eventThreadContext,
        participantCount,
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
    const preparedStatements = statementTexts.slice(0, 15);
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
      .select(
        "statement_id, value, participant_user_id, response_type, text_response",
      )
      .in("statement_id", statementIds);

    if (responsesError) {
      console.error(
        "[Ptolemy] Failed to load responses for analysis:",
        responsesError,
      );
      return { status: "waiting" };
    }

    let participantNameMap: Map<string, string> | undefined;
    const participantIds = new Set(
      (responses ?? [])
        .map((response) => response.participant_user_id)
        .filter((participantId): participantId is string =>
          Boolean(participantId),
        ),
    );

    if (participantIds.size > 0) {
      const { data: participantRows, error: participantError } =
        await this.supabase
          .from("participants")
          .select("user_id, name")
          .eq("session_id", context.session.id)
          .in("user_id", Array.from(participantIds));

      if (participantError) {
        console.error(
          "[Ptolemy] Failed to load participant names for analysis:",
          participantError,
        );
      } else if (participantRows) {
        participantNameMap = new Map(
          participantRows.map((participant) => [
            participant.user_id,
            participant.name,
          ]),
        );
      }
    }

    const participantCount = await this.getParticipantCount(context.session.id);
    const stats = this.buildStatementStats(
      statements ?? [],
      responses ?? [],
      participantNameMap,
    );
    const eventThreadContext = await this.getEventThreadContext(
      context.thread.id,
    );
    let markdown: string;
    try {
      markdown = await generateSurveyAnalysisMarkdown({
        sessionTitle: context.session.title,
        sessionGoal: context.session.goal,
        initialContext: context.session.context,
        totalParticipants: participantCount,
        statements: stats,
        eventThreadContext,
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

  private async getParticipantReflections(
    sessionId: string,
  ): Promise<ParticipantReflectionInput[]> {
    const { data, error } = await this.supabase
      .from("participant_reflections")
      .select(
        `
          text,
          submitted_at,
          participant:participants(name)
        `,
      )
      .eq("session_id", sessionId)
      .order("submitted_at", { ascending: true });

    if (error) {
      console.error("[Ptolemy] Failed to load participant reflections:", error);
      return [];
    }

    if (!data) {
      return [];
    }

    const reflections: ParticipantReflectionInput[] = [];

    for (const row of data) {
      const text = typeof row.text === "string" ? row.text.trim() : "";
      if (!text) {
        continue;
      }

      const participant = row.participant as
        | { name?: string | null }
        | null
        | undefined;
      const submittedAtRaw =
        typeof row.submitted_at === "string"
          ? row.submitted_at
          : row.submitted_at instanceof Date
            ? row.submitted_at.toISOString()
            : undefined;
      const submittedAt =
        submittedAtRaw && submittedAtRaw.length > 0
          ? this.formatTimestamp(submittedAtRaw)
          : undefined;

      reflections.push({
        text,
        name:
          participant && typeof participant.name === "string"
            ? participant.name
            : undefined,
        submittedAt,
      });
    }

    return reflections;
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
    responses: Array<{
      statement_id: string;
      value: number | null;
      response_type?: string;
      text_response?: string | null;
      participant_user_id?: string;
    }>,
    participantNameMap?: Map<string, string>,
  ): StatementStat[] {
    const responseMap = new Map<
      string,
      Array<{
        value: number | null;
        response_type?: string;
        text_response?: string | null;
        participant_user_id?: string;
      }>
    >();
    responses.forEach((response) => {
      const list = responseMap.get(response.statement_id) ?? [];
      list.push(response);
      responseMap.set(response.statement_id, list);
    });

    return statements.map((statement) => {
      const statementResponses =
        responseMap.get(statement.id) ??
        ([] as Array<{
          value: number | null;
          response_type?: string;
          text_response?: string | null;
          participant_user_id?: string;
        }>);
      const scaleResponses = statementResponses.filter(
        (response) => response.response_type !== "free_text",
      );
      const freeTextResponses = statementResponses.filter(
        (response) => response.response_type === "free_text",
      );
      const totalCount = scaleResponses.length;
      const counts = {
        strongYes: 0,
        yes: 0,
        dontKnow: 0,
        no: 0,
        strongNo: 0,
      };

      const participantResponses = scaleResponses.map((response) => {
        const participantId = response.participant_user_id;
        const participantName = participantId
          ? (participantNameMap?.get(participantId) ?? truncate(participantId))
          : "Unknown";

        switch (response.value ?? 0) {
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
        return {
          participantId: participantId ?? "unknown",
          participantName,
          value: response.value ?? 0,
        };
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
        participantResponses,
        freeTextResponses: freeTextResponses
          .filter((entry) => Boolean(entry.text_response))
          .map((entry) => {
            const participantId = entry.participant_user_id;
            return {
              participantId: participantId ?? "unknown",
              participantName: participantId
                ? (participantNameMap?.get(participantId) ??
                  truncate(participantId))
                : "Unknown",
              text: entry.text_response ?? "",
            };
          }),
      };
    });
  }

  private async getEventThreadContext(threadId: string): Promise<string> {
    const { data: events, error } = await this.supabase
      .from("events")
      .select(
        "id, type, agent_id, user_id, progress, payload, order_index, created_at",
      )
      .eq("thread_id", threadId)
      .order("order_index", { ascending: true });

    if (error) {
      console.error("[Ptolemy] Failed to load events for context:", error);
      return "(failed to load event history)";
    }

    if (!events || events.length === 0) {
      return "(no events yet)";
    }

    const statementIds = new Set<string>();
    events.forEach((event) => {
      const payload = (event.payload ?? {}) as { statementIds?: string[] };
      if (Array.isArray(payload.statementIds)) {
        for (const id of payload.statementIds) {
          statementIds.add(id);
        }
      }
    });

    let statements: { id: string; text: string }[] = [];
    if (statementIds.size > 0) {
      const { data: statementRows, error: statementsError } =
        await this.supabase
          .from("statements")
          .select("id, text")
          .in("id", Array.from(statementIds));
      if (statementsError) {
        console.error(
          "[Ptolemy] Failed to load statements for context:",
          statementsError,
        );
      } else if (statementRows) {
        statements = statementRows;
      }
    }

    let responses: {
      statement_id: string;
      value: number | null;
      response_type?: string;
      text_response?: string | null;
      participant_user_id?: string;
    }[] = [];
    if (statementIds.size > 0) {
      const { data: responseRows, error: responsesError } = await this.supabase
        .from("responses")
        .select(
          "statement_id, value, participant_user_id, response_type, text_response",
        )
        .in("statement_id", Array.from(statementIds));
      if (responsesError) {
        console.error(
          "[Ptolemy] Failed to load responses for context:",
          responsesError,
        );
      } else if (responseRows) {
        responses = responseRows;
      }
    }

    const stats = this.buildStatementStats(statements, responses);
    const statsById = new Map<string, StatementStat>();
    statements.forEach((statement, index) => {
      const stat = stats[index];
      if (stat) {
        statsById.set(statement.id, stat);
      }
    });
    const statementTextById = new Map(
      statements.map((statement) => [statement.id, statement.text]),
    );

    const lines: string[] = ["<event_thread_history>"];
    events.forEach((event) => {
      const tagName = event.type ?? "event";
      const attributes = [
        event.created_at
          ? `created_at="${this.formatTimestamp(event.created_at)}"`
          : null,
        event.user_id ? `user="${truncate(event.user_id)}"` : null,
      ]
        .filter(Boolean)
        .join(" ");
      const openTag =
        attributes.length > 0 ? `<${tagName} ${attributes}>` : `<${tagName}>`;
      lines.push(this.indent(openTag, 1));

      const payload = (event.payload ?? {}) as {
        markdown?: string;
        statementIds?: string[];
        error?: string;
      };

      if (payload.error) {
        lines.push(this.indent(`<error>${payload.error}</error>`, 2));
      }

      if (
        Array.isArray(payload.statementIds) &&
        payload.statementIds.length > 0
      ) {
        lines.push(
          this.indent(`<statements count="${payload.statementIds.length}">`, 2),
        );
        payload.statementIds.forEach((statementId, idx) => {
          const text = statementTextById.get(statementId) ?? "(text not found)";
          lines.push(
            this.indent(
              `<statement id="${statementId}" index="${idx + 1}">`,
              3,
            ),
          );
          lines.push(this.indent(`<text>${text}</text>`, 4));
          const stat = statsById.get(statementId);
          if (stat) {
            lines.push(this.indent(`responses total=${stat.totalCount}`, 4));
            lines.push(
              this.indent(`strong_yes=${stat.distribution.strongYes}%`, 5),
            );
            lines.push(this.indent(`yes=${stat.distribution.yes}%`, 5));
            lines.push(
              this.indent(`dont_know=${stat.distribution.dontKnow}%`, 5),
            );
            lines.push(this.indent(`no=${stat.distribution.no}%`, 5));
            lines.push(
              this.indent(`strong_no=${stat.distribution.strongNo}%`, 5),
            );
            if (stat.freeTextResponses.length > 0) {
              lines.push(
                this.indent(
                  `free_text_count=${stat.freeTextResponses.length}`,
                  4,
                ),
              );
              stat.freeTextResponses.slice(0, 3).forEach((entry, entryIdx) => {
                const participantLabel = entry.participantId
                  ? truncate(entry.participantId)
                  : "unknown";
                lines.push(
                  this.indent(
                    `<free_text idx="${entryIdx + 1}" participant="${participantLabel}">${entry.text}</free_text>`,
                    5,
                  ),
                );
              });
            }
          }
          lines.push(this.indent("</statement>", 3));
        });
        lines.push(this.indent("</statements>", 2));
      }

      if (payload.markdown && payload.markdown.trim().length > 0) {
        lines.push(this.indent('<content format="markdown">', 2));
        lines.push(this.indentBlock(payload.markdown.trim(), 3));
        lines.push(this.indent("</content>", 2));
      }

      lines.push(this.indent(`</${tagName}>`, 1));
    });

    lines.push("</event_thread_history>");

    return lines.join("\n");
  }

  private formatTimestamp(value?: string | null) {
    if (!value) return "";
    const match = value.match(
      /^(\d{4}-\d{2}-\d{2})[T ](\d{2}:\d{2})(?::\d{2}(?:\.\d+)?)?(?:Z|[+-]\d{2}:\d{2})?$/,
    );
    if (match) {
      return `${match[1]} ${match[2]}`;
    }
    return value.replace("T", " ").slice(0, 16);
  }

  private indent(text: string, level = 1) {
    const pad = "  ".repeat(level);
    return `${pad}${text}`;
  }

  private indentBlock(text: string, level = 1) {
    return text
      .split("\n")
      .map((line) => this.indent(line, level))
      .join("\n");
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
