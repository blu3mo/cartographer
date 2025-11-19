import {
  createClient,
  type RealtimeChannel,
  type SupabaseClient,
} from "@supabase/supabase-js";

import { PtolemyAgent } from "./PtolemyAgent";
import type { AgentInstanceRow, AgentRunResult } from "./types";
import { delay, truncate } from "./utils";

export class AgentManager {
  private static readonly RECONNECT_INTERVAL_MS = 60 * 60 * 1000;
  private supabase: SupabaseClient;
  private ptolemy: PtolemyAgent;
  private processing = new Set<string>();
  private subscriptions: RealtimeChannel[] = [];
  private reconnectInterval?: NodeJS.Timeout;
  private isRefreshing = false;

  constructor(
    private readonly supabaseUrl: string = process.env
      .NEXT_PUBLIC_SUPABASE_URL ?? "",
    private readonly supabaseServiceRoleKey: string = process.env
      .SUPABASE_SERVICE_ROLE_KEY ?? "",
  ) {
    if (!this.supabaseUrl || !this.supabaseServiceRoleKey) {
      throw new Error(
        "Missing NEXT_PUBLIC_SUPABASE_URL or SUPABASE_SERVICE_ROLE_KEY",
      );
    }

    this.supabase = this.createSupabaseClient();
    this.ptolemy = new PtolemyAgent(this.supabase);
  }

  async start() {
    console.log("[AgentManager] Starting…");
    await this.bootstrapExistingAgents();
    await this.initializeRealtimeSubscriptions();
    this.scheduleReconnect();
    console.log("[AgentManager] Ready");
  }

  async stop() {
    if (this.reconnectInterval) {
      clearInterval(this.reconnectInterval);
      this.reconnectInterval = undefined;
    }
    await this.stopRealtimeSubscriptions();
  }

  private async bootstrapExistingAgents() {
    const { data, error } = await this.supabase
      .from("agent_instances")
      .select("id");

    if (error) {
      console.error("[AgentManager] Failed to bootstrap agents:", error);
      return;
    }

    for (const row of data ?? []) {
      this.enqueueHandle(row.id);
    }
  }

  private async subscribeToAgentInstances() {
    const channel = this.supabase
      .channel("agent-instances")
      .on(
        "postgres_changes",
        { event: "INSERT", schema: "public", table: "agent_instances" },
        (payload) => {
          const id = (payload.new as { id?: string })?.id;
          if (id) {
            console.log("[AgentManager] Detected new agent", truncate(id));
            this.enqueueHandle(id);
          }
        },
      );

    await channel.subscribe();
    this.subscriptions.push(channel);
  }

  private async subscribeToThreadChanges() {
    const channel = this.supabase
      .channel("event-threads")
      .on(
        "postgres_changes",
        { event: "UPDATE", schema: "public", table: "event_threads" },
        (payload) => {
          const threadId = (payload.new as { id?: string })?.id;
          if (threadId) {
            void this.handleThreadUpdate(threadId);
          }
        },
      );

    await channel.subscribe();
    this.subscriptions.push(channel);
  }

  private async subscribeToResponses() {
    const channel = this.supabase
      .channel("responses-watch")
      .on(
        "postgres_changes",
        { event: "*", schema: "public", table: "responses" },
        (payload) => {
          const sessionId =
            (payload.new as { session_id?: string })?.session_id ??
            (payload.old as { session_id?: string })?.session_id;
          if (sessionId) {
            void this.handleResponseChange(sessionId);
          }
        },
      );

    await channel.subscribe();
    this.subscriptions.push(channel);
  }

  private enqueueHandle(agentId: string) {
    if (!agentId) return;
    void this.handleInstance(agentId);
  }

  private async handleInstance(agentId: string) {
    if (this.processing.has(agentId)) {
      return;
    }

    this.processing.add(agentId);
    let shouldRequeue = false;
    try {
      const { data: instance, error } = await this.supabase
        .from("agent_instances")
        .select("id, thread_id, agent_type, state, state_payload")
        .eq("id", agentId)
        .single();

      if (error || !instance) {
        console.error(
          "[AgentManager] Failed to load agent instance:",
          agentId,
          error,
        );
        return;
      }

      let result: AgentRunResult = { status: "idle" };
      if (instance.agent_type === "ptolemy") {
        result = await this.ptolemy.run(instance as AgentInstanceRow);
      } else {
        console.warn(
          `[AgentManager] Unsupported agent_type ${instance.agent_type}`,
        );
      }

      shouldRequeue = result.status === "transitioned";
    } catch (error) {
      console.error(
        "[AgentManager] Error while handling agent",
        agentId,
        error,
      );
    } finally {
      this.processing.delete(agentId);
    }

    if (shouldRequeue) {
      await delay(50);
      this.enqueueHandle(agentId);
    }
  }

  private async handleThreadUpdate(threadId: string) {
    const { data, error } = await this.supabase
      .from("agent_instances")
      .select("id")
      .eq("thread_id", threadId);

    if (error) {
      console.error("[AgentManager] Failed to load agents for thread:", error);
      return;
    }

    for (const row of data ?? []) {
      this.enqueueHandle(row.id);
    }
  }

  private async handleResponseChange(sessionId: string) {
    const { data: threads, error: threadError } = await this.supabase
      .from("event_threads")
      .select("id")
      .eq("session_id", sessionId);

    if (threadError) {
      console.error("[AgentManager] Failed to load threads:", threadError);
      return;
    }

    const threadIds = (threads ?? []).map((row) => row.id);
    if (threadIds.length === 0) {
      return;
    }

    const { data: agents, error: agentsError } = await this.supabase
      .from("agent_instances")
      .select("id, state")
      .in("thread_id", threadIds);

    if (agentsError) {
      console.error(
        "[AgentManager] Failed to load agents for responses:",
        agentsError,
      );
      return;
    }

    for (const agent of agents ?? []) {
      if (agent.state === "COLLECTING_SURVEY") {
        this.enqueueHandle(agent.id);
      }
    }
  }

  private createSupabaseClient(): SupabaseClient {
    return createClient(this.supabaseUrl, this.supabaseServiceRoleKey, {
      auth: {
        autoRefreshToken: false,
        persistSession: false,
      },
    });
  }

  private async initializeRealtimeSubscriptions() {
    await this.subscribeToAgentInstances();
    await this.subscribeToThreadChanges();
    await this.subscribeToResponses();
  }

  private scheduleReconnect() {
    if (this.reconnectInterval) {
      clearInterval(this.reconnectInterval);
    }
    this.reconnectInterval = setInterval(() => {
      void this.refreshSupabaseConnection();
    }, AgentManager.RECONNECT_INTERVAL_MS);
  }

  private async stopRealtimeSubscriptions() {
    const subscriptions = [...this.subscriptions];
    this.subscriptions = [];
    await Promise.all(subscriptions.map((channel) => channel.unsubscribe()));
  }

  private async refreshSupabaseConnection() {
    if (this.isRefreshing) {
      return;
    }

    this.isRefreshing = true;
    console.log("[AgentManager] Refreshing Supabase connection…");
    try {
      await this.stopRealtimeSubscriptions();
      this.supabase = this.createSupabaseClient();
      this.ptolemy.setSupabaseClient(this.supabase);
      await this.bootstrapExistingAgents();
      await this.initializeRealtimeSubscriptions();
      console.log("[AgentManager] Supabase connection refreshed");
    } catch (error) {
      console.error(
        "[AgentManager] Failed to refresh Supabase connection:",
        error,
      );
    } finally {
      this.isRefreshing = false;
    }
  }
}
