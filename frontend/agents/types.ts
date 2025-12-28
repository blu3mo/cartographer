export type ThreadEventType =
  | "plan"
  | "survey"
  | "survey_analysis"
  | "user_message";

export type PtolemyState =
  | "CREATING_PLAN"
  | "WAITING_SURVEY"
  | "CREATING_SURVEY"
  | "COLLECTING_SURVEY"
  | "WAITING_ANALYSIS"
  | "CREATING_ANALYSIS"
  | "WAITING_PLAN";

export interface AgentInstanceRow {
  id: string;
  thread_id: string;
  agent_type: string;
  state: PtolemyState | string;
  state_payload: Record<string, unknown> | null;
}

export interface EventThreadRow {
  id: string;
  session_id: string;
  should_proceed: boolean;
}

export interface SessionRow {
  id: string;
  title: string;
  context: string;
  goal: string;
}

export type AgentRunResult =
  | { status: "transitioned" }
  | { status: "waiting" }
  | { status: "idle" };
