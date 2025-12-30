export interface CreateSessionRequest {
  title: string;
  purpose: string;
  background: string;
  hostUserId: string;
  initialQuestions?: string[];
}

export interface CreateSessionResponse {
  sessionId: string;
  adminAccessToken: string;
}
