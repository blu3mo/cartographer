import { CreateSessionRequest, CreateSessionResponse } from "./types";

const BACKEND_URL = process.env.HASKELL_BACKEND_URL || "http://localhost:8080";

export async function createSession(
  request: CreateSessionRequest,
): Promise<CreateSessionResponse> {
  const response = await fetch(`${BACKEND_URL}/sessions`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify(request),
  });

  if (!response.ok) {
    const errorBody = await response.text();
    throw new Error(
      `Backend Error: ${response.status} ${response.statusText}\nBody: ${errorBody}`,
    );
  }

  return response.json();
}
