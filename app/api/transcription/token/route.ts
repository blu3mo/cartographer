import { NextResponse } from "next/server";

/**
 * ElevenLabs single-use token endpoint for real-time transcription
 * Creates a short-lived token (15 minutes) for browser-based WebSocket connections
 */
export async function POST() {
  try {
    const apiKey = process.env.ELEVENLABS_API_KEY;

    if (!apiKey) {
      console.error("ELEVENLABS_API_KEY is not configured");
      return NextResponse.json(
        { error: "API key not configured" },
        { status: 500 },
      );
    }

    // Request a single-use token from ElevenLabs
    const response = await fetch(
      "https://api.elevenlabs.io/v1/single-use-token/realtime_scribe",
      {
        method: "POST",
        headers: {
          "xi-api-key": apiKey,
        },
      },
    );

    if (!response.ok) {
      const errorText = await response.text();
      console.error("ElevenLabs token request failed:", errorText);
      return NextResponse.json(
        { error: "Failed to generate token" },
        { status: response.status },
      );
    }

    const data = await response.json();

    return NextResponse.json({ token: data.token });
  } catch (error) {
    console.error("Error generating ElevenLabs token:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}
