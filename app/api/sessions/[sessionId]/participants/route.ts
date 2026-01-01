export const dynamic = "force-dynamic";

import { type NextRequest, NextResponse } from "next/server";

import { getUserIdFromRequest } from "@/lib/auth";
import { supabase } from "@/lib/supabase";

type ParticipantRow = {
  user_id: string;
  session_id: string;
  name: string;
  latest_individual_report_id: string | null;
  created_at: string;
  updated_at: string;
};

function mapParticipant(row: ParticipantRow) {
  return {
    userId: row.user_id,
    sessionId: row.session_id,
    name: row.name,
    latestIndividualReportId: row.latest_individual_report_id,
    createdAt: row.created_at,
    updatedAt: row.updated_at,
  };
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
    const { name } = body;

    if (!name) {
      return NextResponse.json(
        { error: "Missing required field: name" },
        { status: 400 },
      );
    }

    const { data: session, error: sessionError } = await supabase
      .from("sessions")
      .select("id")
      .eq("id", sessionId)
      .single();

    if (sessionError || !session) {
      if (sessionError?.code === "PGRST116") {
        return NextResponse.json(
          { error: "Session not found" },
          { status: 404 },
        );
      }
      console.error("Failed to verify session:", sessionError);
      return NextResponse.json({ error: "Session not found" }, { status: 404 });
    }

    const { data: existingParticipant, error: existingParticipantError } =
      await supabase
        .from("participants")
        .select("user_id")
        .eq("user_id", userId)
        .eq("session_id", sessionId)
        .maybeSingle();

    if (existingParticipantError) {
      console.error(
        "Failed to check participant membership:",
        existingParticipantError,
      );
      return NextResponse.json(
        { error: "Failed to join session" },
        { status: 500 },
      );
    }

    if (existingParticipant) {
      return NextResponse.json(
        { error: "Already participating in this session" },
        { status: 409 },
      );
    }

    const { data: participant, error: createParticipantError } = await supabase
      .from("participants")
      .insert({
        user_id: userId,
        session_id: sessionId,
        name,
      })
      .select(
        "user_id, session_id, name, latest_individual_report_id, created_at, updated_at",
      )
      .single();

    if (createParticipantError || !participant) {
      console.error("Failed to create participant:", createParticipantError);
      return NextResponse.json(
        { error: "Failed to join session" },
        { status: 500 },
      );
    }

    return NextResponse.json({ participant: mapParticipant(participant) });
  } catch (error) {
    console.error("Participant registration error:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}
