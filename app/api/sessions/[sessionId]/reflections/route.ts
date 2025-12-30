export const dynamic = "force-dynamic";

import { type NextRequest, NextResponse } from "next/server";

import { getUserIdFromRequest } from "@/lib/auth";
import { supabase } from "@/lib/supabase";

type ReflectionRow = {
  id: string;
  participant_user_id: string;
  session_id: string;
  text: string;
  created_at: string;
  submitted_at: string;
};

const mapReflection = (row: ReflectionRow) => ({
  id: row.id,
  text: row.text,
  createdAt: row.created_at,
  submittedAt: row.submitted_at,
});

async function ensureParticipant(sessionId: string, userId: string) {
  const { data: participant, error } = await supabase
    .from("participants")
    .select("user_id")
    .eq("user_id", userId)
    .eq("session_id", sessionId)
    .maybeSingle();

  if (error) {
    console.error("Failed to verify participant:", error);
    throw new Error("participant_verification_failed");
  }

  if (!participant) {
    throw new Error("participant_not_found");
  }
}

export async function GET(
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
        { error: "Failed to fetch reflections" },
        { status: 500 },
      );
    }

    const { data, error: reflectionsError } = await supabase
      .from("participant_reflections")
      .select(
        `
          id,
          participant_user_id,
          session_id,
          text,
          created_at,
          submitted_at
        `,
      )
      .eq("participant_user_id", userId)
      .eq("session_id", sessionId)
      .order("submitted_at", { ascending: false });

    if (reflectionsError) {
      console.error(
        "Failed to fetch participant reflections:",
        reflectionsError,
      );
      return NextResponse.json(
        { error: "Failed to fetch reflections" },
        { status: 500 },
      );
    }

    return NextResponse.json({
      reflections: (data ?? []).map((row) =>
        mapReflection(row as ReflectionRow),
      ),
    });
  } catch (error) {
    console.error("Failed to fetch participant reflections:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
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
        { error: "Failed to submit reflection" },
        { status: 500 },
      );
    }

    const body = await request.json();
    const { text } = body as { text?: unknown };

    if (typeof text !== "string") {
      return NextResponse.json(
        { error: "Invalid value for text" },
        { status: 400 },
      );
    }

    const normalizedText = text.trim();

    const { data, error: insertError } = await supabase
      .from("participant_reflections")
      .insert({
        participant_user_id: userId,
        session_id: sessionId,
        text: normalizedText,
      })
      .select(
        `
          id,
          participant_user_id,
          session_id,
          text,
          created_at,
          submitted_at
        `,
      )
      .single();

    if (insertError || !data) {
      console.error("Failed to insert participant reflection:", insertError);
      return NextResponse.json(
        { error: "Failed to submit reflection" },
        { status: 500 },
      );
    }

    return NextResponse.json({
      reflection: mapReflection(data as ReflectionRow),
    });
  } catch (error) {
    console.error("Failed to submit participant reflection:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}
