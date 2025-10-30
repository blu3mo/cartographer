import { type NextRequest, NextResponse } from "next/server";

import { getUserIdFromRequest } from "@/lib/auth";
import { generateSessionGoal } from "@/lib/llm";

export async function POST(request: NextRequest) {
  try {
    const userId = getUserIdFromRequest(request);

    if (!userId) {
      return NextResponse.json(
        { error: "Unauthorized: Missing user ID" },
        { status: 401 },
      );
    }

    const body = await request.json();
    const {
      title,
      participants,
      perspectiveFocus,
      decision,
      trigger,
      insightTargets,
      background,
    } = body as {
      title?: unknown;
      participants?: unknown;
      perspectiveFocus?: unknown;
      decision?: unknown;
      trigger?: unknown;
      insightTargets?: unknown;
      background?: unknown;
    };

    if (typeof title !== "string" || title.trim().length === 0) {
      return NextResponse.json({ error: "Invalid title" }, { status: 400 });
    }

    if (
      typeof participants !== "string" ||
      participants.trim().length === 0
    ) {
      return NextResponse.json(
        { error: "Invalid value for participants" },
        { status: 400 },
      );
    }

    if (
      typeof decision !== "string" ||
      decision.trim().length === 0
    ) {
      return NextResponse.json(
        { error: "Invalid value for decision" },
        { status: 400 },
      );
    }

    if (typeof trigger !== "string" || trigger.trim().length === 0) {
      return NextResponse.json(
        { error: "Invalid value for trigger" },
        { status: 400 },
      );
    }

    if (
      perspectiveFocus !== undefined &&
      typeof perspectiveFocus !== "string"
    ) {
      return NextResponse.json(
        { error: "Invalid value for perspectiveFocus" },
        { status: 400 },
      );
    }

    if (
      !Array.isArray(insightTargets) ||
      insightTargets.length === 0 ||
      !insightTargets.every(
        (item) =>
          typeof item === "string" &&
          ["agreement", "difference", "unknown"].includes(item),
      )
    ) {
      return NextResponse.json(
        { error: "Invalid value for insightTargets" },
        { status: 400 },
      );
    }

    if (background !== undefined && typeof background !== "string") {
      return NextResponse.json(
        { error: "Invalid value for background" },
        { status: 400 },
      );
    }

    const sanitizedInsightTargets = Array.from(
      new Set(
        (insightTargets as string[]).map((item) => item.trim()),
      ),
    );

    const goal = await generateSessionGoal({
      title: title.trim(),
      participants: participants.trim(),
      perspectiveFocus:
        typeof perspectiveFocus === "string"
          ? perspectiveFocus.trim()
          : undefined,
      insightTargets: sanitizedInsightTargets,
      decision: decision.trim(),
      trigger: trigger.trim(),
      background: typeof background === "string" ? background : undefined,
    });

    return NextResponse.json({ goal });
  } catch (error) {
    console.error("Failed to generate session goal:", error);
    return NextResponse.json(
      { error: "Failed to generate session goal" },
      { status: 500 },
    );
  }
}
