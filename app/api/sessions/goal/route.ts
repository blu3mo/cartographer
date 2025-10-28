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
    const { title, focus, purpose, background } = body as {
      title?: unknown;
      focus?: unknown;
      purpose?: unknown;
      background?: unknown;
    };

    if (typeof title !== "string" || title.trim().length === 0) {
      return NextResponse.json({ error: "Invalid title" }, { status: 400 });
    }

    if (typeof focus !== "string" || focus.trim().length === 0) {
      return NextResponse.json(
        { error: "Invalid value for focus" },
        { status: 400 },
      );
    }

    if (typeof purpose !== "string" || purpose.trim().length === 0) {
      return NextResponse.json(
        { error: "Invalid value for purpose" },
        { status: 400 },
      );
    }

    if (background !== undefined && typeof background !== "string") {
      return NextResponse.json(
        { error: "Invalid value for background" },
        { status: 400 },
      );
    }

    const goal = await generateSessionGoal({
      title: title.trim(),
      focus: focus.trim(),
      purpose: purpose.trim(),
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
