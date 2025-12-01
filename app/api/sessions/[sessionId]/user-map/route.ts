import { PCA } from "ml-pca";
import { type NextRequest, NextResponse } from "next/server";

import { getUserIdFromRequest } from "@/lib/auth";
import { supabase } from "@/lib/supabase";

interface ParticipantPoint {
  id: string;
  name: string;
  x: number;
  y: number;
  responseCount: number;
}

interface TopStatement {
  id: string;
  orderIndex: number;
  text: string;
  loading: number;
}

interface ComponentInfo {
  explainedVariance: number;
  topStatements: TopStatement[];
}

interface UserMapData {
  participants: ParticipantPoint[];
  pc1: ComponentInfo;
  pc2: ComponentInfo;
  totalStatements: number;
}

function insufficientDataResponse(message: string) {
  return NextResponse.json({
    status: "insufficient-data",
    reason: message,
  });
}

/**
 * Standardize data (mean=0, std=1)
 */
function standardize(data: number[][]): number[][] {
  const n = data.length;
  const m = data[0].length;

  if (n === 0 || m === 0) return data;

  const means = new Array(m).fill(0);
  const stds = new Array(m).fill(0);

  // Calculate means
  for (let j = 0; j < m; j++) {
    for (let i = 0; i < n; i++) {
      means[j] += data[i][j];
    }
    means[j] /= n;
  }

  // Calculate standard deviations
  for (let j = 0; j < m; j++) {
    for (let i = 0; i < n; i++) {
      stds[j] += (data[i][j] - means[j]) ** 2;
    }
    stds[j] = Math.sqrt(stds[j] / n);
    if (stds[j] === 0) stds[j] = 1; // Prevent division by zero
  }

  // Standardize
  const standardized = data.map((row) =>
    row.map((val, j) => (val - means[j]) / stds[j]),
  );

  return standardized;
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
        { error: "Unauthorized: User ID not found" },
        { status: 401 },
      );
    }

    const { data: session, error: sessionError } = await supabase
      .from("sessions")
      .select("id, host_user_id")
      .eq("id", sessionId)
      .single();

    if (sessionError || !session) {
      if (sessionError?.code === "PGRST116") {
        return NextResponse.json(
          { error: "Session not found" },
          { status: 404 },
        );
      }
      console.error("Failed to load session for user map:", sessionError);
      return NextResponse.json(
        { error: "Internal server error" },
        { status: 500 },
      );
    }

    if (session.host_user_id !== userId) {
      return NextResponse.json(
        { error: "Forbidden: You are not the host of this session" },
        { status: 403 },
      );
    }

    const { data: statements, error: statementsError } = await supabase
      .from("statements")
      .select("id, text, order_index")
      .eq("session_id", sessionId)
      .order("order_index", { ascending: true });

    if (statementsError) {
      console.error(
        "Failed to fetch statements for user map:",
        statementsError,
      );
      return NextResponse.json(
        { error: "Internal server error" },
        { status: 500 },
      );
    }

    if ((statements ?? []).length === 0) {
      return insufficientDataResponse(
        "このセッションには質問がまだ設定されていません。",
      );
    }

    const { data: participants, error: participantsError } = await supabase
      .from("participants")
      .select("user_id, name")
      .eq("session_id", sessionId);

    if (participantsError) {
      console.error(
        "Failed to fetch participants for user map:",
        participantsError,
      );
      return NextResponse.json(
        { error: "Internal server error" },
        { status: 500 },
      );
    }

    if ((participants ?? []).length < 3) {
      return insufficientDataResponse(
        "PCA分析を実行するには最低3人の参加者が必要です。",
      );
    }

    const { data: responses, error: responsesError } = await supabase
      .from("responses")
      .select("participant_user_id, statement_id, value, response_type")
      .eq("session_id", sessionId)
      .eq("response_type", "scale");

    if (responsesError) {
      console.error("Failed to fetch responses for user map:", responsesError);
      return NextResponse.json(
        { error: "Internal server error" },
        { status: 500 },
      );
    }

    const statementIds = (statements ?? []).map((statement) => statement.id);
    const participantData: {
      userId: string;
      name: string;
      responses: number[];
      responseCount: number;
    }[] = [];

    const responsesByParticipant = new Map<string, Map<string, number>>();
    (responses ?? []).forEach((response) => {
      const participantResponses =
        responsesByParticipant.get(response.participant_user_id) ??
        new Map<string, number>();
      participantResponses.set(response.statement_id, response.value);
      responsesByParticipant.set(
        response.participant_user_id,
        participantResponses,
      );
    });

    for (const participant of participants ?? []) {
      const responseMap =
        responsesByParticipant.get(participant.user_id) ??
        new Map<string, number>();

      const responseVector = statementIds.map((stmtId) => {
        return responseMap.get(stmtId) ?? 0; // Fill missing responses with 0
      });

      const responseCount = responseMap.size;

      // Only include participants with at least 1 response
      if (responseCount > 0) {
        participantData.push({
          userId: participant.user_id,
          name: participant.name,
          responses: responseVector,
          responseCount,
        });
      }
    }

    if (participantData.length < 3) {
      return insufficientDataResponse(
        "PCA分析を実行するには最低3人の参加者が回答する必要があります。",
      );
    }

    // Extract response matrix
    const responseMatrix = participantData.map((p) => p.responses);

    // Standardize data
    const standardizedData = standardize(responseMatrix);

    // Perform PCA (we already standardized, so turn off auto-centering/scaling)
    const pca = new PCA(standardizedData, { center: false, scale: false });
    const nComponents = Math.min(2, participantData.length - 1);
    const transformed = pca.predict(standardizedData, { nComponents });
    const explainedVariance = pca.getExplainedVariance();

    // Get loadings (eigenvectors)
    const loadings = pca.getLoadings();

    // Debug: check if loadings is defined and log dimensions
    if (!loadings) {
      console.error("Loadings is undefined");
      return NextResponse.json(
        { error: "Failed to compute PCA loadings" },
        { status: 500 },
      );
    }

    console.log("Loadings dimensions:", loadings.rows, "x", loadings.columns);
    console.log("Number of statements:", statements.length);
    console.log("Number of components:", nComponents);

    // Find top statements for each component
    const getTopStatements = (
      componentIndex: number,
      topN: number,
    ): TopStatement[] => {
      const loadingsForComponent = (statements ?? []).map(
        (stmt: { id: string; text: string; order_index: number }, idx: number) => {
          // Get the loading value for this statement and component
          // Loadings matrix is (components x features), so we access as (componentIndex, statementIndex)
          const loadingValue = loadings.get(componentIndex, idx);
          return {
            id: stmt.id,
            orderIndex: stmt.order_index,
            text: stmt.text,
            loading: loadingValue,
          };
        },
      );

      // Sort by absolute value of loading (descending)
      loadingsForComponent.sort(
        (a: TopStatement, b: TopStatement) =>
          Math.abs(b.loading) - Math.abs(a.loading),
      );

      return loadingsForComponent.slice(0, topN);
    };

    const pc1TopStatements = getTopStatements(0, 2);
    const pc2TopStatements = nComponents >= 2 ? getTopStatements(1, 2) : [];

    // Build result
    const participantPoints: ParticipantPoint[] = participantData.map(
      (p, i) => ({
        id: p.userId,
        name: p.name,
        x: transformed.get(i, 0),
        y: nComponents >= 2 ? transformed.get(i, 1) : 0,
        responseCount: p.responseCount,
      }),
    );

    const result: UserMapData = {
      participants: participantPoints,
      pc1: {
        explainedVariance: explainedVariance[0] || 0,
        topStatements: pc1TopStatements,
      },
      pc2: {
        explainedVariance: explainedVariance[1] || 0,
        topStatements: pc2TopStatements,
      },
      totalStatements: (statements ?? []).length,
    };

    return NextResponse.json({ status: "ok", data: result });
  } catch (error) {
    console.error("Error generating user map:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}
