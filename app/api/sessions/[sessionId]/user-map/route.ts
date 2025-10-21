import { NextRequest, NextResponse } from "next/server";
import { getUserIdFromRequest } from "@/lib/auth";
import { prisma } from "@/lib/prisma";
import { PCA } from "ml-pca";

interface ParticipantPoint {
  id: string;
  name: string;
  x: number;
  y: number;
  responseCount: number;
}

interface TopStatement {
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
      stds[j] += Math.pow(data[i][j] - means[j], 2);
    }
    stds[j] = Math.sqrt(stds[j] / n);
    if (stds[j] === 0) stds[j] = 1; // Prevent division by zero
  }

  // Standardize
  const standardized = data.map((row) =>
    row.map((val, j) => (val - means[j]) / stds[j])
  );

  return standardized;
}

export async function GET(
  request: NextRequest,
  { params }: { params: Promise<{ sessionId: string }> }
) {
  try {
    const { sessionId } = await params;
    const userId = getUserIdFromRequest(request);

    if (!userId) {
      return NextResponse.json(
        { error: "Unauthorized: User ID not found" },
        { status: 401 }
      );
    }

    // Verify that the user is the host of this session
    const session = await prisma.session.findUnique({
      where: { id: sessionId },
    });

    if (!session) {
      return NextResponse.json(
        { error: "Session not found" },
        { status: 404 }
      );
    }

    if (session.hostUserId !== userId) {
      return NextResponse.json(
        { error: "Forbidden: You are not the host of this session" },
        { status: 403 }
      );
    }

    // Fetch all statements for this session
    const statements = await prisma.statement.findMany({
      where: { sessionId },
      orderBy: { orderIndex: "asc" },
    });

    if (statements.length === 0) {
      return NextResponse.json(
        { error: "No statements found for this session" },
        { status: 400 }
      );
    }

    // Fetch all participants
    const participants = await prisma.participant.findMany({
      where: { sessionId },
      include: {
        responses: {
          where: { sessionId },
        },
      },
    });

    if (participants.length < 3) {
      return NextResponse.json(
        {
          error:
            "Not enough participants for PCA analysis. At least 3 participants are required.",
        },
        { status: 400 }
      );
    }

    // Build response matrix
    // Rows: participants, Columns: statements
    const statementIds = statements.map((s: { id: string }) => s.id);
    const participantData: {
      userId: string;
      name: string;
      responses: number[];
      responseCount: number;
    }[] = [];

    for (const participant of participants) {
      const responseMap = new Map<string, number>();
      participant.responses.forEach((r: { statementId: string; value: number }) => {
        responseMap.set(r.statementId, r.value);
      });

      const responseVector = statementIds.map((stmtId: string) => {
        return responseMap.get(stmtId) ?? 0; // Fill missing responses with 0
      });

      const responseCount = participant.responses.length;

      // Only include participants with at least 1 response
      if (responseCount > 0) {
        participantData.push({
          userId: participant.userId,
          name: participant.name,
          responses: responseVector,
          responseCount,
        });
      }
    }

    if (participantData.length < 3) {
      return NextResponse.json(
        {
          error:
            "Not enough participants with responses for PCA analysis. At least 3 participants with responses are required.",
        },
        { status: 400 }
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
        { status: 500 }
      );
    }

    console.log("Loadings dimensions:", loadings.rows, "x", loadings.columns);
    console.log("Number of statements:", statements.length);
    console.log("Number of components:", nComponents);

    // Find top statements for each component
    const getTopStatements = (componentIndex: number, topN: number): TopStatement[] => {
      const loadingsForComponent = statements.map((stmt: { text: string }, idx: number) => {
        // Get the loading value for this statement and component
        const loadingValue = loadings.get(idx, componentIndex);
        return {
          text: stmt.text,
          loading: loadingValue,
        };
      });

      // Sort by absolute value of loading (descending)
      loadingsForComponent.sort((a: TopStatement, b: TopStatement) => Math.abs(b.loading) - Math.abs(a.loading));

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
      })
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
      totalStatements: statements.length,
    };

    return NextResponse.json({ data: result });
  } catch (error) {
    console.error("Error generating user map:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 }
    );
  }
}
