"use client";

import axios from "axios";
import {
  Activity,
  ArrowLeft,
  ChevronRight,
  Info,
  Loader2,
  Map,
  Printer,
} from "lucide-react";
import { use, useCallback, useEffect, useMemo, useState } from "react";
import ReactMarkdown from "react-markdown";
import {
  ResponsiveContainer,
  Scatter,
  ScatterChart,
  Tooltip,
  XAxis,
  YAxis,
  ZAxis,
} from "recharts";
import remarkGfm from "remark-gfm";

import { Button } from "@/components/ui/Button";
import { useUserId } from "@/lib/useUserId";

type SessionReportStatus = "pending" | "generating" | "completed" | "failed";

interface SessionReport {
  id: string;
  sessionId: string;
  version: number;
  status: SessionReportStatus;
  requestMarkdown: string;
  contentMarkdown: string | null;
  createdBy: string;
  model: string;
  errorMessage: string | null;
  createdAt: string;
  updatedAt: string;
  completedAt: string | null;
}

/**
 * レポート内ビジュアライズ用のデータ構造（実データ連携）
 */

interface ParticipantNode {
  id: string;
  name: string;
  role: string;
  avatarColor: string;
  x: number;
  y: number;
  clusterId: string;
}

interface StatementNode {
  id: string;
  text: string;
  category: string | null;
  consensusScore: number; // 0~1, 1がコンセンサス度高
  approvalRate: number; // 0~1, 賛成率
  voteCount: number; // 回答数
  isDivisive: boolean; // 分断かどうか
}

interface ClusterInfo {
  id: string;
  label: string;
  color: string;
  description: string;
}

interface UserMapApiResponse {
  status: "ok";
  data: {
    participants: Array<{
      id: string;
      name: string;
      x: number;
      y: number;
      responseCount: number;
    }>;
    pc1: {
      explainedVariance: number;
      topStatements: Array<{
        id: string;
        orderIndex: number;
        text: string;
        loading: number;
      }>;
    };
    pc2: {
      explainedVariance: number;
      topStatements: Array<{
        id: string;
        orderIndex: number;
        text: string;
        loading: number;
      }>;
    };
    totalStatements: number;
  };
}

interface AdminSessionResponse {
  data: {
    statements: Array<{
      id: string;
      text: string;
      orderIndex: number;
      responses: {
        strongYes: number;
        yes: number;
        dontKnow: number;
        no: number;
        strongNo: number;
        totalCount: number;
        freeTextCount: number;
      };
      agreementScore: number;
    }>;
  };
}

const OpinionMap = ({
  onSelectUser,
  participants,
  axisMeaning,
}: {
  onSelectUser: (u: ParticipantNode) => void;
  participants: ParticipantNode[];
  axisMeaning: {
    x: { positive: string; negative: string };
    y: { positive: string; negative: string };
  };
}) => {
  return (
    <div className="h-full w-full relative">
      <div className="absolute inset-0 pointer-events-none flex flex-col justify-between p-8 z-0 opacity-60">
        <div className="flex justify-center h-full relative">
          <span className="absolute top-0 bg-indigo-50 text-indigo-700 px-3 py-1 rounded-full text-xs font-bold border border-indigo-100 shadow-sm">
            ▲ {axisMeaning.y.positive}
          </span>
          <span className="absolute bottom-4 bg-slate-50 text-slate-600 px-3 py-1 rounded-full text-xs font-bold border border-slate-200">
            ▼ {axisMeaning.y.negative}
          </span>
          <div className="h-full w-px border-l border-dashed border-gray-300 absolute left-1/2 -translate-x-1/2" />
        </div>
      </div>

      <div className="absolute inset-0 pointer-events-none flex items-center justify-between p-8 z-0 opacity-60">
        <div className="w-full flex justify-between items-center relative">
          <span className="absolute left-0 bg-slate-50 text-slate-600 px-3 py-1 rounded-full text-xs font-bold border border-slate-200">
            ◀ {axisMeaning.x.negative}
          </span>
          <span className="absolute right-0 bg-indigo-50 text-indigo-700 px-3 py-1 rounded-full text-xs font-bold border border-indigo-100 shadow-sm">
            {axisMeaning.x.positive} ▶
          </span>
          <div className="w-full h-px border-t border-dashed border-gray-300 absolute top-1/2 -translate-y-1/2" />
        </div>
      </div>

      <ResponsiveContainer width="100%" height="100%">
        <ScatterChart margin={{ top: 40, right: 40, bottom: 40, left: 40 }}>
          <XAxis type="number" dataKey="x" domain={[-2.5, 2.5]} hide />
          <YAxis type="number" dataKey="y" domain={[-2.5, 2.5]} hide />
          <Tooltip
            cursor={{ strokeDasharray: "3 3" }}
            content={({ active, payload }) => {
              if (active && payload && payload.length) {
                const data = payload[0].payload as ParticipantNode;
                return (
                  <div className="bg-white p-2 border rounded shadow text-xs">
                    <div className="font-bold flex items-center gap-1">
                      <span
                        className="w-2 h-2 rounded-full"
                        style={{ background: data.avatarColor }}
                      />
                      {data.name}
                    </div>
                    <div className="text-gray-500">{data.role}</div>
                  </div>
                );
              }
              return null;
            }}
          />
          <ZAxis range={[100, 100]} />
          <Scatter
            data={participants}
            onClick={(node) => onSelectUser(node.payload as ParticipantNode)}
            shape={(props: unknown) => {
              const typed = props as { cx: number; cy: number; payload: ParticipantNode };
              const { cx, cy, payload } = typed;
              return (
                <g transform={`translate(${cx},${cy})`} style={{ cursor: "pointer" }}>
                  <circle
                    cx={0}
                    cy={0}
                    r={18}
                    fill="white"
                    stroke={payload.avatarColor}
                    strokeWidth={2}
                  />
                  <text
                    x={0}
                    y={0}
                    dy={5}
                    textAnchor="middle"
                    fill={payload.avatarColor}
                    fontSize={12}
                    fontWeight="bold"
                    style={{ userSelect: "none" }}
                  >
                    {payload.name.charAt(0)}
                  </text>
                </g>
              );
            }}
          />
        </ScatterChart>
      </ResponsiveContainer>
    </div>
  );
};

const ConsensusBeeswarm = ({
  onSelectStatement,
  statements,
}: {
  onSelectStatement: (s: StatementNode) => void;
  statements: StatementNode[];
}) => {
  const sorted = useMemo(
    () => [...statements].sort((a, b) => a.approvalRate - b.approvalRate),
    [statements],
  );

  const jittered = useMemo(
    () =>
      sorted.map((stmt, i) => {
        const direction = i % 2 === 0 ? 1 : -1;
        const magnitude = 12 + ((i * 17) % 45);
        return { ...stmt, jitter: direction * magnitude };
      }),
    [sorted],
  );

  return (
    <div className="h-full w-full flex flex-col justify-center px-4 relative">
      <div className="absolute top-4 left-4 right-4 flex justify-between text-xs font-bold text-gray-500">
        <span className="flex items-center gap-1">
          <span className="w-2 h-2 rounded-full bg-red-500" />
          全員反対 (Disagreement)
        </span>
        <span className="flex items-center gap-1 text-amber-500">
          <span className="w-2 h-2 rounded-full bg-amber-500" />
          意見が割れる (Divisive)
        </span>
        <span className="flex items-center gap-1">
          <span className="w-2 h-2 rounded-full bg-green-500" />
          全員賛成 (Consensus)
        </span>
      </div>

      <div className="relative h-64 w-full mt-6">
        <div className="absolute top-1/2 left-4 right-4 h-2 bg-gradient-to-r from-red-200 via-amber-100 to-green-200 rounded-full opacity-60" />
        <div className="absolute top-1/2 left-4 right-4 h-px bg-gray-300 -translate-y-1/2" />

        {jittered.map((stmt) => {
          const xPercent = stmt.approvalRate * 100;
          const color = stmt.isDivisive
            ? "#f59e0b"
            : stmt.approvalRate > 0.5
              ? "#10b981"
              : "#ef4444";

          return (
            <div
              key={stmt.id}
              className="absolute -translate-x-1/2 -translate-y-1/2 transition-all duration-300 hover:scale-125 hover:z-50 group cursor-pointer"
              style={{ left: `${xPercent}%`, top: `calc(50% + ${stmt.jitter}px)` }}
              onClick={() => onSelectStatement(stmt)}
            >
              <div
                className="w-6 h-6 rounded-full border-2 border-white shadow-md flex items-center justify-center text-[10px] font-bold text-white transition-shadow hover:shadow-lg"
                style={{ backgroundColor: color }}
              >
                {stmt.id.replace("#", "")}
              </div>

              <div className="absolute bottom-full left-1/2 -translate-x-1/2 mb-2 w-64 bg-gray-900 text-white text-xs p-3 rounded shadow-xl opacity-0 group-hover:opacity-100 pointer-events-none z-50 transition-opacity">
                <div className="flex justify-between items-center mb-1 border-b border-gray-700 pb-1">
                  <span className="font-bold text-amber-400">{stmt.id}</span>
                  <span className="text-gray-300">
                    賛成率: {Math.round(stmt.approvalRate * 100)}%
                  </span>
                </div>
                <div className="leading-relaxed">{stmt.text}</div>
              </div>
            </div>
          );
        })}
      </div>

      <div className="text-center text-xs text-gray-500 mt-8 leading-relaxed">
        左右の位置は「賛成率」を表します。中央付近にある項目は意見が割れている論点です。
        <br />
        円をクリックすると詳細が表示されます。
      </div>
    </div>
  );
};

const VisualDeliberationSection = ({
  sessionId,
  accessToken,
  userId,
}: {
  sessionId: string;
  accessToken: string;
  userId: string;
}) => {
  const [activeTab, setActiveTab] = useState<"map" | "beeswarm">("map");
  const [selectedItem, setSelectedItem] = useState<ParticipantNode | StatementNode | null>(null);
  const [participants, setParticipants] = useState<ParticipantNode[]>([]);
  const [statements, setStatements] = useState<StatementNode[]>([]);
  const [axisMeaning, setAxisMeaning] = useState<{
    x: { positive: string; negative: string };
    y: { positive: string; negative: string };
  }>({
    x: { positive: "PC1+", negative: "PC1-" },
    y: { positive: "PC2+", negative: "PC2-" },
  });
  const [clusters, setClusters] = useState<ClusterInfo[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const getPalette = (index: number) => {
    const colors = ["#10b981", "#3b82f6", "#f59e0b", "#ef4444", "#8b5cf6", "#14b8a6"];
    return colors[index % colors.length];
  };

  const fetchData = useCallback(
    async (sessionId: string, accessToken: string, userId: string) => {
      try {
        setLoading(true);
        setError(null);

        const [mapRes, adminRes] = await Promise.all([
          axios.get<UserMapApiResponse>(`/api/sessions/${sessionId}/user-map`, {
            headers: { Authorization: `Bearer ${userId}` },
          }),
          axios.get<AdminSessionResponse>(
            `/api/sessions/${sessionId}/${accessToken}`,
            {
              headers: { Authorization: `Bearer ${userId}` },
            },
          ),
        ]);

        const mapPayload = mapRes.data?.data;
        const adminPayload = adminRes.data?.data;

        if (mapPayload) {
          const computeAnchors = (items: Array<{ text: string; loading: number }>) => {
            const positive = [...items]
              .filter((s) => s.loading > 0)
              .sort((a, b) => Math.abs(b.loading) - Math.abs(a.loading))[0];
            const negative = [...items]
              .filter((s) => s.loading < 0)
              .sort((a, b) => Math.abs(b.loading) - Math.abs(a.loading))[0];
            return {
              positive: positive?.text ?? "プラス方向に寄与する設問",
              negative: negative?.text ?? "マイナス方向に寄与する設問",
            };
          };

          const pc1Anchors = computeAnchors(mapPayload.pc1.topStatements);
          const pc2Anchors = computeAnchors(mapPayload.pc2.topStatements);
          setAxisMeaning({
            x: pc1Anchors,
            y: pc2Anchors,
          });

          const quadrantMeta: ClusterInfo[] = [
            {
              id: "q1",
              label: `${pc1Anchors.positive} × ${pc2Anchors.positive}`,
              color: "#fef3c7",
              description: "PC1/PC2 が正の領域に位置する参加者",
            },
            {
              id: "q2",
              label: `${pc1Anchors.negative} × ${pc2Anchors.positive}`,
              color: "#e0f2fe",
              description: "PC1 は負、PC2 は正の領域に位置する参加者",
            },
            {
              id: "q3",
              label: `${pc1Anchors.negative} × ${pc2Anchors.negative}`,
              color: "#ede9fe",
              description: "PC1/PC2 が負の領域に位置する参加者",
            },
            {
              id: "q4",
              label: `${pc1Anchors.positive} × ${pc2Anchors.negative}`,
              color: "#dcfce7",
              description: "PC1 は正、PC2 は負の領域に位置する参加者",
            },
          ];
          setClusters(quadrantMeta);

          const nodes: ParticipantNode[] = mapPayload.participants.map((p, idx) => {
            const quad =
              p.x >= 0 && p.y >= 0
                ? "q1"
                : p.x < 0 && p.y >= 0
                  ? "q2"
                  : p.x < 0 && p.y < 0
                    ? "q3"
                    : "q4";
            return {
              id: p.id,
              name: p.name,
              role: "参加者",
              avatarColor: getPalette(idx),
              x: p.x,
              y: p.y,
              clusterId: quad,
            };
          });
          setParticipants(nodes);
        }

        if (adminPayload?.statements) {
          const mappedStatements: StatementNode[] = adminPayload.statements.map((stmt) => {
            const positivePercent = stmt.responses.strongYes + stmt.responses.yes;
            const negativePercent = stmt.responses.strongNo + stmt.responses.no;
            const approvalRate = positivePercent / 100;
            const consensusScore = 1 - Math.min(1, Math.abs(positivePercent - negativePercent) / 100);
            const voteCount = stmt.responses.totalCount;
            const isDivisive = voteCount > 0 && consensusScore < 0.55;

            return {
              id: `#${stmt.orderIndex + 1}`,
              text: stmt.text,
              category: null,
              consensusScore,
              approvalRate: Number.isFinite(approvalRate) ? approvalRate : 0,
              voteCount,
              isDivisive,
            };
          });
          setStatements(mappedStatements);
        }
      } catch (err) {
        console.error("Failed to load visualization data:", err);
        setError("可視化用データの取得に失敗しました。");
      } finally {
        setLoading(false);
      }
    },
    [],
  );

  useEffect(() => {
    if (!sessionId || !accessToken || !userId) return;
    void fetchData(sessionId, accessToken, userId);
  }, [accessToken, fetchData, sessionId, userId]);

  const selectedCluster = useMemo(() => {
    if (!selectedItem || !("clusterId" in selectedItem)) return null;
    return clusters.find((c) => c.id === selectedItem.clusterId) ?? null;
  }, [clusters, selectedItem]);

  if (loading) {
    return (
      <div className="w-full bg-gray-50 rounded-2xl border border-slate-200 p-6 flex items-center justify-center text-sm text-gray-500">
        <Loader2 className="h-4 w-4 animate-spin mr-2 text-indigo-500" />
        可視化データを読み込み中…
      </div>
    );
  }

  if (error) {
    return (
      <div className="w-full bg-rose-50 text-rose-700 rounded-2xl border border-rose-200 p-4 text-sm">
        {error}
      </div>
    );
  }

  return (
    <div className="w-full bg-gray-50 rounded-2xl border border-slate-200 p-4 lg:p-6">
      <div className="flex flex-col lg:flex-row lg:items-center justify-between mb-4 gap-3">
        <div>
          <h2 className="text-xl font-semibold text-gray-900 flex items-center gap-2">
            <Activity className="w-5 h-5 text-indigo-600" />
            Visual Deliberation
            <span className="text-[11px] font-medium text-gray-500 bg-gray-200 px-2 py-0.5 rounded-full">
              Beta
            </span>
          </h2>
          <p className="text-gray-500 text-xs mt-1">
            議論構造の2軸マップと合意スペクトラムを合わせて表示します。
          </p>
        </div>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-12 gap-4">
        <div className="lg:col-span-4 flex flex-col gap-3">
          <div className="bg-white rounded-xl shadow-sm border border-gray-200 p-2 grid grid-cols-2 gap-2">
            <button
              type="button"
              onClick={() => setActiveTab("map")}
              className={`flex flex-col items-center justify-center p-3 rounded-lg transition-all ${
                activeTab === "map"
                  ? "bg-indigo-50 text-indigo-700 border border-indigo-100 shadow-sm"
                  : "text-gray-500 hover:bg-gray-50"
              }`}
            >
              <Map className="w-5 h-5 mb-1" />
              <span className="text-[11px] font-bold">Opinion Map</span>
            </button>

            <button
              type="button"
              onClick={() => setActiveTab("beeswarm")}
              className={`flex flex-col items-center justify-center p-3 rounded-lg transition-all ${
                activeTab === "beeswarm"
                  ? "bg-indigo-50 text-indigo-700 border border-indigo-100 shadow-sm"
                  : "text-gray-500 hover:bg-gray-50"
              }`}
            >
              <Activity className="w-5 h-5 mb-1" />
              <span className="text-[11px] font-bold">Consensus Spectrum</span>
            </button>
          </div>

          <div className="bg-white rounded-xl shadow-sm border border-gray-200 p-5 flex-1 min-h-[260px]">
            <h3 className="text-[11px] font-semibold uppercase tracking-wider mb-4 flex items-center gap-2 border-b border-gray-100 pb-2 text-gray-500">
              <Info className="w-4 h-4" />
              詳細情報
            </h3>

            {selectedItem ? (
              <div className="animate-in fade-in slide-in-from-bottom-2 duration-300 space-y-4">
                {"text" in selectedItem ? (
                  <div>
                    <div className="flex items-center justify-between mb-3">
                      <div className="text-[11px] font-bold text-white bg-indigo-600 px-2 py-1 rounded-full shadow-sm">
                        Statement {selectedItem.id}
                      </div>
                      <span
                        className={`text-[11px] font-bold px-2 py-1 rounded border ${
                          selectedItem.isDivisive
                            ? "text-amber-600 bg-amber-50 border-amber-200"
                            : "text-green-600 bg-green-50 border-green-200"
                        }`}
                      >
                        {selectedItem.isDivisive ? "意見が割れています" : "合意傾向あり"}
                      </span>
                    </div>

                    <p className="font-bold text-gray-800 text-base leading-snug">
                      {selectedItem.text}
                    </p>

                    <div className="space-y-3 bg-gray-50 p-4 rounded-lg border border-gray-100">
                      <div>
                        <div className="flex justify-between text-[11px] text-gray-500 mb-1">
                          <span>賛成率</span>
                          <span className="font-bold text-gray-700">
                            {Math.round(selectedItem.approvalRate * 100)}%
                          </span>
                        </div>
                        <div className="h-2.5 w-full bg-gray-200 rounded-full overflow-hidden">
                          <div
                            className={`h-full rounded-full transition-all duration-500 ${
                              selectedItem.approvalRate > 0.5 ? "bg-green-500" : "bg-red-500"
                            }`}
                            style={{ width: `${selectedItem.approvalRate * 100}%` }}
                          />
                        </div>
                      </div>
                      <div className="grid grid-cols-2 gap-4 pt-2">
                        <div>
                          <div className="text-[11px] text-gray-400 mb-0.5">Category</div>
                          <div className="text-sm font-medium text-gray-700">
                            {selectedItem.category ?? "未分類"}
                          </div>
                        </div>
                        <div>
                          <div className="text-[11px] text-gray-400 mb-0.5">Vote Count</div>
                          <div className="text-sm font-medium text-gray-700">
                            {selectedItem.voteCount} 票
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                ) : (
                  <div className="space-y-4">
                    <div className="flex items-center gap-4">
                      <div
                        className="w-12 h-12 rounded-full flex items-center justify-center text-white text-lg font-bold shadow-md"
                        style={{ background: selectedItem.avatarColor }}
                      >
                        {selectedItem.name.charAt(0)}
                      </div>
                      <div>
                        <div className="text-lg font-bold text-gray-900">{selectedItem.name}</div>
                        <div className="text-xs text-gray-500 font-medium bg-gray-100 px-2 py-0.5 rounded inline-block mt-1">
                          {selectedItem.role}
                        </div>
                      </div>
                    </div>

                    {selectedCluster ? (
                      <div className="bg-indigo-50 border border-indigo-100 p-3 rounded-lg">
                        <div className="text-[11px] font-bold text-indigo-400 mb-1 uppercase">
                          所属クラスター
                        </div>
                        <div className="font-bold text-indigo-900">{selectedCluster.label}</div>
                        <div className="text-[11px] text-indigo-700 mt-1 leading-snug">
                          {selectedCluster.description}
                        </div>
                      </div>
                    ) : null}

                    <div className="bg-gray-50 border border-gray-100 p-3 rounded-lg">
                      <div className="text-[11px] font-bold text-gray-400 mb-1 uppercase">
                        PCA 座標
                      </div>
                      <div className="text-[11px] text-gray-600 font-mono">
                        PC1: {selectedItem.x.toFixed(2)}, PC2: {selectedItem.y.toFixed(2)}
                      </div>
                    </div>
                  </div>
                )}
              </div>
            ) : (
              <div className="h-full flex flex-col items-center justify-center text-gray-400 text-[11px] text-center min-h-[180px]">
                <div className="w-12 h-12 bg-gray-100 rounded-full flex items-center justify-center mb-3">
                  <ChevronRight className="w-6 h-6 opacity-40" />
                </div>
                左のチャート上の要素をクリックすると
                <br />
                ここに詳細情報が表示されます
              </div>
            )}
          </div>
        </div>

        <div className="lg:col-span-8 h-[520px] bg-white rounded-xl shadow-sm border border-gray-200 p-1 relative overflow-hidden">
          {activeTab === "map" ? (
            <OpinionMap
              participants={participants}
              axisMeaning={axisMeaning}
              onSelectUser={(u) => setSelectedItem(u)}
            />
          ) : statements.length > 0 ? (
            <ConsensusBeeswarm
              statements={statements}
              onSelectStatement={(s) => setSelectedItem(s)}
            />
          ) : (
            <div className="flex h-full items-center justify-center text-sm text-gray-500">
              賛成率を算出できるステートメントがまだありません
            </div>
          )}
        </div>
      </div>
    </div>
  );
};

export default function SessionReportPrintPage({
  params,
}: {
  params: Promise<{
    sessionId: string;
    accessToken: string;
    reportId: string;
  }>;
}) {
  const { sessionId, accessToken, reportId } = use(params);
  const { userId, isLoading: isUserLoading } = useUserId();
  const [report, setReport] = useState<SessionReport | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (isUserLoading || !userId) return;

    const fetchReport = async () => {
      try {
        setLoading(true);
        const response = await axios.get(
          `/api/sessions/${sessionId}/${accessToken}/reports/${reportId}`,
          {
            headers: { Authorization: `Bearer ${userId}` },
          },
        );
        setReport(response.data.data as SessionReport);
        setError(null);
      } catch (err) {
        console.error("Failed to load report:", err);
        setError("レポートの取得に失敗しました。");
      } finally {
        setLoading(false);
      }
    };

    void fetchReport();
  }, [sessionId, accessToken, reportId, userId, isUserLoading]);

  const handlePrint = () => {
    window.print();
  };

  if (loading) {
    return (
      <div className="min-h-screen bg-white flex items-center justify-center">
        <Loader2 className="h-6 w-6 animate-spin text-slate-400" />
      </div>
    );
  }

  if (error || !report) {
    return (
      <div className="min-h-screen bg-white flex flex-col items-center justify-center gap-4">
        <p className="text-sm text-slate-500">
          {error ?? "レポートが見つかりません。"}
        </p>
        <Button
          type="button"
          variant="outline"
          size="sm"
          onClick={() => window.history.back()}
        >
          <ArrowLeft className="h-4 w-4" />
          戻る
        </Button>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-white text-slate-900 print:bg-white">
      <div className="mx-auto max-w-4xl px-6 py-8 space-y-8 print:max-w-none print:px-0 print:py-0 print:space-y-6">
        <div className="flex items-center justify-between gap-4 print:hidden">
          <Button
            type="button"
            variant="ghost"
            size="sm"
            onClick={() => window.history.back()}
            className="gap-1.5 text-xs"
          >
            <ArrowLeft className="h-4 w-4" />
            管理画面へ戻る
          </Button>
          <Button
            type="button"
            variant="outline"
            size="sm"
            onClick={handlePrint}
            className="gap-1.5 text-xs"
          >
            <Printer className="h-4 w-4" />
            印刷する
          </Button>
        </div>

        <header className="space-y-2 text-center print:hidden">
          <p className="text-[11px] uppercase tracking-[0.3em] text-slate-400">
            Session Report
          </p>
          <h1 className="text-3xl font-semibold">
            セッションレポート v{String(report.version).padStart(2, "0")}
          </h1>
          <div className="flex justify-center gap-4 text-xs text-slate-500">
            <span>Session ID: {sessionId}</span>
            <span>Report ID: {report.id}</span>
          </div>
        </header>

        {report.requestMarkdown ? (
          <section className="rounded-3xl border border-indigo-100 bg-indigo-50/70 p-6 text-sm text-indigo-900 print:hidden">
            <p className="text-[11px] font-semibold uppercase tracking-[0.18em] text-indigo-400">
              Admin Request
            </p>
            <p className="mt-2 whitespace-pre-wrap leading-relaxed">
              {report.requestMarkdown}
            </p>
          </section>
        ) : null}

        <section className="rounded-3xl border border-slate-200 bg-white p-8 shadow-sm print:border-0 print:bg-transparent print:p-0 print:shadow-none">
          <div className="flex items-center justify-between gap-3 mb-4">
            <div>
              <p className="text-[11px] font-semibold uppercase tracking-[0.18em] text-slate-400">
                PCA Analysis
              </p>
              <h2 className="text-lg font-semibold text-slate-900">
                議論の可視化ダッシュボード
              </h2>
            </div>
            <span className="rounded-full bg-slate-100 px-3 py-1 text-[11px] font-medium text-slate-600">
              Beta
            </span>
          </div>
          {userId ? (
            <VisualDeliberationSection
              sessionId={sessionId}
              accessToken={accessToken}
              userId={userId}
            />
          ) : (
            <div className="flex items-center justify-center py-8 text-sm text-slate-500">
              <Loader2 className="mr-2 h-4 w-4 animate-spin text-slate-400" />
              ユーザー情報を読み込み中です…
            </div>
          )}
        </section>

        <section className="rounded-3xl border border-slate-200 bg-white p-8 shadow-sm print:border-0 print:bg-transparent print:p-0 print:shadow-none">
          {report.status === "completed" && report.contentMarkdown ? (
            <div className="markdown-body prose prose-slate max-w-none text-base leading-relaxed print:text-[12pt]">
              <ReactMarkdown remarkPlugins={[remarkGfm]}>
                {report.contentMarkdown}
              </ReactMarkdown>
            </div>
          ) : report.status === "failed" ? (
            <p className="text-sm text-rose-600">
              レポート生成に失敗しました:{" "}
              {report.errorMessage ?? "詳細は管理画面を確認してください。"}
            </p>
          ) : (
            <p className="text-sm text-slate-500">
              レポートはまだ完成していません。管理画面から進行状況を確認してください。
            </p>
          )}
        </section>

        <footer className="text-center text-[11px] uppercase tracking-[0.2em] text-slate-400 print:hidden">
          <p>
            Created:{" "}
            {new Date(report.createdAt).toLocaleString("ja-JP", {
              hour12: false,
            })}
            {report.completedAt
              ? ` / Updated: ${new Date(report.completedAt).toLocaleString("ja-JP", { hour12: false })}`
              : ""}
          </p>
        </footer>
      </div>
    </div>
  );
}
