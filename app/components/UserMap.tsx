"use client";

import axios from "axios";
import { Activity, Info, Loader2, Map } from "lucide-react";
import { useCallback, useEffect, useMemo, useState } from "react";
import {
  CartesianGrid,
  Cell,
  ResponsiveContainer,
  Scatter,
  ScatterChart,
  ReferenceLine,
  Tooltip,
  XAxis,
  YAxis,
} from "recharts";

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

interface TooltipPayload {
  payload: ParticipantPoint;
}

interface UserMapProps {
  sessionId: string;
  userId: string;
}

export default function UserMap({ sessionId, userId }: UserMapProps) {
  const [data, setData] = useState<UserMapData | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [selectedParticipant, setSelectedParticipant] =
    useState<ParticipantPoint | null>(null);
  const [selectedStatement, setSelectedStatement] = useState<{
    component: "pc1" | "pc2";
    statement: TopStatement;
  } | null>(null);

  const fetchUserMapData = useCallback(async () => {
    try {
      setLoading(true);
      setError(null);
      const response = await axios.get(`/api/sessions/${sessionId}/user-map`, {
        headers: {
          Authorization: `Bearer ${userId}`,
        },
      });

      const payload = response.data;

      if (payload?.status === "insufficient-data") {
        setData(null);
        setError(
          payload.reason ||
            "PCA分析を実行できません。十分な参加者または回答が必要です。",
        );
        return;
      }

      if (payload?.status === "ok" && payload.data) {
        setData(payload.data);
        return;
      }

      if (payload?.data) {
        setData(payload.data);
        return;
      }

      setData(null);
      setError("ユーザーマップのデータが取得できませんでした。");
    } catch (err: unknown) {
      console.error("Failed to fetch user map data:", err);
      setError("ユーザーマップの取得に失敗しました。");
    } finally {
      setLoading(false);
    }
  }, [sessionId, userId]);

  useEffect(() => {
    void fetchUserMapData();
  }, [fetchUserMapData]);

  const variance1 = useMemo(() => {
    if (!data) return "0.0";
    return (data.pc1.explainedVariance * 100).toFixed(1);
  }, [data]);

  const variance2 = useMemo(() => {
    if (!data) return "0.0";
    return (data.pc2.explainedVariance * 100).toFixed(1);
  }, [data]);

  const getParticipantColor = (index: number) => {
    const colors = [
      "#10b981",
      "#3b82f6",
      "#f59e0b",
      "#ef4444",
      "#8b5cf6",
      "#ec4899",
      "#14b8a6",
      "#f97316",
    ];
    return colors[index % colors.length];
  };

  const CustomTooltip = ({
    active,
    payload,
  }: {
    active?: boolean;
    payload?: TooltipPayload[];
  }) => {
    if (active && payload && payload.length) {
      const point = payload[0].payload;
      return (
        <div className="bg-gray-900 text-white px-4 py-3 rounded-lg shadow-lg text-sm">
          <p className="font-semibold mb-1">{point.name}</p>
          <p className="text-xs opacity-90">
            回答数: {point.responseCount} / {data.totalStatements}
          </p>
          <p className="text-xs opacity-75 mt-1">
            PC1: {point.x.toFixed(2)} | PC2: {point.y.toFixed(2)}
          </p>
        </div>
      );
    }
    return null;
  };

  const maxLoading = useMemo(() => {
    if (!data) return 1;
    const all = [...data.pc1.topStatements, ...data.pc2.topStatements];
    const max = Math.max(...all.map((s) => Math.abs(s.loading)));
    return Number.isFinite(max) ? max : 1;
  }, [data]);

  const computeAnchors = useCallback((items: TopStatement[]) => {
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
  }, []);

  const pc1Anchors = useMemo(() => {
    if (!data) return { positive: "", negative: "" };
    return computeAnchors(data.pc1.topStatements);
  }, [computeAnchors, data]);
  const pc2Anchors = useMemo(() => {
    if (!data) return { positive: "", negative: "" };
    return computeAnchors(data.pc2.topStatements);
  }, [computeAnchors, data]);

  const handleSelectStatement = (component: "pc1" | "pc2", stmt: TopStatement) => {
    setSelectedStatement({ component, statement: stmt });
    setSelectedParticipant(null);
  };

  const statementLabel = (stmt: TopStatement) =>
    `Statement #${stmt.orderIndex + 1}`;

  const influenceWidth = (stmt: TopStatement) =>
    `${Math.min((Math.abs(stmt.loading) / maxLoading) * 100, 100)}%`;

  if (loading) {
    return (
      <div className="flex items-center justify-center py-12">
        <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
      </div>
    );
  }

  if (error) {
    return (
      <div className="rounded-lg border border-muted bg-muted/50 p-8 text-center">
        <p className="text-sm text-muted-foreground">{error}</p>
        <p className="text-xs text-muted-foreground mt-2">
          ※ PCA分析には最低3人の参加者とそれぞれの回答が必要です
        </p>
      </div>
    );
  }

  if (!data || data.participants.length === 0) {
    return (
      <div className="rounded-lg border border-muted bg-muted/50 p-8 text-center">
        <p className="text-sm text-muted-foreground">
          表示するデータがありません
        </p>
      </div>
    );
  }

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <div className="flex items-center gap-2 text-sm text-muted-foreground">
          <Map className="h-4 w-4 text-indigo-500" />
          <span>主成分分析に基づくユーザーマップ</span>
        </div>
        <div className="flex items-center gap-2 text-xs text-muted-foreground">
          <span className="rounded-full bg-muted px-2 py-1 font-medium text-foreground">
            Beta
          </span>
          <span>
            PC1 寄与率 <span className="font-semibold">{variance1}%</span> /
            PC2 <span className="font-semibold">{variance2}%</span>
          </span>
        </div>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-12 gap-4">
        <div className="lg:col-span-4 flex flex-col gap-3">
          <div className="rounded-xl border border-muted bg-card p-4 shadow-sm min-h-[280px]">
            <div className="flex items-center justify-between pb-3 border-b border-muted/40">
              <div className="flex items-center gap-2 text-xs font-semibold uppercase tracking-wide text-muted-foreground">
                <Info className="h-4 w-4" />
                詳細情報
              </div>
              <span className="text-[11px] text-muted-foreground">
                {data.participants.length}人 / {data.totalStatements}問
              </span>
            </div>

            {selectedStatement ? (
              <div className="pt-3 space-y-3">
                <div className="flex items-center justify-between">
                  <span className="text-xs font-semibold text-indigo-600 bg-indigo-50 border border-indigo-100 rounded-full px-2 py-1">
                    {selectedStatement.component.toUpperCase()}
                  </span>
                  <span className="text-xs text-muted-foreground">
                    {statementLabel(selectedStatement.statement)}
                  </span>
                </div>
                <p className="text-sm font-semibold text-foreground leading-relaxed">
                  {selectedStatement.statement.text}
                </p>
                <div className="space-y-2 bg-muted/40 border border-muted rounded-lg p-3">
                  <div className="flex items-center justify-between text-xs text-muted-foreground">
                    <span>軸への寄与度</span>
                    <span className="font-semibold text-foreground">
                      {Math.abs(selectedStatement.statement.loading).toFixed(3)}
                    </span>
                  </div>
                  <div className="h-2.5 w-full bg-muted rounded-full overflow-hidden">
                    <div
                      className={`h-full rounded-full ${
                        selectedStatement.statement.loading >= 0
                          ? "bg-indigo-500"
                          : "bg-amber-500"
                      }`}
                      style={{ width: influenceWidth(selectedStatement.statement) }}
                    />
                  </div>
                  <p className="text-xs text-muted-foreground leading-relaxed">
                    {selectedStatement.statement.loading >= 0
                      ? "この設問は軸のプラス側に強く寄与しています"
                      : "この設問は軸のマイナス側に強く寄与しています"}
                  </p>
                </div>
              </div>
            ) : selectedParticipant ? (
              <div className="pt-3 space-y-3">
                <div className="flex items-center gap-3">
                  <div
                    className="w-12 h-12 rounded-full flex items-center justify-center text-white text-lg font-bold shadow"
                    style={{
                      backgroundColor: getParticipantColor(
                        data.participants.findIndex(
                          (p) => p.id === selectedParticipant.id,
                        ),
                      ),
                    }}
                  >
                    {selectedParticipant.name.charAt(0)}
                  </div>
                  <div>
                    <div className="text-base font-semibold text-foreground">
                      {selectedParticipant.name}
                    </div>
                    <div className="text-xs text-muted-foreground">
                      回答数 {selectedParticipant.responseCount} /{" "}
                      {data.totalStatements}
                    </div>
                  </div>
                </div>
                <div className="grid grid-cols-2 gap-3">
                  <div className="rounded-lg border border-muted/50 bg-muted/20 p-3">
                    <div className="text-[11px] uppercase text-muted-foreground">
                      PC1
                    </div>
                    <div className="text-sm font-semibold text-foreground">
                      {selectedParticipant.x.toFixed(2)}
                    </div>
                    <p className="text-[11px] text-muted-foreground mt-1">
                      第1主成分の位置
                    </p>
                  </div>
                  <div className="rounded-lg border border-muted/50 bg-muted/20 p-3">
                    <div className="text-[11px] uppercase text-muted-foreground">
                      PC2
                    </div>
                    <div className="text-sm font-semibold text-foreground">
                      {selectedParticipant.y.toFixed(2)}
                    </div>
                    <p className="text-[11px] text-muted-foreground mt-1">
                      第2主成分の位置
                    </p>
                  </div>
                </div>
                <p className="text-xs text-muted-foreground leading-relaxed">
                  近い位置にいる参加者ほど回答パターンが似ています。座標はPCAで2次元に圧縮した結果です。
                </p>
              </div>
            ) : (
              <div className="flex h-full flex-col items-center justify-center text-center text-xs text-muted-foreground gap-3 pt-6">
                <div className="w-12 h-12 rounded-full bg-muted flex items-center justify-center">
                  <Activity className="h-5 w-5 opacity-60" />
                </div>
                <p>
                  グラフ上の参加者、または下の設問リストをクリックすると詳細が表示されます。
                </p>
              </div>
            )}
          </div>
        </div>

        <div className="lg:col-span-8 h-[420px] rounded-xl border border-muted bg-card shadow-sm relative overflow-hidden">
          <div className="absolute inset-0 pointer-events-none">
            <div className="absolute inset-0 flex flex-col justify-between p-6 text-[11px] font-semibold text-muted-foreground">
              <div className="flex justify-center relative">
                <span className="absolute top-0 bg-indigo-50 text-indigo-700 px-3 py-1 rounded-full border border-indigo-100 shadow-sm">
                  ▲ {pc2Anchors.positive}
                </span>
              </div>
              <div className="flex justify-center relative">
                <span className="absolute bottom-0 bg-slate-50 text-slate-600 px-3 py-1 rounded-full border border-muted">
                  ▼ {pc2Anchors.negative}
                </span>
              </div>
            </div>
            <div className="absolute inset-0 flex items-center justify-between p-6 text-[11px] font-semibold text-muted-foreground">
              <div className="w-full relative">
                <span className="absolute left-0 bg-slate-50 text-slate-600 px-3 py-1 rounded-full border border-muted">
                  ◀ {pc1Anchors.negative}
                </span>
                <span className="absolute right-0 bg-indigo-50 text-indigo-700 px-3 py-1 rounded-full border border-indigo-100 shadow-sm">
                  {pc1Anchors.positive} ▶
                </span>
              </div>
            </div>
          </div>

          <ResponsiveContainer width="100%" height="100%">
            <ScatterChart margin={{ top: 30, right: 30, bottom: 40, left: 40 }}>
              <CartesianGrid strokeDasharray="3 3" opacity={0.2} />
              <XAxis
                type="number"
                dataKey="x"
                name="PC1"
                stroke="#9ca3af"
                tick={{ fontSize: 12, fill: "#9ca3af" }}
              />
              <YAxis
                type="number"
                dataKey="y"
                name="PC2"
                stroke="#9ca3af"
                tick={{ fontSize: 12, fill: "#9ca3af" }}
              />
              <ReferenceLine y={0} stroke="#e5e7eb" strokeDasharray="3 3" />
              <ReferenceLine x={0} stroke="#e5e7eb" strokeDasharray="3 3" />
              <Tooltip
                content={<CustomTooltip />}
                cursor={{ strokeDasharray: "3 3" }}
              />
              <Scatter
                name="参加者"
                data={data.participants}
                onClick={(node) => {
                  setSelectedParticipant(node.payload);
                  setSelectedStatement(null);
                }}
              >
                {data.participants.map((participant, index) => (
                  <Cell
                    key={participant.id}
                    fill={getParticipantColor(index)}
                    stroke="#fff"
                    strokeWidth={2}
                    r={10}
                  />
                ))}
              </Scatter>
            </ScatterChart>
          </ResponsiveContainer>
        </div>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        <div className="rounded-xl border border-muted bg-card p-4 shadow-sm">
          <div className="flex items-center justify-between mb-3">
            <div className="flex items-center gap-2">
              <span className="text-xs font-semibold text-indigo-600 bg-indigo-50 border border-indigo-100 rounded-full px-2 py-1">
                PC1
              </span>
              <p className="text-sm font-semibold text-foreground">
                横軸に強く効いている設問
              </p>
            </div>
            <span className="text-xs text-muted-foreground">{variance1}%</span>
          </div>
          <div className="space-y-2">
            {data.pc1.topStatements.map((stmt) => (
              <button
                key={`${stmt.id}-${stmt.loading}`}
                type="button"
                onClick={() => handleSelectStatement("pc1", stmt)}
                className="w-full text-left rounded-lg border border-muted/60 bg-muted/10 px-3 py-2 hover:border-indigo-200 hover:bg-indigo-50/70 transition-colors"
              >
                <div className="flex items-center justify-between text-xs text-muted-foreground">
                  <span className="font-semibold text-foreground">
                    {statementLabel(stmt)}
                  </span>
                  <span className="rounded-full bg-white px-2 py-0.5 border border-muted text-[11px]">
                    寄与度 {Math.abs(stmt.loading).toFixed(3)}
                  </span>
                </div>
                <p className="text-sm text-foreground mt-1 leading-relaxed">
                  {stmt.text}
                </p>
              </button>
            ))}
          </div>
        </div>
        <div className="rounded-xl border border-muted bg-card p-4 shadow-sm">
          <div className="flex items-center justify-between mb-3">
            <div className="flex items-center gap-2">
              <span className="text-xs font-semibold text-indigo-600 bg-indigo-50 border border-indigo-100 rounded-full px-2 py-1">
                PC2
              </span>
              <p className="text-sm font-semibold text-foreground">
                縦軸に強く効いている設問
              </p>
            </div>
            <span className="text-xs text-muted-foreground">{variance2}%</span>
          </div>
          <div className="space-y-2">
            {data.pc2.topStatements.map((stmt) => (
              <button
                key={`${stmt.id}-${stmt.loading}`}
                type="button"
                onClick={() => handleSelectStatement("pc2", stmt)}
                className="w-full text-left rounded-lg border border-muted/60 bg-muted/10 px-3 py-2 hover:border-indigo-200 hover:bg-indigo-50/70 transition-colors"
              >
                <div className="flex items-center justify-between text-xs text-muted-foreground">
                  <span className="font-semibold text-foreground">
                    {statementLabel(stmt)}
                  </span>
                  <span className="rounded-full bg-white px-2 py-0.5 border border-muted text-[11px]">
                    寄与度 {Math.abs(stmt.loading).toFixed(3)}
                  </span>
                </div>
                <p className="text-sm text-foreground mt-1 leading-relaxed">
                  {stmt.text}
                </p>
              </button>
            ))}
          </div>
        </div>
      </div>
    </div>
  );
}
