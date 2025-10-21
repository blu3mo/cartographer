'use client';

import { useEffect, useState } from 'react';
import axios from 'axios';
import {
  ScatterChart,
  Scatter,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  ResponsiveContainer,
  Cell,
} from 'recharts';
import { Loader2 } from 'lucide-react';

interface ParticipantPoint {
  id: string;
  name: string;
  x: number;
  y: number;
  responseCount: number;
}

interface UserMapData {
  participants: ParticipantPoint[];
  explainedVariance: number[];
  totalStatements: number;
}

interface UserMapProps {
  sessionId: string;
  userId: string;
}

export default function UserMap({ sessionId, userId }: UserMapProps) {
  const [data, setData] = useState<UserMapData | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    fetchUserMapData();
  }, [sessionId, userId]);

  const fetchUserMapData = async () => {
    try {
      setLoading(true);
      setError(null);
      const response = await axios.get(
        `/api/sessions/${sessionId}/user-map`,
        {
          headers: {
            Authorization: `Bearer ${userId}`,
          },
        }
      );
      setData(response.data.data);
    } catch (err: any) {
      console.error('Failed to fetch user map data:', err);
      if (err.response?.status === 400) {
        setError(
          err.response.data.error ||
            'PCA分析を実行できません。十分な参加者または回答が必要です。'
        );
      } else {
        setError('ユーザーマップの取得に失敗しました。');
      }
    } finally {
      setLoading(false);
    }
  };

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

  const variance1 = data.explainedVariance[0]
    ? (data.explainedVariance[0] * 100).toFixed(1)
    : '0.0';
  const variance2 = data.explainedVariance[1]
    ? (data.explainedVariance[1] * 100).toFixed(1)
    : '0.0';

  // Color scheme for participants
  const getParticipantColor = (index: number) => {
    const colors = [
      '#10b981', // emerald-500
      '#3b82f6', // blue-500
      '#f59e0b', // amber-500
      '#ef4444', // red-500
      '#8b5cf6', // violet-500
      '#ec4899', // pink-500
      '#14b8a6', // teal-500
      '#f97316', // orange-500
    ];
    return colors[index % colors.length];
  };

  // Custom tooltip component
  const CustomTooltip = ({ active, payload }: any) => {
    if (active && payload && payload.length) {
      const point = payload[0].payload as ParticipantPoint;
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

  return (
    <div className="space-y-4">
      {/* Explained Variance */}
      <div className="flex items-center justify-center gap-6 text-sm text-muted-foreground">
        <span>
          PC1 寄与率: <span className="font-semibold text-foreground">{variance1}%</span>
        </span>
        <span className="h-1 w-1 rounded-full bg-muted-foreground" />
        <span>
          PC2 寄与率: <span className="font-semibold text-foreground">{variance2}%</span>
        </span>
      </div>

      {/* Scatter Chart */}
      <div className="w-full" style={{ height: '500px' }}>
        <ResponsiveContainer width="100%" height="100%">
          <ScatterChart
            margin={{ top: 20, right: 30, bottom: 60, left: 60 }}
          >
            <CartesianGrid strokeDasharray="3 3" opacity={0.3} />
            <XAxis
              type="number"
              dataKey="x"
              name="PC1"
              label={{
                value: `第1主成分 (PC1) - ${variance1}%`,
                position: 'bottom',
                offset: 40,
                style: { fontSize: '12px', fill: '#6b7280' },
              }}
              stroke="#9ca3af"
            />
            <YAxis
              type="number"
              dataKey="y"
              name="PC2"
              label={{
                value: `第2主成分 (PC2) - ${variance2}%`,
                angle: -90,
                position: 'left',
                offset: 40,
                style: { fontSize: '12px', fill: '#6b7280' },
              }}
              stroke="#9ca3af"
            />
            <Tooltip content={<CustomTooltip />} cursor={{ strokeDasharray: '3 3' }} />
            <Scatter
              name="参加者"
              data={data.participants}
              fill="#8884d8"
            >
              {data.participants.map((entry, index) => (
                <Cell
                  key={`cell-${index}`}
                  fill={getParticipantColor(index)}
                  stroke="#fff"
                  strokeWidth={2}
                  r={8}
                />
              ))}
            </Scatter>
          </ScatterChart>
        </ResponsiveContainer>
      </div>

      {/* Participant Count */}
      <div className="text-center">
        <p className="text-xs text-muted-foreground">
          参加者数: {data.participants.length}人 | 質問数: {data.totalStatements}個
        </p>
      </div>

      {/* Explanation */}
      <div className="rounded-lg border border-muted bg-muted/20 p-4 text-xs text-muted-foreground leading-relaxed">
        <p className="font-medium text-foreground mb-2">ユーザーマップについて</p>
        <p>
          このマップは、参加者の回答パターンをPCA（主成分分析）により2次元に投影したものです。
          近くに配置されている参加者は、似た意見傾向を持っています。
        </p>
        <ul className="mt-2 ml-4 space-y-1 list-disc">
          <li>第1主成分（横軸）と第2主成分（縦軸）は、回答の主要な変動パターンを表します</li>
          <li>寄与率は、各主成分がデータ全体の変動をどれだけ説明しているかを示します</li>
          <li>点の上にカーソルを重ねると、参加者名と詳細情報が表示されます</li>
        </ul>
      </div>
    </div>
  );
}
