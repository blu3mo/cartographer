'use client';

import { use, useEffect, useState } from 'react';
import ReactMarkdown from 'react-markdown';
import remarkGfm from 'remark-gfm';
import { useUserId } from '@/lib/useUserId';
import axios from 'axios';

interface ResponseStats {
  strongYes: number;
  yes: number;
  dontKnow: number;
  no: number;
  strongNo: number;
  totalCount: number;
}

interface StatementWithStats {
  id: string;
  sessionId: string;
  text: string;
  orderIndex: number;
  responses: ResponseStats;
  agreementScore: number;
}

interface SituationAnalysisReport {
  id: string;
  sessionId: string;
  contentMarkdown: string;
  createdAt: string;
}

interface SessionAdminData {
  id: string;
  title: string;
  context: string;
  createdAt: string;
  statements: StatementWithStats[];
  latestSituationAnalysisReport?: SituationAnalysisReport;
}

type SortType = 'agreement' | 'yes' | 'dontKnow' | 'no';

export default function AdminPage({
  params,
}: {
  params: Promise<{ sessionId: string }>;
}) {
  const { sessionId } = use(params);
  const { userId, isLoading: isUserIdLoading } = useUserId();
  const [data, setData] = useState<SessionAdminData | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [generating, setGenerating] = useState(false);
  const [generatingStatements, setGeneratingStatements] = useState(false);
  const [sortType, setSortType] = useState<SortType>('agreement');

  useEffect(() => {
    if (isUserIdLoading || !userId) return;
    fetchAdminData();
  }, [userId, isUserIdLoading, sessionId]);

  const fetchAdminData = async () => {
    try {
      setLoading(true);
      const response = await axios.get(
        `/api/sessions/${sessionId}/admin`,
        {
          headers: {
            Authorization: `Bearer ${userId}`,
          },
        }
      );
      setData(response.data.data);
      setError(null);
    } catch (err: any) {
      console.error('Failed to fetch admin data:', err);
      if (err.response?.status === 403) {
        setError('このセッションの管理権限がありません。');
      } else {
        setError('データの取得に失敗しました。');
      }
    } finally {
      setLoading(false);
    }
  };

  const generateReport = async () => {
    try {
      setGenerating(true);
      const response = await axios.post(
        `/api/sessions/${sessionId}/reports/situation-analysis`,
        {},
        {
          headers: {
            Authorization: `Bearer ${userId}`,
          },
        }
      );
      // Update the data with the new report
      if (data) {
        setData({
          ...data,
          latestSituationAnalysisReport: response.data.report,
        });
      }
    } catch (err) {
      console.error('Failed to generate report:', err);
      alert('レポートの生成に失敗しました。');
    } finally {
      setGenerating(false);
    }
  };

  const generateNewStatements = async () => {
    try {
      setGeneratingStatements(true);
      await axios.post(
        `/api/sessions/${sessionId}/statements/generate`,
        {},
        {
          headers: {
            Authorization: `Bearer ${userId}`,
          },
        }
      );
      // Refresh the admin data to show new statements
      await fetchAdminData();
      alert('新しいステートメントを5つ生成しました。');
    } catch (err) {
      console.error('Failed to generate new statements:', err);
      alert('ステートメントの生成に失敗しました。');
    } finally {
      setGeneratingStatements(false);
    }
  };

  const getSortedStatements = () => {
    if (!data) return [];
    const statements = [...data.statements];

    switch (sortType) {
      case 'agreement':
        return statements.sort((a, b) => b.agreementScore - a.agreementScore);
      case 'yes':
        return statements.sort((a, b) => {
          const aYes = a.responses.strongYes + a.responses.yes;
          const bYes = b.responses.strongYes + b.responses.yes;
          return bYes - aYes;
        });
      case 'dontKnow':
        return statements.sort((a, b) => b.responses.dontKnow - a.responses.dontKnow);
      case 'no':
        return statements.sort((a, b) => {
          const aNo = a.responses.strongNo + a.responses.no;
          const bNo = b.responses.strongNo + b.responses.no;
          return bNo - aNo;
        });
      default:
        return statements;
    }
  };

  if (isUserIdLoading || loading) {
    return (
      <div className="min-h-screen bg-gray-50 py-12 px-4">
        <div className="max-w-4xl mx-auto">
          <div className="text-center">読み込み中...</div>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="min-h-screen bg-gray-50 py-12 px-4">
        <div className="max-w-4xl mx-auto">
          <div className="bg-red-50 border border-red-200 text-red-800 rounded-lg p-4">
            {error}
          </div>
        </div>
      </div>
    );
  }

  if (!data) {
    return (
      <div className="min-h-screen bg-gray-50 py-12 px-4">
        <div className="max-w-4xl mx-auto">
          <div className="text-center">セッションが見つかりません。</div>
        </div>
      </div>
    );
  }

  const sortedStatements = getSortedStatements();

  return (
    <div className="min-h-screen bg-gray-50 py-12 px-4">
      <div className="max-w-4xl mx-auto">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">
          {data.title}
        </h1>
        <p className="text-gray-600 mb-8">管理画面</p>

        {/* Control Panel */}
        <div className="bg-white rounded-lg shadow-sm p-6 mb-8">
          <h2 className="text-xl font-semibold mb-4">コントロールパネル</h2>
          <div className="flex gap-4">
            <button
              onClick={generateReport}
              disabled={generating}
              className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition-colors disabled:bg-gray-400 disabled:cursor-not-allowed"
            >
              {generating ? '生成中...' : '現状分析レポートを生成する'}
            </button>
            <button
              onClick={generateNewStatements}
              disabled={generatingStatements}
              className="px-4 py-2 bg-green-600 text-white rounded-lg hover:bg-green-700 transition-colors disabled:bg-gray-400 disabled:cursor-not-allowed"
            >
              {generatingStatements ? '生成中...' : '新しいStatementを5つ生成する'}
            </button>
          </div>
        </div>

        {/* Latest Report */}
        {data.latestSituationAnalysisReport && (
          <div className="bg-white rounded-lg shadow-sm p-6 mb-8">
            <h2 className="text-xl font-semibold mb-4">最新の現状分析レポート</h2>
            <div className="markdown-body">
              <ReactMarkdown remarkPlugins={[remarkGfm]}>
                {data.latestSituationAnalysisReport.contentMarkdown}
              </ReactMarkdown>
            </div>
            <p className="text-sm text-gray-500 mt-4">
              生成日時: {new Date(data.latestSituationAnalysisReport.createdAt).toLocaleString('ja-JP')}
            </p>
          </div>
        )}

        {/* Statements List */}
        <div className="bg-white rounded-lg shadow-sm p-6">
          <div className="flex items-center justify-between mb-6">
            <h2 className="text-xl font-semibold">ステートメント一覧</h2>
            <div className="flex items-center gap-2">
              <span className="text-sm text-gray-600">並び替え:</span>
              <select
                value={sortType}
                onChange={(e) => setSortType(e.target.value as SortType)}
                className="px-3 py-1 border border-gray-300 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
              >
                <option value="agreement">合意度順</option>
                <option value="yes">Yesの割合順</option>
                <option value="dontKnow">わからないの割合順</option>
                <option value="no">Noの割合順</option>
              </select>
            </div>
          </div>

          <div className="space-y-6">
            {sortedStatements.map((statement) => (
              <StatementCard key={statement.id} statement={statement} />
            ))}
          </div>
        </div>
      </div>
    </div>
  );
}

function StatementCard({ statement }: { statement: StatementWithStats }) {
  const { responses } = statement;
  const hasResponses = responses.totalCount > 0;

  return (
    <div className="border border-gray-200 rounded-lg p-4">
      <p className="text-gray-900 mb-3">{statement.text}</p>

      {hasResponses ? (
        <>
          {/* Horizontal Bar Chart */}
          <div className="flex w-full h-6 rounded-full overflow-hidden mb-2">
            {responses.strongYes > 0 && (
              <div
                className="bg-green-600"
                style={{ width: `${responses.strongYes}%` }}
                title={`Strong Yes: ${responses.strongYes.toFixed(1)}%`}
              />
            )}
            {responses.yes > 0 && (
              <div
                className="bg-green-400"
                style={{ width: `${responses.yes}%` }}
                title={`Yes: ${responses.yes.toFixed(1)}%`}
              />
            )}
            {responses.dontKnow > 0 && (
              <div
                className="bg-yellow-400"
                style={{ width: `${responses.dontKnow}%` }}
                title={`わからない: ${responses.dontKnow.toFixed(1)}%`}
              />
            )}
            {responses.no > 0 && (
              <div
                className="bg-red-400"
                style={{ width: `${responses.no}%` }}
                title={`No: ${responses.no.toFixed(1)}%`}
              />
            )}
            {responses.strongNo > 0 && (
              <div
                className="bg-red-600"
                style={{ width: `${responses.strongNo}%` }}
                title={`Strong No: ${responses.strongNo.toFixed(1)}%`}
              />
            )}
          </div>

          {/* Statistics Text */}
          <div className="grid grid-cols-5 gap-2 text-xs text-gray-600">
            <div>
              <span className="font-medium text-green-600">Strong Yes:</span>{' '}
              {responses.strongYes.toFixed(1)}%
            </div>
            <div>
              <span className="font-medium text-green-500">Yes:</span>{' '}
              {responses.yes.toFixed(1)}%
            </div>
            <div>
              <span className="font-medium text-yellow-600">わからない:</span>{' '}
              {responses.dontKnow.toFixed(1)}%
            </div>
            <div>
              <span className="font-medium text-red-500">No:</span>{' '}
              {responses.no.toFixed(1)}%
            </div>
            <div>
              <span className="font-medium text-red-600">Strong No:</span>{' '}
              {responses.strongNo.toFixed(1)}%
            </div>
          </div>

          <p className="text-xs text-gray-500 mt-2">
            回答者数: {responses.totalCount}人 | 合意度スコア: {statement.agreementScore}
          </p>
        </>
      ) : (
        <p className="text-sm text-gray-500">まだ回答がありません。</p>
      )}
    </div>
  );
}
