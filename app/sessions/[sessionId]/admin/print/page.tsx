"use client";

import axios from "axios";
import { Loader2 } from "lucide-react";
import { use, useEffect, useState } from "react";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";
import { useUserId } from "@/lib/useUserId";

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
  isPublic: boolean;
  createdAt: string;
  latestSituationAnalysisReport?: SituationAnalysisReport;
}

export default function PrintReportPage({
  params,
}: {
  params: Promise<{ sessionId: string }>;
}) {
  const { sessionId } = use(params);
  const { userId, isLoading: isUserIdLoading } = useUserId();
  const [data, setData] = useState<SessionAdminData | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (isUserIdLoading || !userId) return;

    const fetchAdminData = async () => {
      try {
        setLoading(true);
        const response = await axios.get(`/api/sessions/${sessionId}/admin`, {
          headers: {
            Authorization: `Bearer ${userId}`,
          },
        });
        setData(response.data.data);
        setError(null);
      } catch (err: unknown) {
        console.error("Failed to fetch admin data:", err);
        if (axios.isAxiosError(err) && err.response?.status === 403) {
          setError("このセッションの管理権限がありません。");
        } else {
          setError("データの取得に失敗しました。");
        }
      } finally {
        setLoading(false);
      }
    };

    fetchAdminData();
  }, [userId, isUserIdLoading, sessionId]);

  if (isUserIdLoading || loading) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-white">
        <Loader2 className="h-8 w-8 animate-spin text-gray-400" />
      </div>
    );
  }

  if (error) {
    return (
      <div className="min-h-screen bg-white">
        <div className="max-w-4xl mx-auto px-8 py-12">
          <div className="border border-red-300 rounded-lg p-6 bg-red-50">
            <p className="text-red-700">{error}</p>
          </div>
        </div>
      </div>
    );
  }

  if (!data || !data.latestSituationAnalysisReport) {
    return (
      <div className="min-h-screen bg-white">
        <div className="max-w-4xl mx-auto px-8 py-12">
          <div className="border border-amber-300 rounded-lg p-6 bg-amber-50">
            <p className="text-amber-700">
              現状分析レポートがまだ生成されていません。
            </p>
          </div>
        </div>
      </div>
    );
  }

  const report = data.latestSituationAnalysisReport;
  const reportDate = new Date(report.createdAt).toLocaleString("ja-JP", {
    year: "numeric",
    month: "long",
    day: "numeric",
    hour: "2-digit",
    minute: "2-digit",
  });

  return (
    <>
      <style jsx global>{`
        @media print {
          @page {
            margin: 1.5cm;
          }
          
          body {
            print-color-adjust: exact;
            -webkit-print-color-adjust: exact;
            background: white !important;
            margin: 0 !important;
            padding: 0 !important;
          }
          
          .no-print {
            display: none !important;
          }
          
          .print-page-wrapper {
            background: white !important;
            padding: 0 !important;
            margin: 0 !important;
          }
          
          .print-container {
            box-shadow: none !important;
            border-radius: 0 !important;
            padding: 0 !important;
            margin: 0 !important;
            max-width: none !important;
          }
          
          .print-header {
            page-break-after: avoid;
            border: none !important;
            padding-bottom: 1.5rem !important;
            margin-bottom: 1.5rem !important;
          }
          
          .markdown-body {
            page-break-inside: avoid;
          }
          
          .markdown-body h1,
          .markdown-body h2,
          .markdown-body h3 {
            page-break-after: avoid;
          }
          
          .markdown-body p,
          .markdown-body ul,
          .markdown-body ol {
            page-break-inside: avoid;
          }
        }
        
        @media screen {
          .print-container {
            background: white;
            box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
          }
        }
      `}</style>

      <div className="print-page-wrapper min-h-screen bg-gray-50">
        <div className="max-w-4xl mx-auto px-8 py-12">
          {/* Print button - hidden when printing */}
          <div className="no-print mb-8 flex justify-end">
            <button
              type="button"
              onClick={() => window.print()}
              className="px-6 py-2.5 bg-slate-800 text-white rounded-lg hover:bg-slate-700 transition-colors font-medium text-sm shadow-sm"
            >
              印刷 / PDF保存
            </button>
          </div>

          {/* Print content */}
          <div className="print-container bg-white rounded-lg px-12 py-10">
            {/* Header */}
            <div className="print-header mb-8 pb-6 border-b border-gray-200">
              <h1 className="text-3xl font-bold text-gray-900 mb-3">
                現状分析レポート
              </h1>
              <div className="space-y-1.5 text-sm text-gray-600">
                <p className="font-medium text-base text-gray-800">
                  {data.title}
                </p>
                <p>生成日時: {reportDate}</p>
              </div>
            </div>

            {/* Report content */}
            <div className="markdown-body prose prose-slate max-w-none">
              <ReactMarkdown remarkPlugins={[remarkGfm]}>
                {report.contentMarkdown}
              </ReactMarkdown>
            </div>
          </div>
        </div>
      </div>
    </>
  );
}
