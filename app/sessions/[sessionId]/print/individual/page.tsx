"use client";

import { use, useEffect, useState } from "react";
import { useUserId } from "@/lib/useUserId";
import axios from "axios";
import ReactMarkdown from "react-markdown";
import remarkGfm from "remark-gfm";
import { Loader2, Printer } from "lucide-react";
import { Button } from "@/components/ui/Button";

interface IndividualReport {
  id: string;
  participantUserId: string;
  sessionId: string;
  contentMarkdown: string;
  createdAt: string;
}

interface SessionInfo {
  id: string;
  title: string;
}

export default function PrintIndividualReportPage({
  params,
}: {
  params: Promise<{ sessionId: string }>;
}) {
  const { sessionId } = use(params);
  const { userId, isLoading: isUserIdLoading } = useUserId();
  const [report, setReport] = useState<IndividualReport | null>(null);
  const [sessionInfo, setSessionInfo] = useState<SessionInfo | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (isUserIdLoading || !userId) return;

    const fetchData = async () => {
      try {
        setLoading(true);

        // Fetch individual report
        const reportResponse = await axios.get(
          `/api/sessions/${sessionId}/individual-report`,
          {
            headers: {
              Authorization: `Bearer ${userId}`,
            },
          }
        );

        // Fetch session info
        const sessionResponse = await axios.get(`/api/sessions/${sessionId}`, {
          headers: {
            Authorization: `Bearer ${userId}`,
          },
        });

        setReport(reportResponse.data.report);
        setSessionInfo(sessionResponse.data.session);
        setError(null);
      } catch (err: any) {
        console.error("Failed to fetch data:", err);
        setError("データの取得に失敗しました。");
      } finally {
        setLoading(false);
      }
    };

    fetchData();
  }, [userId, isUserIdLoading, sessionId]);

  const handlePrint = () => {
    window.print();
  };

  if (loading || isUserIdLoading) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-background">
        <Loader2 className="w-8 h-8 animate-spin text-muted-foreground" />
      </div>
    );
  }

  if (error) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-background p-4">
        <div className="text-center">
          <p className="text-destructive">{error}</p>
        </div>
      </div>
    );
  }

  if (!report) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-background p-4">
        <div className="text-center">
          <p className="text-muted-foreground">レポートがまだ生成されていません。</p>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-white">
      {/* Print Button - Hidden when printing */}
      <div className="print:hidden fixed top-4 right-4 z-50">
        <Button onClick={handlePrint} size="lg" className="shadow-lg">
          <Printer className="w-4 h-4 mr-2" />
          印刷
        </Button>
      </div>

      {/* Content */}
      <div className="max-w-4xl mx-auto px-8 py-12">
        {/* Header */}
        <div className="mb-8 pb-6 border-b border-gray-200">
          <h1 className="text-3xl font-bold text-gray-900 mb-2">
            じぶんレポート
          </h1>
          {sessionInfo && (
            <p className="text-lg text-gray-600">{sessionInfo.title}</p>
          )}
          <p className="text-sm text-gray-500 mt-2">
            生成日時: {new Date(report.createdAt).toLocaleString("ja-JP")}
          </p>
        </div>

        {/* Report Content */}
        <div className="markdown-body prose prose-lg max-w-none">
          <ReactMarkdown remarkPlugins={[remarkGfm]}>
            {report.contentMarkdown}
          </ReactMarkdown>
        </div>
      </div>
    </div>
  );
}
