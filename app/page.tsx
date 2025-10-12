'use client';

import Link from "next/link";
import { useEffect, useState } from "react";
import { useUserId } from "@/lib/useUserId";
import { createAuthorizationHeader } from "@/lib/auth";
import axios from "axios";

type Session = {
  id: string;
  title: string;
  context: string;
  hostUserId: string;
  createdAt: string;
  _count: {
    participants: number;
    statements: number;
  };
};

export default function Home() {
  const { userId, isLoading: userLoading } = useUserId();
  const [sessions, setSessions] = useState<Session[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (!userId || userLoading) return;

    const fetchSessions = async () => {
      try {
        setLoading(true);
        const response = await axios.get('/api/sessions', {
          headers: createAuthorizationHeader(userId),
        });
        setSessions(response.data.sessions);
      } catch (err) {
        console.error('Failed to fetch sessions:', err);
        setError('セッションの取得に失敗しました。');
      } finally {
        setLoading(false);
      }
    };

    fetchSessions();
  }, [userId, userLoading]);

  if (userLoading || loading) {
    return (
      <div className="min-h-screen bg-gray-50 py-12 px-4">
        <div className="max-w-3xl mx-auto">
          <div className="text-center">読み込み中...</div>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50 py-12 px-4">
      <div className="max-w-3xl mx-auto">
        <h1 className="text-4xl font-bold text-gray-900 mb-4">
          Cartographer
        </h1>
        <p className="text-lg text-gray-700 mb-8">
          認識を可視化し、合意形成を促進するウェブサービス
        </p>

        <div className="bg-white rounded-lg shadow-sm p-6 mb-6">
          <div className="flex items-center justify-between mb-6">
            <h2 className="text-2xl font-semibold text-gray-900">
              セッション一覧
            </h2>
            <Link
              href="/sessions/new"
              className="px-4 py-2 bg-gray-800 text-white rounded-lg shadow-sm hover:bg-gray-700 transition-colors"
            >
              新規作成
            </Link>
          </div>

          {error && (
            <p className="text-red-600 mb-4">{error}</p>
          )}

          {sessions.length === 0 ? (
            <p className="text-gray-600">
              まだセッションがありません。新しいセッションを作成してください。
            </p>
          ) : (
            <div className="space-y-4">
              {sessions.map((session) => (
                <div
                  key={session.id}
                  className="border border-gray-200 rounded-lg p-4 hover:border-gray-300 transition-colors"
                >
                  <h3 className="text-lg font-semibold text-gray-900 mb-2">
                    {session.title}
                  </h3>
                  <p className="text-sm text-gray-600 mb-3 line-clamp-2">
                    {session.context}
                  </p>
                  <div className="flex items-center justify-between">
                    <div className="flex items-center gap-4 text-xs text-gray-500">
                      <span>{session._count.participants}人参加</span>
                      <span>{session._count.statements}問</span>
                      <span>{new Date(session.createdAt).toLocaleDateString('ja-JP')}</span>
                    </div>
                    <div className="flex gap-2">
                      {session.hostUserId === userId && (
                        <Link
                          href={`/sessions/${session.id}/admin`}
                          className="px-3 py-1 text-sm bg-blue-600 text-white rounded hover:bg-blue-700 transition-colors"
                        >
                          管理
                        </Link>
                      )}
                      <Link
                        href={`/sessions/${session.id}`}
                        className="px-3 py-1 text-sm bg-gray-600 text-white rounded hover:bg-gray-700 transition-colors"
                      >
                        参加
                      </Link>
                    </div>
                  </div>
                </div>
              ))}
            </div>
          )}
        </div>
      </div>
    </div>
  );
}
