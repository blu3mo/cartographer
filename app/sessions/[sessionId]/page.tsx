'use client';

import { use, useEffect, useState } from 'react';
import { useUserId } from '@/lib/useUserId';
import { createAuthorizationHeader } from '@/lib/auth';
import axios from 'axios';

type Statement = {
  id: string;
  text: string;
  sessionId: string;
  orderIndex: number;
};

type SessionState = 'NEEDS_NAME' | 'ANSWERING' | 'COMPLETED';

export default function SessionPage({
  params,
}: {
  params: Promise<{ sessionId: string }>;
}) {
  const { sessionId } = use(params);
  const { userId, isLoading: userLoading } = useUserId();
  const [state, setState] = useState<SessionState>('NEEDS_NAME');
  const [name, setName] = useState('');
  const [currentStatement, setCurrentStatement] = useState<Statement | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (!userId || userLoading) return;

    // Check if already participating
    const checkParticipation = async () => {
      try {
        const response = await axios.get(
          `/api/sessions/${sessionId}/statements/next`,
          { headers: createAuthorizationHeader(userId) }
        );

        // If we got a statement, user is already participating
        if (response.data.statement) {
          setCurrentStatement(response.data.statement);
          setState('ANSWERING');
        } else {
          setState('COMPLETED');
        }
      } catch (err: unknown) {
        // If error is 401, user hasn't joined yet
        if (axios.isAxiosError(err) && err.response?.status === 401) {
          setState('NEEDS_NAME');
        } else {
          setState('NEEDS_NAME');
        }
      }
    };

    checkParticipation();
  }, [userId, userLoading, sessionId]);

  const handleJoinSession = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!userId) return;

    setIsLoading(true);
    setError(null);

    try {
      await axios.post(
        `/api/sessions/${sessionId}/participants`,
        { name },
        { headers: createAuthorizationHeader(userId) }
      );

      // Fetch first statement
      const response = await axios.get(
        `/api/sessions/${sessionId}/statements/next`,
        { headers: createAuthorizationHeader(userId) }
      );

      if (response.data.statement) {
        setCurrentStatement(response.data.statement);
        setState('ANSWERING');
      } else {
        setState('COMPLETED');
      }
    } catch (err) {
      console.error('Failed to join session:', err);
      setError('セッションへの参加に失敗しました。');
    } finally {
      setIsLoading(false);
    }
  };

  const handleAnswer = async (value: number) => {
    if (!userId || !currentStatement) return;

    setIsLoading(true);
    setError(null);

    try {
      // Submit answer
      await axios.post(
        `/api/sessions/${sessionId}/responses`,
        { statementId: currentStatement.id, value },
        { headers: createAuthorizationHeader(userId) }
      );

      // Fetch next statement
      const response = await axios.get(
        `/api/sessions/${sessionId}/statements/next`,
        { headers: createAuthorizationHeader(userId) }
      );

      if (response.data.statement) {
        setCurrentStatement(response.data.statement);
      } else {
        setState('COMPLETED');
        setCurrentStatement(null);
      }
    } catch (err) {
      console.error('Failed to submit answer:', err);
      setError('回答の送信に失敗しました。');
    } finally {
      setIsLoading(false);
    }
  };

  if (userLoading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <p className="text-gray-600">読み込み中...</p>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50 py-12 px-4">
      <div className="max-w-3xl mx-auto">
        <h1 className="text-3xl font-bold text-gray-900 mb-8">
          セッション参加
        </h1>

        {state === 'NEEDS_NAME' && (
          <div className="bg-white rounded-lg shadow-sm p-6">
            <p className="text-gray-700 mb-6">
              参加するには、まず名前を入力してください
            </p>
            <form onSubmit={handleJoinSession} className="space-y-4">
              <input
                type="text"
                value={name}
                onChange={(e) => setName(e.target.value)}
                required
                placeholder="あなたの名前"
                className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent"
              />
              {error && (
                <p className="text-red-600 text-sm">{error}</p>
              )}
              <button
                type="submit"
                disabled={isLoading}
                className="w-full px-6 py-3 bg-gray-800 text-white rounded-lg shadow-sm hover:bg-gray-700 transition-colors disabled:bg-gray-400"
              >
                {isLoading ? '参加中...' : '参加する'}
              </button>
            </form>
          </div>
        )}

        {state === 'ANSWERING' && currentStatement && (
          <div className={`bg-white rounded-lg shadow-sm p-6 ${isLoading ? 'opacity-50' : ''}`}>
            <div className="mb-6">
              <p className="text-xl text-gray-900">{currentStatement.text}</p>
            </div>
            <div className="grid grid-cols-5 gap-2">
              <button
                onClick={() => handleAnswer(2)}
                disabled={isLoading}
                className="px-4 py-3 bg-green-600 text-white rounded-lg hover:bg-green-700 transition-colors disabled:bg-gray-400 text-sm"
              >
                Strong Yes
              </button>
              <button
                onClick={() => handleAnswer(1)}
                disabled={isLoading}
                className="px-4 py-3 bg-green-400 text-white rounded-lg hover:bg-green-500 transition-colors disabled:bg-gray-400 text-sm"
              >
                Yes
              </button>
              <button
                onClick={() => handleAnswer(0)}
                disabled={isLoading}
                className="px-4 py-3 bg-gray-400 text-white rounded-lg hover:bg-gray-500 transition-colors disabled:bg-gray-300 text-sm"
              >
                わからない
              </button>
              <button
                onClick={() => handleAnswer(-1)}
                disabled={isLoading}
                className="px-4 py-3 bg-red-400 text-white rounded-lg hover:bg-red-500 transition-colors disabled:bg-gray-400 text-sm"
              >
                No
              </button>
              <button
                onClick={() => handleAnswer(-2)}
                disabled={isLoading}
                className="px-4 py-3 bg-red-600 text-white rounded-lg hover:bg-red-700 transition-colors disabled:bg-gray-400 text-sm"
              >
                Strong No
              </button>
            </div>
            {error && (
              <p className="text-red-600 text-sm mt-4">{error}</p>
            )}
          </div>
        )}

        {state === 'COMPLETED' && (
          <div className="bg-white rounded-lg shadow-sm p-6">
            <p className="text-xl text-gray-900 mb-4">
              全ての質問への回答が完了しました。お疲れ様でした！
            </p>
            <p className="text-gray-600">
              じぶんレポート機能はPhase 3で実装されます。
            </p>
          </div>
        )}
      </div>
    </div>
  );
}
