'use client';

import { useState } from 'react';
import { useRouter } from 'next/navigation';
import { useUserId } from '@/lib/useUserId';
import { createAuthorizationHeader } from '@/lib/auth';
import axios from 'axios';

export default function NewSessionPage() {
  const router = useRouter();
  const { userId, isLoading: userLoading } = useUserId();
  const [title, setTitle] = useState('');
  const [whatToClarify, setWhatToClarify] = useState('');
  const [purpose, setPurpose] = useState('');
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    if (!userId) {
      setError('User ID not available');
      return;
    }

    setIsSubmitting(true);
    setError(null);

    try {
      // Combine fields into context
      const context = `【何の認識を洗い出すか】\n${whatToClarify}\n\n【何のために洗い出すか】\n${purpose}`;

      const response = await axios.post(
        '/api/sessions',
        { title, context },
        { headers: createAuthorizationHeader(userId) }
      );

      const sessionId = response.data.session.id;
      router.push(`/sessions/${sessionId}/admin`);
    } catch (err) {
      console.error('Failed to create session:', err);
      setError('セッションの作成に失敗しました。もう一度お試しください。');
      setIsSubmitting(false);
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
          新しいセッションを作成
        </h1>

        <form onSubmit={handleSubmit} className="bg-white rounded-lg shadow-sm p-6 space-y-6">
          <div>
            <label htmlFor="title" className="block text-sm font-medium text-gray-700 mb-2">
              セッションのタイトル
            </label>
            <input
              type="text"
              id="title"
              value={title}
              onChange={(e) => setTitle(e.target.value)}
              required
              className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent"
              placeholder="例: プロジェクトXの現状認識"
            />
          </div>

          <div>
            <label htmlFor="whatToClarify" className="block text-sm font-medium text-gray-700 mb-2">
              何の認識を洗い出すか
            </label>
            <textarea
              id="whatToClarify"
              value={whatToClarify}
              onChange={(e) => setWhatToClarify(e.target.value)}
              required
              rows={4}
              className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent"
              placeholder="例: プロジェクトXの現状、課題、今後の方向性について"
            />
          </div>

          <div>
            <label htmlFor="purpose" className="block text-sm font-medium text-gray-700 mb-2">
              何のために洗い出すか
            </label>
            <textarea
              id="purpose"
              value={purpose}
              onChange={(e) => setPurpose(e.target.value)}
              required
              rows={4}
              className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent"
              placeholder="例: チーム全体で認識を合わせ、次のアクションを決めるため"
            />
          </div>

          {error && (
            <div className="p-4 bg-red-50 border border-red-200 rounded-lg">
              <p className="text-red-600">{error}</p>
            </div>
          )}

          <button
            type="submit"
            disabled={isSubmitting}
            className="w-full px-6 py-3 bg-gray-800 text-white rounded-lg shadow-sm hover:bg-gray-700 transition-colors disabled:bg-gray-400 disabled:cursor-not-allowed"
          >
            {isSubmitting ? '作成中...' : '作成する'}
          </button>
        </form>
      </div>
    </div>
  );
}
