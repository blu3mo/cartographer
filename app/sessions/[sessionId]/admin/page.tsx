'use client';

import { use } from 'react';

export default function AdminPage({
  params,
}: {
  params: Promise<{ sessionId: string }>;
}) {
  const { sessionId } = use(params);

  return (
    <div className="min-h-screen bg-gray-50 py-12 px-4">
      <div className="max-w-4xl mx-auto">
        <h1 className="text-3xl font-bold text-gray-900 mb-8">
          管理画面
        </h1>
        <div className="bg-white rounded-lg shadow-sm p-6">
          <p className="text-gray-600">
            Session ID: {sessionId}
          </p>
          <p className="text-gray-600 mt-4">
            管理画面はPhase 2で実装されます。
          </p>
        </div>
      </div>
    </div>
  );
}
