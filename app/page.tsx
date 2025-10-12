import Link from "next/link";

export default function Home() {
  return (
    <div className="min-h-screen bg-gray-50 py-12 px-4">
      <div className="max-w-3xl mx-auto">
        <h1 className="text-4xl font-bold text-gray-900 mb-4">
          Cartographer
        </h1>
        <p className="text-lg text-gray-700 mb-8">
          認識を可視化し、合意形成を促進するウェブサービス
        </p>

        <div className="bg-white rounded-lg shadow-sm p-6">
          <h2 className="text-2xl font-semibold text-gray-900 mb-4">
            セッション一覧
          </h2>
          <p className="text-gray-600 mb-6">
            セッション一覧機能はPhase 3で実装されます。
          </p>

          <Link
            href="/sessions/new"
            className="inline-block px-6 py-3 bg-gray-800 text-white rounded-lg shadow-sm hover:bg-gray-700 transition-colors"
          >
            新しいセッションを作成
          </Link>
        </div>
      </div>
    </div>
  );
}
