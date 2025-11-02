# Cartographer 開発環境セットアップ & 動作ガイド

このドキュメントは、Cartographer のローカル開発環境を構築し、日々の運用やトラブルシューティングを行うための詳細な手順をまとめたものです。新しく参加した開発者でも、このガイドに沿って進めれば短時間でアプリとエージェントを起動し、主要フローを確認できるようになっています。

---

## 1. システム概要

- **アプリ本体**: Next.js 15 (App Router) + React 19 で構築されたフロントエンド/バックエンド。`app/api` 配下に API ルートを実装。
- **データストア**: Supabase（PostgreSQL/Realtime）を採用。`supabase/schema.sql` でテーブルを管理。
- **LLM 連携**: OpenRouter 経由で Google Gemini 2.5 Pro を呼び出し、セッションゴール・ステートメント・分析レポートを生成。
- **エージェント**: `npm run agent:dev` で起動する Node.js プロセス。Supabase Realtime を購読し、Plan → Survey → Analysis のイベント生成を自動化（`agents/AgentManager.ts` + `agents/PtolemyAgent.ts`）。

### 1.1 コンポーネントのつながり（ざっくり版）

1. **ブラウザ (Next.js アプリ)**
   - ユーザーがセッションを作成・回答するための UI を提供。
   - 「API を呼ぶ → Supabase に書き込む → 画面を再描画」という役割。
2. **Next.js API**
   - ブラウザから届いたリクエストを受け取り、Supabase に保存・取得する中継役。
   - LLM を呼び出してゴールやレポートを生成し、その結果も Supabase に保管。
3. **Supabase (PostgreSQL + Realtime)**
   - すべてのデータの置き場所。セッション・ステートメント・回答・イベントなどを一括管理。
   - Realtime 機能が、データの変化をエージェントへ通知する「ベル」の役割を担う。
4. **Ptolemy エージェント (Node.js プロセス)**
   - Supabase から届く通知を聞きつつ、「Plan を書く」「質問を作る」「分析を書く」といった重たい仕事を代行。
   - 結果は再び Supabase に書き戻し、Next.js 側がそれを読み取って UI に反映する。

要するに、**Next.js がユーザーと Supabase をつなぎ、Supabase がデータのハブとなり、エージェントが裏側で自動生成処理を動かす**という三者リレーで成り立っています。詳しい技術要素を知らなくても、「画面 ↔ API ↔ データベース ↔ 自動処理」の循環をイメージできれば OK です。

---

## 2. リポジトリ構成

| パス | 説明 |
| --- | --- |
| `app/` | Next.js アプリ（ページ・API・共有ライブラリ） |
| `agents/` | Ptolemy エージェント実装とラッパー |
| `supabase/schema.sql` | DB スキーマ定義と Realtime 設定 |
| `docs/` | 設計・運用ノート (`docs/design.md`, `docs/agentic.md` など) |
| `middleware.ts` | Basic 認証・IP 制限などのミドルウェア |
| `package.json` | npm スクリプトと依存パッケージ |

---

## 3. 必要な外部サービス・アカウント

1. **Supabase プロジェクト**
   - プロジェクト URL (`NEXT_PUBLIC_SUPABASE_URL`)
   - anon キー (`NEXT_PUBLIC_SUPABASE_ANON_KEY`)
   - Service Role キー (`SUPABASE_SERVICE_ROLE_KEY`) ※サーバーサイド専用
2. **OpenRouter アカウント**
   - `OPENROUTER_API_KEY`（Gemini 2.5 Pro を利用可能なプラン）
3. 任意: Neon 等の外部 PostgreSQL を利用する場合は、Supabase 互換の Realtime 設定を用意。

---

## 4. 前提ツール

- Node.js 18.x 以上（推奨: Node.js 20 LTS）
- npm（標準で付属）または pnpm/bun（任意）
- Git
- Supabase SQL Editor または `psql` など SQL を実行できるツール
- （任意）Supabase CLI: マイグレーションを CLI から適用したい場合に便利

インストール確認:

```bash
node -v
npm -v
psql --version
```

---

## 5. 環境変数の設定

### 5.1 `.env.local`

Next.js（フロントエンド & API）用。`OPENROUTER_API_KEY` と Supabase の URL/Key を設定します。

```env
OPENROUTER_API_KEY=sk-or-xxxx

NEXT_PUBLIC_SUPABASE_URL=https://your-project.supabase.co
NEXT_PUBLIC_SUPABASE_ANON_KEY=eyJhbGciOi...
SUPABASE_SERVICE_ROLE_KEY=eyJhbGciOi...

# 任意: アクセス制御
# BASIC_AUTH_USERNAME=admin
# BASIC_AUTH_PASSWORD=secret
# ALLOWED_IPS=203.0.113.10,198.51.100.7
```

> `SUPABASE_SERVICE_ROLE_KEY` はサーバーサイドでのみ使用します。クライアントに露出しないよう注意してください。

### 5.2 `.env.agent`（任意）

エージェント起動時に読み込まれる設定。`.env.local` が同じ内容を持つ場合は省略可能ですが、エージェントのみ別環境を使いたい場合に便利です。

```env
OPENROUTER_API_KEY=sk-or-xxxx
NEXT_PUBLIC_SUPABASE_URL=https://your-project.supabase.co
SUPABASE_SERVICE_ROLE_KEY=eyJhbGciOi...
```

`agents/index.ts` は `.env.agent` → `.env.local` → `.env` の順で読み込み、既に定義済みの環境変数は上書きしません。

---

## 6. 依存パッケージのインストール

```bash
npm install
```

初回セットアップ後、`node_modules/` が生成されます。依存関係の更新は `package-lock.json` に反映されます（コミット対象）。

---

## 7. データベース準備

1. Supabase のプロジェクトを作成し、Database → SQL Editor を開きます。
2. `supabase/schema.sql` の内容をまるごと貼り付けて実行してください。
   - テーブル・インデックス・外部キー・Realtime publication の設定が一度に行われます。
   - 既存テーブルがある場合でも `create table if not exists` で安全に適用されます。
3. Realtime を有効にするため、Supabase の「Database → Replication」で `supabase_realtime` が active になっているか確認します。
4. 追加テーブルやポリシーを導入する場合は SQL を追記し、再度実行してください。

---

## 8. 開発サーバーの起動

フロントエンド/Next.js API を起動:

```bash
npm run dev
```

- デフォルトで `http://localhost:3000` が立ち上がります。
- Turbopack を使用しており、ソース変更は高速に反映されます。
- `.env.local` の変更を反映させる場合はサーバーを再起動してください。

---

## 9. エージェントの起動

Plan → Survey → Analysis の自動生成には、別ターミナルでエージェントを起動する必要があります。

```bash
npm run agent:dev
```

- `agents/AgentManager.ts` が Supabase Realtime を購読し、`agent_instances` が変化すると `agents/PtolemyAgent.ts` の状態機械を進めます。
- 安全に終了する際は `Ctrl + C`。内部で SIGINT/SIGTERM を捕捉し、サブスクリプションを解除します。
- エージェント起動中のログは Plan/Suite 生成のトラブルシューティングに役立つので、別ウィンドウで監視するのがおすすめです。

---

## 10. アプリの基本フロー

### 10.1 ホスト（管理者）

1. トップページ (`/`) で「新しいセッション」をクリック。
2. セッションタイトル、関係者、得たい洞察などの入力を埋め、必要なら「ゴールを生成」を実行（`app/sessions/new/page.tsx`）。
3. 作成完了後、`/sessions/{id}/admin` に遷移。Event Thread タイムラインが表示されます。
4. `shouldProceed` を切り替えることでエージェントの進行を制御可能。User Message を追加するとプロンプト履歴に反映されます。

### 10.2 参加者

1. `/sessions/{id}` にアクセス。初回アクセス時に名前を入力して参加登録（`app/api/sessions/[sessionId]/participants/route.ts`）。
2. ステートメントカードに表示された問いへ 5 段階（-2〜2）の回答を送信。
3. 任意で「じぶんレポート」を生成すると、自分の回答傾向に基づいた Markdown レポートが `individual_reports` に保存されます。

### 10.3 自動生成フロー

- `ensureEventThreadForSession` がセッション作成時に `event_threads` と `agent_instances` を自動用意し、初期 `user_message` を投入します (`app/lib/server/event-threads.ts`)。
- エージェントは状態遷移に応じて `events` テーブルへ `plan` / `survey` / `survey_analysis` を追加し、必要に応じて `statements` と `responses` を参照します。
- 回答率が 80% を超えると Survey が完了とみなされ、Analysis 生成 → 再度 Plan 生成のループに戻ります (`agents/PtolemyAgent.ts` 内の `COLLECTING_SURVEY` など)。

---

## 11. よく使う npm スクリプト

| コマンド | 説明 |
| --- | --- |
| `npm run dev` | Next.js + Turbopack 開発サーバー |
| `npm run build` | 本番ビルド（Turbopack） |
| `npm run start` | 本番サーバー起動（ビルド済み成果物を使用） |
| `npm run agent:dev` | Ptolemy エージェントの常駐プロセス |
| `npm run lint` | Biome による静的解析 |
| `npm run format` | Biome による整形（`--write`） |

---

## 12. テストと検証

現時点で自動テストは整備されていないため、以下を手動確認してください。

1. 新規セッション作成 → 管理画面表示
2. 参加者が回答を完了し、Responses テーブルに保存されること
3. エージェントが Plan/Suvey/Analysis を生成できること
4. 個人レポートが生成され、再訪時にも閲覧できること

Supabase ダッシュボードで `sessions`, `statements`, `events` などのテーブルを直接確認し、想定通りのデータが残っているかチェックすると確実です。

---

## 13. トラブルシューティング

| 症状 | チェックポイント |
| --- | --- |
| API が 401 を返す | ローカルストレージ `cartographer_user_id` が存在するか、リクエストに `Authorization: Bearer <id>` が付与されているか（`app/lib/useUserId.ts`）。 |
| セッション作成後に Event が出てこない | Supabase で `event_threads` / `agent_instances` が作成されているか確認。エージェントが起動していない可能性。 |
| エージェントが進行しない | `should_proceed` が `true` か、`agents` プロセスのログにエラーが出ていないか（OpenRouter API キーや Supabase 接続を確認）。 |
| LLM 呼び出しが失敗する | OpenRouter の利用制限（Rate Limit）や課金状況、API キーが正しいかを確認。`agents/llm.ts` や `app/lib/llm.ts` のログに詳細が出ます。 |
| Basic 認証が無限ループ | ブラウザで保存されている認証ヘッダーや Cookie を削除し、正しい資格情報を再入力。 |
| DB 反映が遅い | Realtime が有効か確認。`supabase_realtime` Publication に対象テーブルが追加されているか（`supabase/schema.sql` 末尾）を再適用。 |

---

## 14. 運用上のヒント

- **shouldProceed の活用**: 管理画面のトグルでエージェントを一時停止できます。Plan を確認してから Survey に進ませたい場合などに有効です。
- **User Message**: セッション文脈に追加指示を与えたいときは管理画面からメッセージを投稿すると `events` に `user_message` として記録され、次回生成時のプロンプトに反映されます。
- **ステートメントの整理**: `statements` テーブルには `order_index` が付与されるので、不要になった質問を手動で削除する際は整合性に注意しましょう。
- **セキュリティ**: Service Role Key をフロントエンドへ渡さない・`.env*` を Git 管理下に置かない・IP/BASIC 認証を活用するなど、運用環境に合わせてガードを追加してください。

---

## 15. 参考ドキュメント

- `docs/design.md` … システム設計の詳細や画面仕様
- `docs/agentic.md` … Event Thread / Agent のアーキテクチャ解説
- `docs/steps.md` … システム全体を構築するためのマクロ手順

必要に応じてこれらも併読し、より深い背景情報を把握してください。

---

## 16. 次のステップ

1. 上記手順でローカル環境を起動し、セッション作成 → 回答 → レポート生成の一連の流れを検証する。
2. Biome で lint/format を走らせ、コード変更がある場合は `npm run lint` / `npm run format` を実行。
3. 開発が進んだら、README や本ガイドを更新し、運用メモ（`docs/`）と整合性を保つ。

このガイドは随時更新していく想定です。新しい運用フローや依存関係が追加された場合は、該当セクションを追記してください。

---

Happy mapping!
