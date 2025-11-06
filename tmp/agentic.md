# Agentic Workflow Overview

このドキュメントでは、新しく導入した Event Thread / Agent ベースのワークフローについてまとめます。Next.js のバックエンド、Supabase スキーマ、外部 Agent サービス（Ptolemy）それぞれの責務を明確にし、実装/運用で迷わないようにします。

## 1. Event Thread モデル

| リソース | 役割 | 主なフィールド |
| --- | --- | --- |
| `event_threads` | セッションごとのワークフローコンテナ。shouldProceed で Agent の進行を制御。 | `session_id`, `should_proceed`, `created_at`, `updated_at` |
| `events` | 時系列イベント。type により Plan / Survey / SurveyAnalysis / UserMessage を表現。 | `thread_id`, `type`, `agent_id`, `user_id`, `progress (0-1)`, `payload (jsonb)` |
| `agent_instances` | Event Thread を処理する Agent の状態管理。 | `thread_id`, `agent_type`, `state`, `state_payload` |

### Event Payload の形

* Plan: `{ markdown: string }`
* Survey: `{ statementIds: string[] }`
* SurveyAnalysis: `{ statementIds: string[], markdown: string }`
* UserMessage: `{ markdown: string, metadata?: Record<string, unknown> }`

## 2. Next.js API

新設された API ルート:

| Method/Path | 説明 |
| --- | --- |
| `GET /api/sessions/[id]/event-thread` | スレッド・イベント・紐づくエージェントの一覧を返却。 |
| `POST /api/sessions/[id]/event-thread/events/user-message` | 管理画面から UserMessage Event を追加。 |
| `PATCH /api/sessions/[id]/event-thread/should-proceed` | shouldProceed の ON/OFF をトグル。 |

`POST /api/sessions` ではセッション生成後に `ensureEventThreadForSession` を呼び出し、Event Thread・初期 UserMessage・Ptolemy Agent instance を自動で用意します。既存セッションに対しても GET/PATCH/POST 時に不足分があれば補完されるため、移行コストは最小限です。

## 3. 管理画面 UI

* **Event Thread タイムライン** – すべての Event をカード表示（デフォルト折りたたみ）。Progress バー、Agent/User、作成/更新日時、payload の詳細（Markdown）を参照可能。
* **shouldProceed トグル** – WAITING\_\* ステートで Agent の進行を許可/停止。
* **User Message 投稿欄** – context や指示を任意に記録。送信すると即座に Event が追加される。
* **Agent Monitor** – Ptolemy の state/state\_payload を一覧表示し、最終更新を追跡。

UI は 5 秒間隔で Event Thread API をポーリングし、最新状態へ追従します。

## 4. Ptolemy Agent

スタンドアロン Node.js サービス（`npm run agent:dev` で起動）。

### 状態マシン

1. `CREATING_PLAN` → Plan Event 作成、ダミー Markdown を 2 秒後に投入。
2. `WAITING_SURVEY` → shouldProceed が true なら `CREATING_SURVEY` へ。
3. `CREATING_SURVEY` → Survey Event と 10 件のダミー Statement を生成、progress=0.5。
4. `COLLECTING_SURVEY` → responses 監視。すべての Statement で回答率 80% 以上になると progress=1。
5. `WAITING_ANALYSIS` → shouldProceed true で `CREATING_ANALYSIS`。
6. `CREATING_ANALYSIS` → 最新 Statement 群をもとにダミー Markdown を生成。
7. `WAITING_PLAN` → shouldProceed true で再び `CREATING_PLAN` へループ。

### Realtime トリガー

* `event_threads` UPDATE – shouldProceed 変更を検知し WAITING ステートを再評価。
* `responses` 変化 – `COLLECTING_SURVEY` 中の Agent を起こし回答率を再計算。
* `agent_instances` INSERT – 新規 Thread ができたら即座に処理を開始。

### 環境変数

* `NEXT_PUBLIC_SUPABASE_URL`
* `SUPABASE_SERVICE_ROLE_KEY`

`agents/index.ts` に簡易 `.env.agent` / `.env.local` ローダーを実装済み。上記ファイルにキーを書いておけば `npm run agent:dev` で読み込まれます（既存の環境変数が優先されます）。

## 5. 今後の拡張ポイント

* Agent 種類の追加 – `agent_instances.agent_type` に応じて `AgentManager` が別クラスを呼び出せる設計。
* Realtime 以外のトリガー – Supabase Edge Function などで `responses` 集計を push することも可能。
* shouldProceed の自動制御 – WAITING に入ったタイミングで自動的に false へ倒すなど、ガードレールの追加。

以上が v1 実装の全体像です。詳細な処理フローは `agents/PtolemyAgent.ts`、API 実装は `app/api/sessions/[sessionId]/event-thread/*` を参照してください。
