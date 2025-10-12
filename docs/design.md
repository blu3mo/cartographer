### Cartographer: システム設計書

このドキュメントでは、認識を可視化し、合意形成を促進するウェブサービス「Cartographer」の全体設計について記述します。

### 1\. システム概要

Cartographerは、特定のテーマ（例：「このプロジェクトの現状」）に関する複数人の認識を収集・分析し、可視化するサービスです。ホストユーザー（管理者）が「セッション」を作成し、参加者はそれに回答します。LLMを活用して、回答データから洞察を得るための分析レポートや、さらに深い議論を促すための新たな問い（ステートメント）を生成することが最大の特徴です。

**主要コンポーネント:**

  * **Next.js Frontend:** ユーザーインターフェースを提供。React、Tailwind CSSで構築。
  * **Next.js API Routes (Backend):** ビジネスロジック、データベースとの連携、LLM APIの呼び出しなどを担当。
  * **Neon DB (PostgreSQL):** データ永続化ストア。セッション、ステートメント、回答などの情報を保存。
  * **LLM API (OpenRouter経由でGemini 2.5 Proを利用):** レポート生成、新規ステートメント生成に使用。

### 2\. データベース設計 (Neon DB - PostgreSQL)

正規化を意識しつつ、データの関連性が分かりやすいように設計します。

**テーブル定義:**

1.  **`sessions`**

      * セッションの基本情報を管理します。
      * `id` (UUID, Primary Key): セッションの一意なID
      * `title` (TEXT, NOT NULL): セッションのタイトル
      * `context` (TEXT, NOT NULL): セッションのテーマや目的など、LLMに渡す前提情報をまとめたテキスト
      * `host_secret_key` (UUID, NOT NULL, UNIQUE): ホストユーザーが管理画面にアクセスするための秘密鍵
      * `created_at` (TIMESTAMPTZ, default: now())
      * `updated_at` (TIMESTAMPTZ, default: now())

      *備考:* フロントエンドでは「何の認識を」「何のために」「背景情報」など複数の入力欄を用意し、それらを適切に結合したテキストを`context`としてAPIに送信します。

2.  **`statements`**

      * 各セッションに紐づく質問文を管理します。
      * `id` (UUID, Primary Key): ステートメントの一意なID
      * `session_id` (UUID, Foreign Key to `sessions.id`, NOT NULL): 紐づくセッションID
      * `text` (TEXT, NOT NULL): ステートメントの本文
      * `order_index` (INTEGER, default: 0): 表示順などのためのインデックス
      * `created_at` (TIMESTAMPTZ, default: now())

3.  **`participants`**

      * セッションへの参加者を管理します。ユーザー認証はブラウザの永続ストレージで行うため、一意なIDをキーとします。
      * `id` (UUID, Primary Key): 参加者の一意なID（クライアント側で生成し、localStorageに保存）
      * `session_id` (UUID, Foreign Key to `sessions.id`, NOT NULL): 参加しているセッションID
      * `name` (VARCHAR(255), NOT NULL): 参加者の表示名
      * `latest_individual_report_id` (UUID, Foreign Key to `individual_reports.id`, NULLABLE): 最新の「じぶんレポート」ID
      * `created_at` (TIMESTAMPTZ, default: now())
      * `updated_at` (TIMESTAMPTZ, default: now())
      * *Constraint: UNIQUE(`id`, `session_id`)*

4.  **`responses`**

      * 誰がどのステートメントにどう回答したかを記録します。
      * `id` (UUID, Primary Key): 回答の一意なID
      * `participant_id` (UUID, Foreign Key to `participants.id`, NOT NULL): 回答した参加者ID
      * `statement_id` (UUID, Foreign Key to `statements.id`, NOT NULL): 回答対象のステートメントID
      * `value` (INTEGER, NOT NULL): 回答内容。(-2: Strong No, -1: No, 0: わからない, 1: Yes, 2: Strong Yes)
      * `created_at` (TIMESTAMPTZ, default: now())
      * *Constraint: UNIQUE(`participant_id`, `statement_id`)*

5.  **`situation_analysis_reports`**

      * セッション全体に関する現状分析レポートを管理します。
      * `id` (UUID, Primary Key): レポートの一意なID
      * `session_id` (UUID, Foreign Key to `sessions.id`, NOT NULL): 紐づくセッションID
      * `content_markdown` (TEXT, NOT NULL): LLMが生成したMarkdown形式のレポート内容
      * `created_at` (TIMESTAMPTZ, default: now())

6.  **`individual_reports`**

      * 各参加者向けの「じぶんレポート」を管理します。
      * `id` (UUID, Primary Key): レポートの一意なID
      * `participant_id` (UUID, Foreign Key to `participants.id`, NOT NULL): レポートの対象となる参加者ID
      * `content_markdown` (TEXT, NOT NULL): LLMが生成したMarkdown形式のレポート内容
      * `created_at` (TIMESTAMPTZ, default: now())

**ER図 (概念):**

```
[sessions] 1--* [statements]
[sessions] 1--* [participants]
[sessions] 1--* [situation_analysis_reports]

[participants] 1--* [responses]
[statements]   1--* [responses]

[participants] 1--* [individual_reports]
```

### 3\. オブジェクト設計 (TypeScript)

主要なデータ構造を型として定義します。

```typescript
// データベースのスキーマに対応

interface Session {
  id: string; // UUID
  title: string;
  context: string;
  createdAt: Date;
}

interface Statement {
  id: string; // UUID
  sessionId: string; // UUID
  text: string;
  orderIndex: number;
}

interface Participant {
  id: string; // UUID
  sessionId: string; // UUID
  name: string;
  latestIndividualReportId?: string; // UUID
}

type ResponseValue = -2 | -1 | 0 | 1 | 2;

interface Response {
  id: string; // UUID
  participantId: string; // UUID
  statementId: string; // UUID
  value: ResponseValue;
}

interface SituationAnalysisReport {
  id: string; // UUID
  sessionId: string; // UUID
  contentMarkdown: string;
  createdAt: Date;
}

interface IndividualReport {
  id: string; // UUID
  participantId: string; // UUID
  contentMarkdown: string;
  createdAt: Date;
}

// APIのレスポンスなどで使用する複合的な型

interface StatementWithStats extends Statement {
  responses: {
    strongYes: number; // 割合 (%)
    yes: number;
    dontKnow: number;
    no: number;
    strongNo: number;
    totalCount: number;
  };
  agreementScore: number; // 合意度スコア (例: (yes+strongYes) - (no+strongNo) の絶対値など)
}

interface SessionAdminData extends Session {
  statements: StatementWithStats[];
  latestSituationAnalysisReport?: SituationAnalysisReport;
}

interface SessionParticipantData extends Session {
  participant?: Participant; // 参加済みの場合
  individualReport?: IndividualReport;
}
```

### 4\. API設計

Next.jsのAPI Routes (`/pages/api/*`) を使用します。

LLMへのリクエストはOpenRouter API経由で行い、ターゲットモデルとして`google/gemini-2.5-pro`を利用します。プロジェクトのサーバーサイド環境変数にOpenRouterのAPIキーを保持し、API Routesからの呼び出し時に`HTTP Authorization`ヘッダーへ付与します。

#### 4.1 ユーザー識別

  * クライアント側で初めてサイトにアクセスした際にUUIDを生成し、`localStorage`に`cartographer_user_id`のようなキーで保存します。
  * 以降のAPIリクエストでは、このIDを`Authorization: Bearer <user_id>`ヘッダーに含めて送信します。サーバー側ではこのIDを`participant.id`として扱います。

#### 4.2 ホストユーザー向けAPI

  * **セッション作成**

      * `POST /api/sessions`
      * Request Body: `{ title: string, context: string }`
      * 処理:
        1.  `sessions`テーブルにデータを保存。`host_secret_key`も生成。
        2.  LLMを呼び出し、初期ステートメントを10個生成。
        3.  生成されたステートメントを`statements`テーブルに保存。
      * Response Body: `{ session: Session, hostSecretKey: string }`

  * **管理画面データ取得**

      * `GET /api/sessions/[sessionId]/admin?secret_key=[hostSecretKey]`
      * 処理:
        1.  `secret_key`を検証。
        2.  セッション情報、紐づく全ステートメント、各ステートメントの回答統計（集計処理が必要）、最新の現状分析レポートを取得。
      * Response Body: `{ data: SessionAdminData }`

  * **現状分析レポート生成**

      * `POST /api/sessions/[sessionId]/reports/situation-analysis?secret_key=[hostSecretKey]`
      * 処理 (非同期):
        1.  `secret_key`を検証。
        2.  LLMに渡すためのデータをDBから取得・整形。
              * セッションのコンテキスト (`context`)
              * 全参加者の最新「じぶんレポート」のリスト
              * 全ステートメントを合意度順にソートしたもの（回答率付き）
        3.  LLM APIを呼び出し、レポートを生成。
        4.  結果を`situation_analysis_reports`テーブルに保存。
      * Response Body: `{ report: SituationAnalysisReport }`

  * **新規ステートメント生成**

      * `POST /api/sessions/[sessionId]/statements/generate?secret_key=[hostSecretKey]`
      * 処理 (非同期):
        1.  `secret_key`を検証。
        2.  LLMに渡すためのデータをDBから取得・整形。
              * セッションのコンテキスト (`context`)
              * 既存ステートメントのリスト（回答率付き）
              * 最新の現状分析レポート
        3.  LLM APIを呼び出し、新たなステートメントを5個生成。
        4.  結果を`statements`テーブルに保存。
      * Response Body: `{ newStatements: Statement[] }`

#### 4.3 参加ユーザー向けAPI

  * **公開セッション一覧取得**

      * `GET /api/sessions`
      * 処理: `sessions`テーブルから一覧を取得（将来的には公開/非公開フラグでフィルタリング）。
      * Response Body: `{ sessions: Session[] }`

  * **セッション参加ページ情報取得**

      * `GET /api/sessions/[sessionId]`
      * Header: `Authorization: Bearer <user_id>`
      * 処理:
        1.  セッション情報を取得。
        2.  `user_id`をもとに`participants`テーブルを検索し、参加済みか確認。
        3.  参加済みの場合、その参加者情報と最新の「じぶんレポート」を取得。
      * Response Body: `{ data: SessionParticipantData }`

  * **セッションへの参加登録**

      * `POST /api/sessions/[sessionId]/participants`
      * Header: `Authorization: Bearer <user_id>`
      * Request Body: `{ name: string }`
      * 処理: `participants`テーブルに新しい参加者レコードを作成。
      * Response Body: `{ participant: Participant }`

  * **次のステートメント取得**

      * `GET /api/sessions/[sessionId]/statements/next`
      * Header: `Authorization: Bearer <user_id>`
      * 処理:
        1.  `user_id`が未回答のステートメントの中からランダムに1つ選択。
        2.  全て回答済みの場合はnullを返す。
      * Response Body: `{ statement: Statement | null }`

  * **回答の送信**

      * `POST /api/sessions/[sessionId]/responses`
      * Header: `Authorization: Bearer <user_id>`
      * Request Body: `{ statementId: string, value: ResponseValue }`
      * 処理: `responses`テーブルに回答を記録（既に回答があれば更新）。
      * Response Body: `{ success: true }`

  * **「じぶんレポート」生成・更新**

      * `POST /api/sessions/[sessionId]/individual-report/update`
      * Header: `Authorization: Bearer <user_id>`
      * 処理 (非同期):
        1.  `user_id`に対応する参加者の全回答履歴を取得。
        2.  セッションのコンテキスト (`context`) も取得。
        3.  LLM APIを呼び出し、「じぶんレポート」を生成。
        4.  結果を`individual_reports`テーブルに保存。
        5.  `participants`テーブルの`latest_individual_report_id`を更新。
      * Response Body: `{ report: IndividualReport }`

### 5\. LLMプロンプト設計（概要）

LLMの性能を最大限に引き出すため、プロンプトは詳細に設計する必要があります。

  * **初期ステートメント生成プロンプト:**

      * **役割:** あなたは優れたファシリテーターです。
      * **入力:** `context`
      * **指示:** 以下のテーマと目的に基づき、参加者の多様な視点を引き出すための、示唆に富む10個のステートメント（短い断定文）を生成してください。YES/NOで答えやすい形式にしてください。JSON配列で出力してください。

  * **現状分析レポート生成プロンプト:**

      * **役割:** あなたは鋭い洞察力を持つ組織コンサルタントまたは社会調査アナリストです。
      * **入力:** `context`, 全参加者の最新じぶんレポートリスト, 回答率付きステートメントリスト
      * **指示:** 以下の情報に基づき、現状の分析レポートをMarkdown形式で作成してください。特に「合意が形成されている点」「意見が対立している点」「多くの人がまだ分かっていない、確信が持てない点」を明確に指摘し、目的に対して次に行うべきアクションや、検証すべき仮説を提案してください。

  * **新規ステートメント生成プロンプト:**

      * **役割:** あなたは次の議論をデザインする戦略的なリサーチャーです。
      * **入力:** `context`, 回答率付き既存ステートメントリスト, 最新の現状分析レポート
      * **指示:** 既存の回答状況と分析レポートを踏まえ、議論をさらに深めるための新たなステートメントを5つ生成してください。特に、意見が割れている点や、まだ誰も分かっていない点を掘り下げるような、新たな仮説を検証するための問いを設計してください。JSON配列で出力してください。

  * **じぶんレポート生成プロンプト:**

      * **役割:** あなたは思慮深いコーチまたはカウンセラーです。
      * **入力:** `context`, ある参加者の全回答履歴
      * **指示:** この参加者の回答パターンから、彼/彼女がこのテーマに対してどのような認識を持っているかを分析し、本人向けのフィードバックレポートをMarkdown形式で作成してください。特徴的な回答や、他の人と意見が異なりそうな点を優しく指摘し、自己理解を深める手助けをしてください。
