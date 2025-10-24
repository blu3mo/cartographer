# Event Thread & Agent System 設計書

## 概要

本システムは、現在のフロントエンドとバックエンドのStatement自動生成、現状分析レポート生成、新しいStatement生成を、Event ThreadベースのAgent Systemに置き換えるものです。

### 置き換え対象

**フロントエンド:**
- `app/sessions/[sessionId]/admin/page.tsx` の「現状分析レポートを生成」ボタン
- `app/sessions/[sessionId]/admin/page.tsx` の「新しいStatementを10個生成」ボタン
- セッション作成時の初期Statement自動生成

**バックエンド:**
- `app/lib/llm.ts` の `generateInitialStatements`
- `app/lib/llm.ts` の `generateSituationAnalysis`
- `app/lib/llm.ts` の `generateNewStatements`

---

## 1. データモデル設計

### 1.1 EventThread Model

```prisma
model EventThread {
  id          String   @id @default(uuid()) @db.Uuid
  sessionId   String   @unique @map("session_id") @db.Uuid
  shouldProceed Boolean @default(true) @map("should_proceed")
  createdAt   DateTime @default(now()) @map("created_at") @db.Timestamptz
  updatedAt   DateTime @default(now()) @updatedAt @map("updated_at") @db.Timestamptz

  session     Session  @relation(fields: [sessionId], references: [id], onDelete: Cascade)
  events      Event[]

  @@map("event_threads")
}
```

**フィールド説明:**
- `id`: EventThreadの一意識別子
- `sessionId`: 対応するSessionのID（1:1関係）
- `shouldProceed`: Agentが処理を続けるべきかのフラグ
- `createdAt`, `updatedAt`: 作成・更新日時

---

### 1.2 Event Model (Union Type)

Eventは4つのタイプを持つポリモーフィックなモデルです。

```prisma
model Event {
  id            String      @id @default(uuid()) @db.Uuid
  eventThreadId String      @map("event_thread_id") @db.Uuid
  eventType     EventType   @map("event_type")

  // Common fields
  progress      Float       @default(0) // 0.0 ~ 1.0
  createdAt     DateTime    @default(now()) @map("created_at") @db.Timestamptz
  updatedAt     DateTime    @default(now()) @updatedAt @map("updated_at") @db.Timestamptz
  orderIndex    Int         @map("order_index") // Display order in thread

  // Type-specific nullable fields
  agentId       String?     @map("agent_id") @db.VarChar(255) // For Plan, Survey, SurveyAnalysis
  userId        String?     @map("user_id") @db.Uuid // For UserMessage
  contentMarkdown String?   @map("content_markdown") @db.Text // For Plan, SurveyAnalysis, UserMessage
  statementIds  String[]    @map("statement_ids") @db.Uuid[] // For Survey, SurveyAnalysis

  eventThread   EventThread @relation(fields: [eventThreadId], references: [id], onDelete: Cascade)

  @@index([eventThreadId, orderIndex])
  @@map("events")
}

enum EventType {
  PLAN
  SURVEY
  SURVEY_ANALYSIS
  USER_MESSAGE

  @@map("event_type")
}
```

**フィールド説明:**
- `eventType`: Plan / Survey / SurveyAnalysis / UserMessage
- `progress`: 0.0~1.0の進捗率
- `orderIndex`: EventThread内での表示順序（小さいほど古い）
- `agentId`: AgentのID（"ptolemy", "future_agent", etc.）
- `userId`: UserMessageの場合のユーザーID
- `contentMarkdown`: Markdown形式のコンテンツ
- `statementIds`: SurveyまたはSurveyAnalysisが対象とするStatement IDのリスト

**Event Type別の使用フィールド:**

| Type | agentId | userId | contentMarkdown | statementIds |
|------|---------|--------|-----------------|--------------|
| PLAN | ✓ | - | ✓ | - |
| SURVEY | ✓ | - | - | ✓ |
| SURVEY_ANALYSIS | ✓ | - | ✓ | ✓ |
| USER_MESSAGE | - | ✓ | ✓ | - |

---

### 1.3 Session Model への拡張

既存のSessionモデルに`eventThread`リレーションを追加:

```prisma
model Session {
  // ... 既存のフィールド
  eventThread EventThread?
}
```

---

### 1.4 AgentState Model（Agent処理状態の永続化用）

```prisma
model AgentState {
  id            String   @id @default(uuid()) @db.Uuid
  agentId       String   @map("agent_id") @db.VarChar(255) // "ptolemy", etc.
  sessionId     String   @map("session_id") @db.Uuid
  currentPhase  String   @map("current_phase") @db.VarChar(100) // "plan", "survey", "analysis", "idle"
  isProcessing  Boolean  @default(false) @map("is_processing")
  lastEventId   String?  @map("last_event_id") @db.Uuid // 最後に処理したEventのID
  metadata      Json?    @default("{}") // Agent固有の状態データ
  createdAt     DateTime @default(now()) @map("created_at") @db.Timestamptz
  updatedAt     DateTime @default(now()) @updatedAt @map("updated_at") @db.Timestamptz

  session       Session  @relation(fields: [sessionId], references: [id], onDelete: Cascade)

  @@unique([agentId, sessionId])
  @@index([sessionId])
  @@map("agent_states")
}
```

**用途:**
- Agentが常時起動しているプロセスではなく、イベント駆動で起動するため、状態を永続化
- race conditionを防ぐため`isProcessing`フラグで排他制御
- `currentPhase`でAgentがどの処理段階にあるかを保持

---

## 2. システムアーキテクチャ

### 2.1 全体構成

```
┌─────────────────────────────────────────────────────┐
│         Next.js Frontend (Port 3000)                │
│  - EventThread 表示画面                             │
│  - UserMessage 送信                                 │
│  - shouldProceed 制御                               │
└─────────────────┬───────────────────────────────────┘
                  │ API calls
                  │ (HTTP REST)
┌─────────────────▼───────────────────────────────────┐
│         Next.js Backend API Routes                  │
│  - /api/sessions/[id]/event-thread                  │
│  - /api/sessions/[id]/events                        │
│  - /api/sessions/[id]/should-proceed                │
└─────────────────┬───────────────────────────────────┘
                  │
                  │ Both access
                  │ PostgreSQL
┌─────────────────▼───────────────────────────────────┐
│         PostgreSQL Database                         │
│  - event_threads, events, agent_states              │
│  - sessions, statements, responses, etc.            │
└─────────────────┬───────────────────────────────────┘
                  │ Direct DB access
                  │ Pub/Sub notifications
┌─────────────────▼───────────────────────────────────┐
│    Agent System (Independent Node.js Process)       │
│                                                      │
│  ┌────────────────────────────────────────────────┐ │
│  │   AgentOrchestrator                            │ │
│  │   - shouldProceed 監視 (Pub/Sub)              │ │
│  │   - Agent タスクの起動                         │ │
│  │   - Race condition 制御                        │ │
│  └──────────┬─────────────────────────────────────┘ │
│             │                                        │
│  ┌──────────▼──────────┐  ┌──────────────────────┐ │
│  │  Ptolemy Agent      │  │  Future Agent 2      │ │
│  │  (v1)               │  │  (Placeholder)       │ │
│  │  - Plan生成         │  │                      │ │
│  │  - Survey生成       │  │                      │ │
│  │  - Analysis生成     │  │                      │ │
│  └─────────────────────┘  └──────────────────────┘ │
│                                                      │
└──────────────────────────────────────────────────────┘
```

---

### 2.2 Agent System の独立性

Agent Systemは**Next.jsとは別のNode.jsプロセス**として動作します。

**理由:**
- Next.jsのサーバーレス関数には実行時間制限がある
- 長時間実行や複雑な状態管理が必要
- Agentのスケーラビリティと監視を独立して管理したい

**実装方針:**
- `agent-system/` ディレクトリに独立したTypeScriptプロジェクトを作成
- 独自の`package.json`, `tsconfig.json`を持つ
- `npm run dev:agents` で起動
- PrismaクライアントでDBに直接アクセス
- PostgreSQL LISTEN/NOTIFYまたはポーリングでshouldProceed変更を検知

---

## 3. フロントエンド設計

### 3.1 EventThread表示画面

**場所:** `/app/sessions/[sessionId]/admin/page.tsx` に統合

**UI構成:**

```
┌────────────────────────────────────────────────────────┐
│ セッション管理画面                                     │
│                                                        │
│ ┌────────────────────────────────────────────────────┐ │
│ │ セッション設定 (既存)                              │ │
│ └────────────────────────────────────────────────────┘ │
│                                                        │
│ ┌────────────────────────────────────────────────────┐ │
│ │ Event Thread                                       │ │
│ │                                                    │ │
│ │ ┌──────────────────────────────────────────────┐  │ │
│ │ │ [Plan Event - Ptolemy] ▼                    │  │ │
│ │ │ Progress: ███████████████████ 100%          │  │ │
│ │ │                                              │  │ │
│ │ │ [Collapsed content - click to expand]       │  │ │
│ │ └──────────────────────────────────────────────┘  │ │
│ │                                                    │ │
│ │ ┌──────────────────────────────────────────────┐  │ │
│ │ │ [Survey Event - Ptolemy] ▼                  │  │ │
│ │ │ Progress: ████████░░░░░░░░░░ 50%            │  │ │
│ │ │                                              │  │ │
│ │ │ Statements: 10件生成済み                    │  │ │
│ │ │ Responses: 8/10 participants (80%)          │  │ │
│ │ └──────────────────────────────────────────────┘  │ │
│ │                                                    │ │
│ │ ┌──────────────────────────────────────────────┐  │ │
│ │ │ [User Message - You] ▼                      │  │ │
│ │ │                                              │  │ │
│ │ │ "次は〇〇について調査してください"            │  │ │
│ │ └──────────────────────────────────────────────┘  │ │
│ │                                                    │ │
│ │ ┌──────────────────────────────────────────────┐  │ │
│ │ │ Send Message: [___________________________] │  │ │
│ │ │                                  [Send 🚀]  │  │ │
│ │ └──────────────────────────────────────────────┘  │ │
│ │                                                    │ │
│ │ Control:  [⏸️ Pause] / [▶️ Resume]  (shouldProceed)│ │
│ └────────────────────────────────────────────────────┘ │
│                                                        │
│ ┌────────────────────────────────────────────────────┐ │
│ │ ステートメント一覧 (既存)                          │ │
│ └────────────────────────────────────────────────────┘ │
└────────────────────────────────────────────────────────┘
```

**Event表示のデザイン指針:**
- **コンパクト:** 画面に多くのEventが表示できるよう、文字サイズは小さめ
- **Collapse/Expand:** 長いMarkdownコンテンツはデフォルトで折りたたみ
- **Progress Bar:** 視覚的に進捗が分かるプログレスバー
- **Color Coding:**
  - Plan: 青系
  - Survey: 緑系
  - SurveyAnalysis: 紫系
  - UserMessage: グレー系
- **Minimal borders & shadows:** 情報密度を上げるため余白を最小限に

---

### 3.2 UserMessage送信機能

**実装:**
- Event Thread表示の下部にテキスト入力フォーム
- 送信時にPOST `/api/sessions/[sessionId]/events`
- サーバー側でUserMessage Eventを作成し、EventThreadに追加
- 作成後、shouldProceedがfalseの場合は自動的にtrueにする（オプション）

---

### 3.3 shouldProceed制御

**実装:**
- `[⏸️ Pause]` / `[▶️ Resume]` トグルボタン
- PATCH `/api/sessions/[sessionId]/should-proceed` で更新
- 現在の状態はEventThreadから取得

---

### 3.4 リアルタイム更新（Optional, Phase 2）

**実装案:**
- Server-Sent Events (SSE) または WebSocket
- Agentが Event を更新するたびにフロントエンドに通知
- Phase 1 では単純なポーリング（3秒ごとにAPI呼び出し）で実装

---

## 4. バックエンドAPI設計

### 4.1 EventThread作成（セッション作成時）

**既存のAPI:** `POST /api/sessions`

**変更点:**
1. Sessionを作成
2. EventThreadを作成（`sessionId`, `shouldProceed: true`）
3. UserMessage Eventを作成（`contentMarkdown: session.context`）
4. AgentStateを初期化（`ptolemy`, `idle`, `isProcessing: false`）

---

### 4.2 EventThread取得

**新規API:** `GET /api/sessions/[sessionId]/event-thread`

**レスポンス:**
```typescript
{
  eventThread: {
    id: string;
    sessionId: string;
    shouldProceed: boolean;
    createdAt: string;
    updatedAt: string;
    events: Array<{
      id: string;
      eventType: "PLAN" | "SURVEY" | "SURVEY_ANALYSIS" | "USER_MESSAGE";
      progress: number;
      createdAt: string;
      updatedAt: string;
      orderIndex: number;
      // Type-specific fields
      agentId?: string;
      userId?: string;
      contentMarkdown?: string;
      statementIds?: string[];
    }>;
  };
}
```

**認可:**
- セッションのホストユーザーのみアクセス可能

---

### 4.3 Event作成（UserMessage）

**新規API:** `POST /api/sessions/[sessionId]/events`

**リクエスト:**
```typescript
{
  message: string; // User message content
}
```

**処理:**
1. EventThreadを取得
2. 最新の`orderIndex`を取得し、+1した値で新しいEventを作成
3. `eventType: USER_MESSAGE`, `userId: <current-user>`, `contentMarkdown: message`, `progress: 1.0`
4. （Optional）shouldProceedをtrueに設定
5. EventThreadのupdatedAtを更新

**レスポンス:**
```typescript
{
  event: Event;
}
```

---

### 4.4 shouldProceed更新

**新規API:** `PATCH /api/sessions/[sessionId]/should-proceed`

**リクエスト:**
```typescript
{
  shouldProceed: boolean;
}
```

**処理:**
1. EventThreadを取得
2. `shouldProceed`を更新
3. PostgreSQL NOTIFY でAgent Systemに通知（`channel: 'event_thread_should_proceed'`, `payload: sessionId`）

**レスポンス:**
```typescript
{
  eventThread: EventThread;
}
```

---

## 5. Agent System設計

### 5.1 ディレクトリ構成

```
agent-system/
├── package.json
├── tsconfig.json
├── src/
│   ├── index.ts                  // Entry point
│   ├── orchestrator.ts           // AgentOrchestrator
│   ├── agents/
│   │   ├── base-agent.ts         // BaseAgent abstract class
│   │   ├── ptolemy-agent.ts      // Ptolemy Agent implementation
│   │   └── index.ts              // Agent registry
│   ├── db/
│   │   └── client.ts             // Prisma client instance
│   ├── services/
│   │   └── event-service.ts      // Event CRUD operations
│   └── utils/
│       ├── logger.ts             // Logging utility
│       └── retry.ts              // Retry logic
├── .env                          // DATABASE_URL, etc.
└── README.md
```

---

### 5.2 AgentOrchestrator

**責務:**
- shouldProceed変更の監視
- 対象SessionのAgent起動
- Race condition制御（同時実行の防止）

**実装:**

```typescript
// agent-system/src/orchestrator.ts

import { PrismaClient } from '@prisma/client';
import { getAgent } from './agents';
import { logger } from './utils/logger';

export class AgentOrchestrator {
  private prisma: PrismaClient;
  private pollInterval: number = 5000; // 5 seconds
  private isShuttingDown: boolean = false;

  constructor(prisma: PrismaClient) {
    this.prisma = prisma;
  }

  /**
   * Start orchestrator (polling mode)
   */
  async start() {
    logger.info('AgentOrchestrator started');

    while (!this.isShuttingDown) {
      await this.checkAndProcessThreads();
      await this.sleep(this.pollInterval);
    }
  }

  /**
   * Check all threads with shouldProceed=true and trigger agents
   */
  private async checkAndProcessThreads() {
    try {
      const threads = await this.prisma.eventThread.findMany({
        where: { shouldProceed: true },
        include: {
          session: true,
          events: {
            orderBy: { orderIndex: 'asc' },
          },
        },
      });

      for (const thread of threads) {
        await this.processThread(thread);
      }
    } catch (error) {
      logger.error('Error checking threads:', error);
    }
  }

  /**
   * Process a single thread
   */
  private async processThread(thread: any) {
    const sessionId = thread.sessionId;

    // Check AgentState for "ptolemy"
    let agentState = await this.prisma.agentState.findUnique({
      where: {
        agentId_sessionId: {
          agentId: 'ptolemy',
          sessionId,
        },
      },
    });

    // Initialize if not exists
    if (!agentState) {
      agentState = await this.prisma.agentState.create({
        data: {
          agentId: 'ptolemy',
          sessionId,
          currentPhase: 'idle',
          isProcessing: false,
        },
      });
    }

    // Skip if already processing (race condition prevention)
    if (agentState.isProcessing) {
      logger.debug(`Agent ptolemy for session ${sessionId} is already processing, skip`);
      return;
    }

    // Lock agent (set isProcessing=true)
    await this.prisma.agentState.update({
      where: { id: agentState.id },
      data: { isProcessing: true },
    });

    try {
      // Get and execute agent
      const agent = getAgent('ptolemy');
      if (agent) {
        await agent.execute(sessionId);
      }
    } catch (error) {
      logger.error(`Error executing agent ptolemy for session ${sessionId}:`, error);
    } finally {
      // Unlock agent
      await this.prisma.agentState.update({
        where: { id: agentState.id },
        data: { isProcessing: false },
      });
    }
  }

  /**
   * Graceful shutdown
   */
  async shutdown() {
    logger.info('AgentOrchestrator shutting down...');
    this.isShuttingDown = true;
  }

  private sleep(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}
```

**補足:**
- **ポーリング方式:** 5秒ごとにDB確認（シンプルで確実）
- **LISTEN/NOTIFY方式（Phase 2）:** より効率的だが複雑
- **isProcessing flag:** race conditionを防ぐためのロック機構

---

### 5.3 BaseAgent（抽象クラス）

```typescript
// agent-system/src/agents/base-agent.ts

import { PrismaClient } from '@prisma/client';

export abstract class BaseAgent {
  protected prisma: PrismaClient;
  abstract agentId: string;

  constructor(prisma: PrismaClient) {
    this.prisma = prisma;
  }

  /**
   * Main execution logic
   */
  abstract execute(sessionId: string): Promise<void>;

  /**
   * Check if agent should continue (shouldProceed check)
   */
  protected async shouldContinue(sessionId: string): Promise<boolean> {
    const thread = await this.prisma.eventThread.findUnique({
      where: { sessionId },
    });
    return thread?.shouldProceed ?? false;
  }

  /**
   * Update agent state
   */
  protected async updateAgentState(
    sessionId: string,
    updates: { currentPhase?: string; metadata?: any }
  ) {
    await this.prisma.agentState.update({
      where: {
        agentId_sessionId: {
          agentId: this.agentId,
          sessionId,
        },
      },
      data: updates,
    });
  }

  /**
   * Get agent state
   */
  protected async getAgentState(sessionId: string) {
    return this.prisma.agentState.findUnique({
      where: {
        agentId_sessionId: {
          agentId: this.agentId,
          sessionId,
        },
      },
    });
  }

  /**
   * Create a new Event in the thread
   */
  protected async createEvent(
    sessionId: string,
    eventData: {
      eventType: 'PLAN' | 'SURVEY' | 'SURVEY_ANALYSIS' | 'USER_MESSAGE';
      progress?: number;
      contentMarkdown?: string;
      statementIds?: string[];
    }
  ) {
    const thread = await this.prisma.eventThread.findUnique({
      where: { sessionId },
      include: { events: { orderBy: { orderIndex: 'desc' }, take: 1 } },
    });

    if (!thread) throw new Error('EventThread not found');

    const maxOrderIndex = thread.events[0]?.orderIndex ?? -1;
    const newOrderIndex = maxOrderIndex + 1;

    return this.prisma.event.create({
      data: {
        eventThreadId: thread.id,
        eventType: eventData.eventType,
        agentId: this.agentId,
        progress: eventData.progress ?? 0,
        contentMarkdown: eventData.contentMarkdown,
        statementIds: eventData.statementIds ?? [],
        orderIndex: newOrderIndex,
      },
    });
  }

  /**
   * Update an existing Event
   */
  protected async updateEvent(
    eventId: string,
    updates: {
      progress?: number;
      contentMarkdown?: string;
      statementIds?: string[];
    }
  ) {
    return this.prisma.event.update({
      where: { id: eventId },
      data: updates,
    });
  }
}
```

---

### 5.4 Ptolemy Agent

Ptolemyは3つのフェーズを循環するAgent:
1. **Plan作成**
2. **Survey作成**
3. **SurveyAnalysis作成**

```typescript
// agent-system/src/agents/ptolemy-agent.ts

import { BaseAgent } from './base-agent';
import { PrismaClient } from '@prisma/client';
import { logger } from '../utils/logger';

export class PtolemyAgent extends BaseAgent {
  agentId = 'ptolemy';

  async execute(sessionId: string): Promise<void> {
    logger.info(`PtolemyAgent: Executing for session ${sessionId}`);

    const state = await this.getAgentState(sessionId);
    if (!state) {
      logger.error(`PtolemyAgent: AgentState not found for session ${sessionId}`);
      return;
    }

    const currentPhase = state.currentPhase;

    // Determine next action based on currentPhase
    switch (currentPhase) {
      case 'idle':
        await this.startPlanPhase(sessionId);
        break;
      case 'plan':
        await this.continuePlanPhase(sessionId);
        break;
      case 'survey':
        await this.continueSurveyPhase(sessionId);
        break;
      case 'analysis':
        await this.continueAnalysisPhase(sessionId);
        break;
      default:
        logger.warn(`PtolemyAgent: Unknown phase ${currentPhase}`);
    }
  }

  /**
   * Start Plan Phase
   */
  private async startPlanPhase(sessionId: string) {
    logger.info(`PtolemyAgent: Starting Plan phase for session ${sessionId}`);

    // Create Plan Event with progress=0
    const event = await this.createEvent(sessionId, {
      eventType: 'PLAN',
      progress: 0,
      contentMarkdown: '',
    });

    // Update agent state
    await this.updateAgentState(sessionId, {
      currentPhase: 'plan',
      metadata: { currentEventId: event.id },
    });

    // Start generating plan (async simulation)
    await this.generatePlan(sessionId, event.id);
  }

  /**
   * Continue Plan Phase (check if completed)
   */
  private async continuePlanPhase(sessionId: string) {
    const state = await this.getAgentState(sessionId);
    const eventId = state?.metadata?.currentEventId as string | undefined;

    if (!eventId) {
      logger.warn(`PtolemyAgent: No currentEventId in metadata, skipping`);
      return;
    }

    const event = await this.prisma.event.findUnique({ where: { id: eventId } });
    if (!event) return;

    // If plan is complete (progress=1) and shouldProceed=true, move to survey
    if (event.progress >= 1.0) {
      const shouldProceed = await this.shouldContinue(sessionId);
      if (shouldProceed) {
        await this.startSurveyPhase(sessionId);
      } else {
        logger.info(`PtolemyAgent: Plan complete but shouldProceed=false, waiting`);
      }
    }
  }

  /**
   * Generate Plan (Dummy implementation)
   */
  private async generatePlan(sessionId: string, eventId: string) {
    // Simulate async work
    await this.sleep(2000);

    const session = await this.prisma.session.findUnique({ where: { id: sessionId } });
    const dummyPlan = `# Plan (DUMMY)

## 調査の目的
${session?.context}

## 実施項目
1. 初期Surveyの作成（10問のStatement）
2. 参加者から回答を収集
3. 回答データの分析
4. 分析結果に基づく追加Surveyの計画

## 想定される成果物
- 参加者の認識マップ
- 合意形成領域の特定
- 意見対立ポイントの可視化

---
*このPlanはダミーです。実際の実装ではLLMを使用して生成します。*
`;

    // Update event with completed plan
    await this.updateEvent(eventId, {
      progress: 1.0,
      contentMarkdown: dummyPlan,
    });

    logger.info(`PtolemyAgent: Plan generated for session ${sessionId}`);
  }

  /**
   * Start Survey Phase
   */
  private async startSurveyPhase(sessionId: string) {
    logger.info(`PtolemyAgent: Starting Survey phase for session ${sessionId}`);

    // Create Survey Event with progress=0
    const event = await this.createEvent(sessionId, {
      eventType: 'SURVEY',
      progress: 0,
      statementIds: [],
    });

    // Update agent state
    await this.updateAgentState(sessionId, {
      currentPhase: 'survey',
      metadata: { currentEventId: event.id },
    });

    // Generate survey statements
    await this.generateSurvey(sessionId, event.id);
  }

  /**
   * Continue Survey Phase
   */
  private async continueSurveyPhase(sessionId: string) {
    const state = await this.getAgentState(sessionId);
    const eventId = state?.metadata?.currentEventId as string | undefined;
    if (!eventId) return;

    const event = await this.prisma.event.findUnique({ where: { id: eventId } });
    if (!event) return;

    // Check response rate
    if (event.progress >= 0.5 && event.progress < 1.0) {
      const responseRate = await this.calculateResponseRate(sessionId, event.statementIds);

      if (responseRate >= 0.8) {
        // Update to progress=1.0
        await this.updateEvent(eventId, { progress: 1.0 });
        logger.info(`PtolemyAgent: Survey response target reached (${responseRate * 100}%)`);
      }
    }

    // If survey is complete and shouldProceed=true, move to analysis
    if (event.progress >= 1.0) {
      const shouldProceed = await this.shouldContinue(sessionId);
      if (shouldProceed) {
        await this.startAnalysisPhase(sessionId, event.statementIds);
      } else {
        logger.info(`PtolemyAgent: Survey complete but shouldProceed=false, waiting`);
      }
    }
  }

  /**
   * Generate Survey (Dummy implementation)
   */
  private async generateSurvey(sessionId: string, eventId: string) {
    await this.sleep(2000);

    const session = await this.prisma.session.findUnique({ where: { id: sessionId } });

    // Create 10 dummy statements
    const statementTexts = [
      '[DUMMY] このテーマについて十分に理解している',
      '[DUMMY] 現状の方向性に満足している',
      '[DUMMY] チーム内のコミュニケーションは良好だ',
      '[DUMMY] リソースは適切に配分されている',
      '[DUMMY] 意思決定プロセスは透明性がある',
      '[DUMMY] 今後の計画は明確である',
      '[DUMMY] ステークホルダーとの関係は良好だ',
      '[DUMMY] 現在の優先順位は適切である',
      '[DUMMY] リスク管理は十分に行われている',
      '[DUMMY] 改善の余地がある分野を特定できている',
    ];

    const statementIds: string[] = [];

    for (const text of statementTexts) {
      const statement = await this.prisma.statement.create({
        data: {
          sessionId,
          text,
          orderIndex: 0, // Will be updated by orderIndex logic if needed
        },
      });
      statementIds.push(statement.id);
    }

    // Update event: progress=0.5, statementIds set
    await this.updateEvent(eventId, {
      progress: 0.5,
      statementIds,
    });

    logger.info(`PtolemyAgent: Survey generated for session ${sessionId}, ${statementIds.length} statements`);
  }

  /**
   * Calculate response rate for statements
   */
  private async calculateResponseRate(sessionId: string, statementIds: string[]): Promise<number> {
    if (statementIds.length === 0) return 0;

    // Get participant count
    const participantCount = await this.prisma.participant.count({ where: { sessionId } });
    if (participantCount === 0) return 0;

    // Get response count per statement
    const responseCounts = await Promise.all(
      statementIds.map(id =>
        this.prisma.response.count({ where: { statementId: id, sessionId } })
      )
    );

    // Average response count
    const avgResponseCount = responseCounts.reduce((sum, count) => sum + count, 0) / statementIds.length;
    return avgResponseCount / participantCount;
  }

  /**
   * Start Analysis Phase
   */
  private async startAnalysisPhase(sessionId: string, statementIds: string[]) {
    logger.info(`PtolemyAgent: Starting Analysis phase for session ${sessionId}`);

    // Create SurveyAnalysis Event with progress=0
    const event = await this.createEvent(sessionId, {
      eventType: 'SURVEY_ANALYSIS',
      progress: 0,
      contentMarkdown: '',
      statementIds,
    });

    // Update agent state
    await this.updateAgentState(sessionId, {
      currentPhase: 'analysis',
      metadata: { currentEventId: event.id },
    });

    // Generate analysis
    await this.generateAnalysis(sessionId, event.id, statementIds);
  }

  /**
   * Continue Analysis Phase
   */
  private async continueAnalysisPhase(sessionId: string) {
    const state = await this.getAgentState(sessionId);
    const eventId = state?.metadata?.currentEventId as string | undefined;
    if (!eventId) return;

    const event = await this.prisma.event.findUnique({ where: { id: eventId } });
    if (!event) return;

    // If analysis is complete and shouldProceed=true, loop back to plan
    if (event.progress >= 1.0) {
      const shouldProceed = await this.shouldContinue(sessionId);
      if (shouldProceed) {
        // Loop back to idle (will start plan again in next cycle)
        await this.updateAgentState(sessionId, {
          currentPhase: 'idle',
          metadata: {},
        });
        logger.info(`PtolemyAgent: Analysis complete, looping back to idle`);
      } else {
        logger.info(`PtolemyAgent: Analysis complete but shouldProceed=false, waiting`);
      }
    }
  }

  /**
   * Generate Analysis (Dummy implementation)
   */
  private async generateAnalysis(sessionId: string, eventId: string, statementIds: string[]) {
    await this.sleep(2000);

    const dummyAnalysis = `# Survey Analysis (DUMMY)

## 回答概要
- 調査対象: ${statementIds.length}件のStatement
- 回答状況: 80%以上の参加者から回答を取得

## 主な発見
### 合意が形成されている点
- [DUMMY] ポイント1: チーム内のコミュニケーション改善の必要性
- [DUMMY] ポイント2: リソース配分の見直し

### 意見が対立している点
- [DUMMY] ポイント3: 優先順位の設定方法
- [DUMMY] ポイント4: 意思決定プロセスの透明性

### 分かっていない点
- [DUMMY] ポイント5: ステークホルダーの具体的な期待値
- [DUMMY] ポイント6: リスク管理の具体的な手法

## 次のアクション提案
1. 対立している点についてさらに深掘りする追加Survey
2. 合意点に基づいたアクションプランの策定
3. 不明点を明確にするための情報収集

---
*この分析はダミーです。実際の実装ではLLMを使用して生成します。*
`;

    // Update event with completed analysis
    await this.updateEvent(eventId, {
      progress: 1.0,
      contentMarkdown: dummyAnalysis,
    });

    logger.info(`PtolemyAgent: Analysis generated for session ${sessionId}`);
  }

  /**
   * Sleep utility
   */
  private sleep(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}
```

**補足:**
- **Phase遷移:** idle → plan → survey → analysis → idle（ループ）
- **shouldProceed確認:** 各フェーズ完了時に確認し、falseなら待機
- **progress段階:**
  - Plan: 0 → 1.0
  - Survey: 0 → 0.5（Statement生成完了） → 1.0（80%回答達成）
  - Analysis: 0 → 1.0
- **Dummy実装:** 実際のLLM呼び出しは後で実装

---

### 5.5 Agent Registry

```typescript
// agent-system/src/agents/index.ts

import { PrismaClient } from '@prisma/client';
import { BaseAgent } from './base-agent';
import { PtolemyAgent } from './ptolemy-agent';

const agentRegistry: Record<string, (prisma: PrismaClient) => BaseAgent> = {
  ptolemy: (prisma) => new PtolemyAgent(prisma),
  // Add future agents here
};

export function getAgent(agentId: string): BaseAgent | null {
  const prisma = new PrismaClient();
  const factory = agentRegistry[agentId];
  return factory ? factory(prisma) : null;
}

export function listAgents(): string[] {
  return Object.keys(agentRegistry);
}
```

---

## 6. 実装フェーズ

### Phase 1: 基礎実装（v0.1）

**目標:** 基本的なEvent Thread & Agent Systemを動作させる

**タスク:**
1. ✅ データベーススキーマ追加（EventThread, Event, AgentState）
2. ✅ Prisma migration実行
3. ✅ Backend API実装
   - GET `/api/sessions/[sessionId]/event-thread`
   - POST `/api/sessions/[sessionId]/events`
   - PATCH `/api/sessions/[sessionId]/should-proceed`
4. ✅ セッション作成時にEventThread自動作成
5. ✅ Agent System構築
   - AgentOrchestrator (polling mode)
   - BaseAgent abstract class
   - PtolemyAgent (dummy LLM)
6. ✅ Frontend: EventThread表示画面
   - Event一覧表示（collapse/expand）
   - UserMessage送信フォーム
   - shouldProceed制御ボタン

**成果物:**
- 動作するEvent Thread & Agent System
- Ptolemy AgentがPlan → Survey → Analysisを循環
- 管理画面でEventを確認できる

---

### Phase 2: UI/UX改善 & リアルタイム化（v0.2）

**タスク:**
1. Event表示のデザイン改善
   - Color coding
   - Progress bar のアニメーション
   - Markdown rendering の最適化
2. リアルタイム更新
   - Server-Sent Events (SSE) 実装
   - Agentの進捗をリアルタイム表示
3. LISTEN/NOTIFY for shouldProceed
   - PostgreSQL LISTEN/NOTIFYでOrchestrator効率化

---

### Phase 3: LLM統合 & 高度な機能（v0.3）

**タスク:**
1. PtolemyAgentにLLM統合
   - `generatePlan()` with LLM
   - `generateSurvey()` with LLM
   - `generateAnalysis()` with LLM
2. エラーハンドリング & リトライロジック
3. Agent追加のためのインフラ整備
   - Agent設定ファイル
   - Dynamic agent loading

---

## 7. 技術スタック

| Layer | Technology |
|-------|-----------|
| Frontend | Next.js 14, React, TypeScript, TailwindCSS |
| Backend API | Next.js API Routes, Prisma |
| Agent System | Node.js, TypeScript, Prisma |
| Database | PostgreSQL |
| Pub/Sub (Phase 2) | PostgreSQL LISTEN/NOTIFY |
| LLM (Phase 3) | OpenRouter API (Gemini 2.5 Pro) |
| Monitoring | Winston (logger), PM2 (process manager) |

---

## 8. セキュリティ & パフォーマンス考慮事項

### 8.1 セキュリティ

- **認証:** 既存のユーザーID認証を継承
- **認可:** EventThreadへのアクセスはセッションホストのみ
- **Agent System:** DBへの直接アクセスは読み取り専用ユーザーを推奨（Phase 2）
- **LLM API Key:** 環境変数で管理、絶対にフロントエンドに公開しない

### 8.2 パフォーマンス

- **EventThread取得:** `events`のeager loadingで1クエリで取得
- **Orchestrator polling:** 5秒間隔（調整可能）
- **Agent並列実行:** 同一セッションでは排他制御（`isProcessing` flag）
- **DB connection pooling:** PrismaのデフォルトPoolを使用

### 8.3 エラーハンドリング

- **Agent処理失敗:** Event progress は更新せず、次回リトライ
- **LLM API失敗:** 3回リトライ、失敗時はダミーデータまたはエラーEvent
- **DB接続失敗:** Orchestratorは自動再接続（Prisma機能）

---

## 9. 移行戦略

### 9.1 既存機能との共存

Phase 1では既存の機能を残しつつ、新システムを並行稼働:

- **既存のボタン:** 一旦残す（廃止予定として灰色表示）
- **EventThread:** 新規セッションのみ有効化
- **既存セッション:** EventThreadがない場合は従来通り動作

### 9.2 段階的移行

1. **Week 1-2:** Phase 1実装、新規セッションで有効化
2. **Week 3:** ユーザーフィードバック収集、UI改善
3. **Week 4:** Phase 2実装（リアルタイム化）
4. **Week 5-6:** Phase 3実装（LLM統合）
5. **Week 7:** 既存機能の完全廃止、全セッションでEventThread有効化

---

## 10. テスト戦略

### 10.1 Unit Tests

- **Agent Logic:** BaseAgent, PtolemyAgentのメソッド単体テスト
- **API Routes:** EventThread, Event CRUDのテスト
- **Event Service:** Event作成・更新ロジックのテスト

### 10.2 Integration Tests

- **Agent Execution:** OrchestatorからAgent実行までのフロー
- **API + DB:** Prismaを使ったDB操作のテスト

### 10.3 E2E Tests

- **フロントエンド:** Playwrightでセッション作成 → EventThread表示 → UserMessage送信
- **Agent System:** 実際のDB環境でOrchestratorを起動し、Plan→Survey→Analysisの循環を確認

---

## 11. モニタリング & ロギング

### 11.1 ログレベル

- `DEBUG`: 詳細な処理ログ（開発環境のみ）
- `INFO`: 重要なイベント（Agent実行開始/完了、Event作成）
- `WARN`: 想定外だが致命的でない状況（shouldProceed=false時の待機）
- `ERROR`: エラー発生（LLM API失敗、DB接続失敗）

### 11.2 メトリクス（Phase 2+）

- Agent実行時間
- Event作成数/更新数
- LLM API呼び出し回数・レイテンシ
- Orchestrator polling interval実績

### 11.3 アラート

- Agent処理が10分以上停止
- LLM API失敗率が50%超
- DB接続失敗

---

## 12. ドキュメント

### 12.1 開発者向け

- `docs/ARCHITECTURE.md`: システムアーキテクチャ概要
- `docs/AGENT_DEVELOPMENT.md`: 新しいAgentの追加方法
- `docs/API_REFERENCE.md`: API仕様書

### 12.2 運用者向け

- `docs/DEPLOYMENT.md`: デプロイ手順
- `docs/MONITORING.md`: モニタリング & トラブルシューティング

---

## 13. 今後の拡張可能性

### 13.1 複数Agentの同時稼働

- Session単位で複数のAgentが協調動作
- Agent間のメッセージング（Agent-to-Agent Event）

### 13.2 Agent Marketplace

- ユーザーがカスタムAgentを追加できる仕組み
- Agent実行のサンドボックス化

### 13.3 高度なWorkflow

- Conditional branching（条件分岐）
- Parallel execution（並列実行）
- Human-in-the-loop（ユーザー承認待ち）

---

## まとめ

本設計書では、Event ThreadとAgent Systemを使った新しいアーキテクチャを提案しました。

**主な特徴:**
- ✅ **イベント駆動:** Event Threadにすべての履歴が記録される透明性
- ✅ **Modular Agent System:** Agentの追加・削除が容易
- ✅ **shouldProceedによる制御:** ユーザーがAgentの動作を制御可能
- ✅ **Phase-based implementation:** 段階的に機能を追加し、リスクを最小化

次のステップとして、Phase 1の実装を開始します。
