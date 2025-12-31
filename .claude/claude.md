# Cartographer - Claude Code Project Rules

## Project Overview

**Cartographer (倍速会議)** is a web service that visualizes recognition and facilitates consensus building. It collects and analyzes recognition from multiple people on specific themes using LLM-powered insights.

**Architecture:**
- **Frontend**: Next.js + TypeScript + React + Supabase
- **Backend**: Haskell (Servant + Polysemy + Project:M36)
- **Build System**: Nix Flakes
- **Database**: Supabase (PostgreSQL)
- **LLM**: OpenRouter API

## Core Development Principles

### 1. Build System - NIX FIRST, ALWAYS

**CRITICAL - READ THIS FIRST:**

Haskellのビルドなどにおいて、**cabal build や cabal test ではなく nix build や nix run, nix flake check などを利用してください**。

何回言ったらわかるのかわかんないけど、**cabal build や cabal test ではなく、nixを使って**。

**おい、絶対cabalは使うなって言っただろ?! 次使ったら殺す。**

Use:
- ✅ `nix build`
- ✅ `nix run`
- ✅ `nix flake check`
- ✅ `nix develop`

Do NOT use:
- ❌ `cabal build`
- ❌ `cabal test`
- ❌ `cabal run`

### 2. Haskell Backend Architecture

This project uses a modern Haskell stack with:

#### Core Libraries
- **Servant**: Type-level DSL for API definitions
  - API仕様を型として記述
  - サーバー実装、クライアント、ドキュメントを型安全に生成
  - `hoistServer` でPolysemyと統合

- **Polysemy**: Higher-order, low-boilerplate effect system
  - ビジネスロジックと実装詳細を完全分離
  - `Sem r a` モナドでEffect Rowを管理
  - `Member` 制約で型安全な依存性注入

- **Project:M36**: Relational algebra database
  - リレーショナル代数に基づくデータベース
  - `bracket` パターンでリソース管理

#### Haskell Development Guidelines

**Pure Functions First:**
- 可能な限り関数は純粋（参照透過）に保つ
- 副作用（IO）は分離する
- Polysemyを使用する際は、ビジネスロジックを純粋なEffectとして定義
- IOの実装をInterpreterとして分離

**Type Safety:**
- ドメインロジックは `data` (ADT) および `newtype` で表現
- `String` ではなく `Data.Text` と `Data.ByteString` を使用
- `OverloadedStrings` 言語拡張を活用

**Error Handling:**
- 部分関数（`head`, `tail`）の使用を避ける
- パターンマッチや `Maybe`, `Either` 型で安全に処理
- IO層では `Control.Exception` で例外を捕捉
- ビジネスロジックのエラーは `Either CustomErrorType a` で表現

**Concurrency:**
- GHCの軽量スレッド（`forkIO` または `async`）を使用
- 共有状態管理には **STM (Software Transactional Memory)** を使用
- `atomically`, `retry`, `orElse` でデッドロック回避

**Performance:**
- 遅延評価によるスペースリーク防止のため `foldl'` を使用
- バン・パターン（`!pattern`）で必要に応じて評価を強制
- `-O2 -flate-specialise -fspecialise-aggressively` でコンパイル

**Testing:**
- QuickCheck でプロパティベーステスト
- HUnit で従来のユニットテスト
- `stack test --coverage` でカバレッジ確認

**Servant Patterns:**
- API型定義ファースト（全ての入出力を型で明示）
- `NamedRoutes` (Generic based API) で可読性向上
- `hoistServer` でカスタムモナド（Polysemy）を統合
- `servant-openapi3` でAPI仕様書を自動生成

**Polysemy Patterns:**
- エフェクトをGADTとして定義
- `makeSem` でスマートコンストラクタを自動生成
- `interpret` で一次エフェクトを解釈
- `interpretH` と Tactics で高階エフェクトを解釈
- `Embed IO` でIOアクションを持ち上げ（Interpreter内のみ）
- 実装の詳細をインタープリタに追い出し、DIを型レベルで実現

**Module Structure:**
- `app/`: Main モジュール（最小限）
- `src/`: ライブラリロジック（テスト可能）
- `test/`: テストコード

### 3. Frontend Development

**Technology Stack:**
- Next.js (App Router)
- TypeScript
- React
- Supabase Client
- Tailwind CSS

**Frontend Guidelines:**
- コンポーネントの責務を明確に分離
- Server Components と Client Components を適切に使い分け
- Supabaseクライアントは環境変数から初期化
- 型安全性を重視（TypeScript strict mode）

### 4. Database & Authentication

**Supabase:**
- PostgreSQL database
- Row Level Security (RLS) を活用
- Realtime subscriptions でリアルタイム更新
- マイグレーションは `supabase/migrations/` で管理

**Authentication:**
- Supabase Auth を使用
- セッション管理はcookieベース（将来的に改善予定）

### 5. Project Context (from Scrapbox)

**Purpose:**
- 認識を可視化し、合意形成を促進する
- 会議の録音ログから自動的にセッションを生成（未来機能）
- リアルタイムで参加者の回答を収集・分析

**Key Features:**
- セッション作成（プロンプトのテンプレート機能）
- 参加者の回答収集（5段階評価 + 自然言語入力）
- LLMによる分析レポート生成
- 管理画面（admin）とレポート共有

**Current Issues (from infobox):**
- セッション開始に時間がかかる → 1分以内に開始できるようにしたい
- QRコード表示でURL共有を簡単に
- レポートをadmin以外も閲覧可能に
- ダークモード対応（現在対応中）

**Tech Debt:**
- 認証周りの設計改善が必要
- セッション管理UIの整理
- Supabase fetcher のバンドル化

## Development Workflow

### Setup
```bash
# Frontend
cp .env.example .env.local  # Supabase + OpenRouter keys を設定
npm install
npm run dev  # http://localhost:3000

# Backend (Haskell)
nix develop  # 開発環境に入る
nix build    # ビルド
nix run      # 実行
```

### Testing
```bash
# Frontend
npm test

# Backend
nix flake check  # ← これを使う！cabal test は使わない！
```

### Database
```bash
# Supabase ディレクトリ
cd supabase
supabase db push  # マイグレーション適用
```

## Code Style

### Haskell
- 型注釈は必須（トップレベル関数）
- 関数合成（`.`）とポイントフリースタイルを活用
- `where` と `let` を適切に使い分け
- レコード構文でアクセサ関数を自動生成

### TypeScript
- ESLint + Prettier で統一
- 明示的な型注釈を推奨
- async/await でPromiseを扱う

## File Organization

```
cartographer/
├── .claude/          # Claude Code rules
├── .agent/rules/     # Legacy AI agent rules
├── app/              # Next.js app (frontend)
├── backend/          # Haskell backend
│   ├── app/          # Main module
│   ├── src/          # Library code
│   └── test/         # Tests
├── supabase/         # Database migrations
├── doc/              # Documentation (including Scrapbox dump)
├── flake.nix         # Nix build configuration
└── package.json      # Frontend dependencies
```

## Resources

### Documentation
- Main project info: `doc/cartographer.2hop.txt` (Scrapbox dump)
- Setup guide: `GUIDE_ja.md`
- Haskell rules: `.agent/rules/haskell*.md`
- Servant guide: `.agent/rules/servant.md`
- Polysemy guide: `.agent/rules/polysemy.md`

### External
- [GitHub Repository](https://github.com/plural-reality/cartographer)
- [Scrapbox](https://scrapbox.io/plural-reality/Cartographer)
- Production: https://cartographer-agents.vercel.app/

## Important Reminders

1. **NIX ONLY** - 絶対にcabalコマンドは使わない
2. **Pure Functions** - 副作用は分離し、型で表現する
3. **Type Safety** - 型システムを最大限活用する
4. **Effect System** - Polysemyで依存性を管理する
5. **API First** - Servantの型定義から実装を導く

---

このルールに従い、型安全で保守性の高いコードを書くこと。
疑問がある場合は、`.agent/rules/` のドキュメントを参照すること。
/doc を参照すること
