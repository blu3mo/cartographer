# Cartographer Docker Setup Guide

このガイドでは、Cartographerを外部サービスに依存せずにローカル環境で実行する方法を説明します。

## 概要

Docker Compose環境では以下のコンポーネントが含まれます：

- **PostgreSQL Database**: ローカルのPostgreSQLデータベース
- **Supabase Stack**: Kong API Gateway、PostgREST、Realtime、Meta、Studio
- **Web Application**: Next.jsアプリケーション
- **Agent System**: Ptolemyエージェント

## 前提条件

- Docker Engine 20.10以上
- Docker Compose 2.0以上
- 8GB以上のRAM推奨

## セットアップ手順

### 1. 環境変数の設定

`.env.docker.example`をコピーして`.env.docker`を作成します：

```bash
cp .env.docker.example .env.docker
```

`.env.docker`を編集して、以下の設定を行います：

#### LLMプロバイダーの選択

**オプション1: OpenRouter（デフォルト）**

```env
OPENROUTER_API_KEY=your_openrouter_api_key_here
USE_VERTEX_AI=false
```

**オプション2: Google Vertex AI**

```env
USE_VERTEX_AI=true
GOOGLE_CLOUD_PROJECT=your-gcp-project-id
GOOGLE_APPLICATION_CREDENTIALS=/app/credentials/vertex-ai-key.json
GOOGLE_APPLICATION_CREDENTIALS_PATH=/path/to/your/vertex-ai-key.json
```

Vertex AIを使用する場合は、Google Cloud Platformでサービスアカウントを作成し、JSONキーファイルをダウンロードしてください。

### 2. Docker Composeの起動

すべてのサービスを起動します：

```bash
docker-compose --env-file .env.docker up -d
```

初回起動時は、イメージのビルドとダウンロードに時間がかかります。

### 3. サービスの確認

以下のURLでサービスにアクセスできます：

- **Webアプリケーション**: http://localhost:3000
- **Supabase Studio**: http://localhost:3001
- **Supabase API**: http://localhost:8000

### 4. データベースの初期化

データベースは自動的に初期化されます（`supabase/schema.sql`が適用されます）。

Supabase Studioでデータベースの状態を確認できます：
http://localhost:3001

## 使用方法

### サービスの起動

```bash
docker-compose --env-file .env.docker up -d
```

### サービスの停止

```bash
docker-compose down
```

### ログの確認

全サービスのログ：
```bash
docker-compose logs -f
```

特定のサービスのログ：
```bash
docker-compose logs -f web
docker-compose logs -f agent
docker-compose logs -f db
```

### データベースのリセット

データベースをリセットする場合：

```bash
docker-compose down -v
docker-compose --env-file .env.docker up -d
```

## クラウドサービスとの併用

このDocker環境は、既存のクラウドサービス（Supabase、OpenRouter）との併用も可能です。

### クラウドSupabaseを使用する場合

`.env.docker`で以下を設定：

```env
NEXT_PUBLIC_SUPABASE_URL=https://your-project.supabase.co
ANON_KEY=your_supabase_anon_key
SERVICE_ROLE_KEY=your_supabase_service_role_key
```

そして、docker-compose.ymlから不要なSupabaseサービス（db、kong、rest、realtime、meta、studio）をコメントアウトします。

## トラブルシューティング

### ポートが既に使用されている

他のサービスがポートを使用している場合、docker-compose.ymlのポート設定を変更してください：

```yaml
ports:
  - "3000:3000"  # 左側を変更（例: "3001:3000"）
```

### データベース接続エラー

1. データベースコンテナが起動しているか確認：
   ```bash
   docker-compose ps
   ```

2. データベースログを確認：
   ```bash
   docker-compose logs db
   ```

### Vertex AI認証エラー

1. JSONキーファイルのパスが正しいか確認
2. Google Cloud Projectで必要なAPIが有効になっているか確認：
   - Vertex AI API
   - Cloud AI Platform API

### エージェントが動作しない

1. エージェントログを確認：
   ```bash
   docker-compose logs agent
   ```

2. Realtimeサービスが正常に動作しているか確認：
   ```bash
   docker-compose logs realtime
   ```

## 開発モード

開発中は、ローカルで`npm run dev`と`npm run agent`を実行することをお勧めします。Docker環境は本番環境のテストや、完全に独立した環境でのテストに使用してください。

## 本番環境への展開

本番環境では、以下の点に注意してください：

1. `.env.docker`のシークレット値を安全に管理
2. `JWT_SECRET`を強力なランダム値に変更
3. `POSTGRES_PASSWORD`を強力なパスワードに変更
4. Basic認証やIP制限を有効化
5. HTTPSを使用（リバースプロキシ経由）

## 参考

- [Supabase Self-Hosting Guide](https://supabase.com/docs/guides/self-hosting)
- [Next.js Docker Deployment](https://nextjs.org/docs/deployment#docker-image)
- [Google Vertex AI Documentation](https://cloud.google.com/vertex-ai/docs)
