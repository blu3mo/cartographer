このドキュメントは Nix flake（`flake.nix`）を前提に、ゼロからローカル開発環境を整え、アプリを起動できる状態までの最短手順を示します。

## 0. 前提（初回のみ）
- macOS で Nix をインストール（公式インストーラ推奨）。
- Nix の Flakes を有効化（インストーラの設定 UI で “Flakes” を有効、または `/etc/nix/nix.conf` に `experimental-features = nix-command flakes`）。

## 1. Nix devShell に入る（依存は flake で完結）
`flake.nix` は次のツールを提供します：
- Node.js 20（npm 同梱）
- supabase-cli
- postgresql（`psql` クライアント）
- python3 / pkg-config / vips / openssl（ネイティブ拡張のビルドに必要）
- git / watchman

開発シェルへ入る：
```zsh
nix develop
```

終了するとき：
```zsh
exit
```

## 2. 環境変数ファイルを用意する
`.env.example` をコピーし、必須キーを埋めます。
```zsh
cp .env.example .env.local
```
必須キー（例）:
- `OPENROUTER_API_KEY`
- `NEXT_PUBLIC_SUPABASE_URL`
- `NEXT_PUBLIC_SUPABASE_ANON_KEY`
- `SUPABASE_SERVICE_ROLE_KEY`（サーバー専用。クライアントへ公開しない）

## 3. Supabase スキーマを適用する
`supabase/schema.sql` を、接続先の Postgres に流し込みます。Nix シェルには `psql` が入っています。

1) Supabase ダッシュボードから接続文字列を取得し、環境変数 `DATABASE_URL` をセット（例：SSL 必須）。
```zsh
export DATABASE_URL='postgresql://postgres:YOUR_PASSWORD@db.YOUR_REF.supabase.co:5432/postgres?sslmode=require'
```

2) スキーマ適用：
```zsh
psql "$DATABASE_URL" -v ON_ERROR_STOP=1 -f supabase/schema.sql
```

補足:
- `SUPABASE_DB_URL` を使いたい場合は `export SUPABASE_DB_URL=...` として同様に利用可能です。
- 接続エラー時はパスワード/ホスト/ポート/`sslmode=require` を再確認。

## 4. 依存パッケージのインストールと起動
```zsh
npm install
npm run dev        # http://localhost:3000
```

任意（自動エージェント実行）:
```zsh
npm run agent
```

コード品質:
```zsh
npm run lint       # Biome check
npm run format     # Biome format
```

## 5. 起動後のローカル状態（概要）
- プロセス:
  - Next.js dev server（Turbopack, ポート 3000）
  - バンドルワーカー / ファイル監視
  - （任意）エージェント実行用 Node プロセス
- ネットワーク:
  - ブラウザ → `http://localhost:3000`
  - サーバー/クライアント → Supabase（REST/Realtime/WebSocket）
  - サーバー → OpenRouter（LLM 推論時）
- ストレージ:
  - `node_modules/`, `.next/`（ビルドキャッシュ/生成物）
  - `.env.local`（手動で作成・秘匿）

## 6. トラブルシューティング
- 401/403（Supabase）: キー誤り / RLS ポリシー不整合。`SUPABASE_SERVICE_ROLE_KEY` はクライアントに露出させない。
- 429/401（OpenRouter）: API Key 未設定 / レート制限。キーを再確認し、エージェント頻度を調整。
- `fetch failed`（接続）: `NEXT_PUBLIC_SUPABASE_URL`・ネットワーク・CORS を確認。
- `sharp` 関連ビルド失敗: Node バージョン/`vips`/ヘッダ類の不足。Nix シェル内で実行しているか確認。
- Turbopack の不調: `rm -rf .next` して再起動。

## 7. 補足（ポリシー/セキュリティ）
- `NEXT_PUBLIC_` 接頭辞のみクライアントへ埋め込み可能。
- `SUPABASE_SERVICE_ROLE_KEY` はサーバー側専用。
- `accessToken` を含む URL は共有リンク相当。取り扱い注意。

---

## 8. 並行稼働環境 (Event Sourcing / Postgres 18)
Supabase (5432) とは別に、純粋なイベントソーシング用の Postgres 18 をポート **5433** で起動します。
この環境は `flake.nix` で完結しており、追加のインストールは不要です。

### 起動方法
以下のNixコマンド一発で、DBの初期化(`initdb`)・起動(`postgres`)・**スキーマ適用(`migrate`)の全て**が自動で行われます。

```zsh
# フォアグラウンドで起動
nix run .#db-up
```
または、`nix develop` 済みのシェル内であれば単に：
```zsh
# `devShell` に入っているコマンド
db-up
```

### 接続確認
```zsh
make db-shell
```
で `psql`に入れます。スキーマは起動時に自動適用されているので、すぐに利用可能です。
