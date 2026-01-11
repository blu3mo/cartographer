# Cartographer デプロイメントガイド

このガイドでは、以下の3つのレイヤーに分けてデプロイ手順を解説します。

1.  **Workplace Management (管理者向け)**: Terraform Cloud ワークスペース自体の設定・権限管理
2.  **Infrastructure Management (開発者向け)**: AWS/Cloudflare リソースの構成管理
3.  **Application Deployment (開発者向け)**: アプリケーションコードのデプロイ

---

## 1. Workspace Management (管理者向け)

**対象ディレクトリ:** `infra/tfc-bootstrap`

Terraform Cloud (TFC) のワークスペース設定、環境変数、チームメンバー招待をコード (`tfc-bootstrap`) で管理します。**通常、開発者がここを触る必要はありません。**

### 初回セットアップ / 設定変更

```bash
cd infra/tfc-bootstrap
cp terraform.tfvars.example terraform.tfvars
# terraform.tfvars に管理者用設定（Cloudflareトークンなど）を記述

terraform init
terraform apply
```

### 管理できる項目
- **AWS認証情報**: 手順内で専用IAMユーザーを作成・設定済み
- **Cloudflareトークン**: TFCの環境変数として注入
- **チームメンバー**: `terraform.tfvars` の `team_members` にメールアドレスを追加して apply すると招待が送信されます

---

## 2. Infrastructure Management (開発者向け)

**対象ディレクトリ:** `infra/terraform`

AWS (EC2, EFS, VPC) や Cloudflare (DNS, SSL) のリソースを変更する場合の手順です。
state は Terraform Cloud で共有されているため、チーム全員が常に同じ最新状態を参照できます。

### 準備

TFC にログインします（初回のみ）。

```bash
cd infra/terraform
terraform login
```

### インフラの変更・適用

`terraform.tfvars` は不要です（TFC上の変数が自動的に使われます）。

```bash
terraform init
terraform plan   # 変更内容の確認
terraform apply  # 変更の適用
```

### 主な管理リソース
- **EC2**: インスタンスタイプ、AMI
- **Networking**: VPC, Subnet, Security Groups
- **Storage**: EFS Settings
- **DNS/SSL**: Cloudflare Records, Origin Certificates

---

## 3. Application Deployment (開発者向け)

**対象ツール:** Colmena (Nix)

インフラ（EC2）が立ち上がった後、実際にアプリケーションをデプロイする手順です。

### 準備: 環境変数

プロジェクトルートに `.env.production` を作成し、アプリケーション用シークレットを記述します。

```bash
# .env.production
DATABASE_URL=postgresql://postgres:PASSWORD@db.xxx.supabase.co:5432/postgres?sslmode=require
OPENROUTER_API_KEY=sk-or-xxx
```
> **Note**: `NEXT_PUBLIC_` 変数は `flake.nix` に記述してください（ビルド時に埋め込まれます）。

### デプロイ実行

```bash
# --impure フラグは $PWD（秘密情報のパス解決）を参照するために必要です
nix run github:zhaofengli/colmena -- apply --on cartographer-prod --impure
```

Colmena が自動的に以下を行います:
- ローカル (or リモートビルダ) で NixOS システムをビルド
- EC2 に SSH 接続して構成を適用 (Switch)
- systemd サービスの再起動 (Backend, Frontend, Nginx)

---

## 4. 秘匿情報の管理とロードマップ

現在、アプリケーションの環境変数（`.env`）や SSL 証明書は、デプロイを実行するマシンのローカルファイルとして管理され、Colmena の `deployment.keys` 機能を通じてターゲットサーバーに転送されています。

### 現状と短期的な対策
- **現状**: `flake.nix` にデプロイ実行者の環境に依存した絶対パスが含まれる可能性があります。
- **対策**: `flake.nix` 内では、環境変数（`$PWD`）を利用してパスを動的に組み立てるようにし、特定の環境（例: `/Volumes/...`）への依存を排除します。
- **なぜ Nix Store に直接含めないのか？**: 
    - **セキュリティ**: NixOS のパス型（例: `keyFile = ./.env.production`）として直接記述すると、そのファイルは Nix Store (`/nix/store`) にコピーされます。Nix Store はシステム上の全ユーザーから読み取り可能であるため、秘密鍵や API キーが漏洩するリスクがあります。
    - **ポータビリティ**: 環境変数 + `--impure` オプションを使用することで、秘密情報を Nix Store に残さず、実行時のコンテキストとして安全にターゲットに転送できます。

### 中長期的なロードマップ
リポジトリの安全性とポータビリティをさらに向上させるため、以下の移行を計画しています。

1.  **フェーズ1: 秘匿情報の暗号化 (agenix / sops-nix)**
    - `.env` ファイルや証明書を `age` や `pgp` で暗号化した状態でリポジトリにコミットします。
    - サーバーの SSH 鍵を用いてデプロイ時に動的に復号する仕組みを導入します。
    - これにより、リポジトリをクローンした開発者が（権限があれば）即座にデプロイ可能な状態になります。

2.  **フェーズ2: 外部シークレットマネージャーの活用 (AWS Secrets Manager)**
    - 本番環境については、AWS Secrets Manager 等のマネージドサービスでの管理に移行します。
    - アプリケーション起動時に AWS SDK を通じてシークレットを取得し、ディスクへの書き出しを最小限に抑えます。

---

## その他

### トラブルシューティング
- [troubleshooting.md](./troubleshooting.md)

### 既存リソースの取り込み (Import)
TFC導入前に作られたリソースを管理下に置く場合のみ必要です。

```bash
cd infra/terraform
terraform import aws_vpc.main vpc-xxxxxxxx
```
