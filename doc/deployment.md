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
nix run github:zhaofengli/colmena -- apply --on cartographer-prod
```

Colmena が自動的に以下を行います:
- ローカル (or リモートビルダ) で NixOS システムをビルド
- EC2 に SSH 接続して構成を適用 (Switch)
- systemd サービスの再起動 (Backend, Frontend, Nginx)

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
