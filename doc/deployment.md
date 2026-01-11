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

## 3. インフラ設定の自動同期 (Infrastructure Sync)

`flake.nix` は、Terraform が出力する JSON ファイル (`infra/terraform/infra-*.json`) を動的に読み込みます。これにより、IP アドレスや EFS ID を手動で修正する必要がなくなります。

### 同期の仕組み
1.  **Terraform**: `terraform apply` 実行時に、機密情報（秘密鍵）を含まない設定 JSON (`infra-*.json`) が自動生成されます。
2.  **Git**: 生成された JSON をコミットすることで、インフラの状態（IP や EFS ID）をリポジトリ内で共有できます。
3.  **Nix**: `colmena apply` 実行時に、`builtins.fromJSON` を使って上記のファイルを読み込みます。

> [!TIP]
> インフラに変更がない場合は、JSON の再生成は不要です。インフラを再構築した場合は、以下のコマンドで最新の状態を同期してください。
> ```bash
> # 例: staging の出力を手動同期する場合
> TF_WORKSPACE=staging terraform output -json infra_metadata > infra-cartographer-staging.json
> ```

---

## 4. デプロイの実行 (Deployment) (開発者向け)

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

# Staging 環境へのデプロイ
colmena apply --on cartographer-staging --impure

# Production 環境へのデプロイ
colmena apply --on cartographer-prod --impure
```

Colmena が自動的に以下を行います:
- ローカル (or リモートビルダ) で NixOS システムをビルド
- EC2 に SSH 接続して構成を適用 (Switch)
- systemd サービスの再起動 (Backend, Frontend, Nginx)

---


#### EFS のマウントと権限管理
バックエンドサービスが正しい権限で起動することを保証するため、`application.nix` 内で `ExecStartPre` による所有権の自動修復を行っています。

より詳細な背景や、将来的な改善計画については [Infrastructure Roadmap](./infrastructure-roadmap.md) を参照してください。

---

## 5. その他

### トラブルシューティング
- [troubleshooting.md](./troubleshooting.md)

### 既存リソースの取り込み (Import)
TFC導入前に作られたリソースを管理下に置く場合のみ必要です。

```bash
cd infra/terraform
terraform import aws_vpc.main vpc-xxxxxxxx
```
