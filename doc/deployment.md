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

## 5. 信頼性と設計の意図 (Architecture & Reliability)

システム全体の安定稼働のため、特に起動順序とファイル権限の管理において以下の設計を採用しています。

### EFS 権限の動的解決 (`ExecStartPre`)
バックエンドサービス (`cartographer-backend`) では、起動の直前に以下の処理を行っています。

```nix
serviceConfig.ExecStartPre = "+${pkgs.coreutils}/bin/chown cartographer:cartographer /mnt/efs";
```

#### なぜこれが必要なのか？
1.  **マウント時のリセット**: EFS (NFSv4) が Linux にマウントされる際、マウントポイントの所有権が OS によって `root:root` に上書きされる場合があります。
2.  **依存関係の限界**: Systemd の `after` や `requires` は「いつマウントするか」を制御できますが、「マウントされたディレクトリの所有権が正しいか」までは保証できません。
3.  **起動エラーの回避**: Haskell バックエンドは `cartographer` ユーザーで動作するため、マウントポイントが `root` 所有のままだと、自身のデータベースディレクトリ (`m36-data`) の作成やアクセスに失敗してクラッシュします。

#### 他の手段との比較
- **`tmpfiles.d`**: システム起動時に実行されますが、EFS のマウント完了前にローカルディスク側にディレクトリを作ってしまう競合（Race Condition）のリスクがあります。
- **`ExecStartPre` (採用)**: バイナリが動く **コンマ数秒前** に、root 権限（`+` プレフィックス）で所有権を「矯正」します。これにより、マウントの状態にかかわらず、バックエンドが常に書き込み可能な状態で起動することを保証しています。

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
