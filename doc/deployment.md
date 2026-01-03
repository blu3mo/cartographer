# Cartographer デプロイメントガイド

AWS ARM インスタンス上の NixOS システムへのデプロイ手順です。

通常は 2. 以降のみで良いと思います(AWS自体をいじる必要がない場合。)

## 前提条件

- **Nix** (Flakes 有効化済み)
- **AWS CLI** (`aws configure` 済み)
- **SSH 鍵** (Terraform で指定したもの)
- **Cloudflare アカウント** (ドメイン `baisoku-kaigi.com` 管理用)

## デプロイ手順

### 1. インフラ構築 (Terraform)

```bash
cd infra/terraform
cp terraform.tfvars.example terraform.tfvars
# terraform.tfvars を編集
```

**terraform.tfvars に設定する値:**

| 変数 | 取得方法 |
|------|----------|
| `ssh_public_key` | ローカルの SSH 公開鍵 |
| `cloudflare_api_token` | 下記参照（2種類の権限が必要） |
| `cloudflare_zone_id` | Cloudflare ダッシュボード → `baisoku-kaigi.com` → 右サイドバー **Zone ID** |

**Cloudflare API Token の作成:**

1. [API Tokens](https://dash.cloudflare.com/profile/api-tokens) → **Create Token**
2. **Custom token** を選択
3. 以下の権限を付与:
   - `Zone - DNS - Edit`
   - `Zone - SSL and Certificates - Edit` (Origin Certificate用)
4. Zone Resources で `baisoku-kaigi.com` を選択
5. トークンを `terraform.tfvars` に記載

```bash
terraform init
terraform plan   # 変更内容を確認
terraform apply
```

これにより以下が自動設定されます:
- EC2 インスタンス + Elastic IP
- Cloudflare DNS レコード (`app.baisoku-kaigi.com`)
- Cloudflare Origin Certificate (15年有効、`origin-cert.pem` / `origin-key.pem`)
- セキュリティグループ (HTTP/HTTPS)


```bash
# 1. TFC Bootstrap (初回のみ / 管理者用)
# Terraform Cloud のワークスペース設定と変数を管理します
cd infra/tfc-bootstrap
cp terraform.tfvars.example terraform.tfvars
# terraform.tfvars に Cloudflare API Token などを設定
terraform init
terraform apply
```

```bash
# 2. インフラ構築 (Terraform Cloud)
# 通常の運用はこちら
cd infra/terraform

# TFCログイン (未ログインの場合)
terraform login

# プランと適用 (リモート実行)
terraform init
terraform plan
terraform apply
```

これにより以下が自動設定されます:
- EC2 インスタンス + Elastic IP
- Cloudflare DNS レコード (`app.baisoku-kaigi.com`)
- Cloudflare Origin Certificate (15年有効)
- セキュリティグループ (HTTP/HTTPS)

> **Note**: Terraform Cloud (TFC) 上で state 管理と `terraform plan/apply` の実行が行われます。
> AWS認証情報やAPIトークンは `tfc-bootstrap` で TFC に設定済みのため、ローカルでの設定は不要です。

#### 変数の確認・変更
変数は `infra/tfc-bootstrap/terraform.tfvars` で管理し、apply することで TFC に反映されます。
直接 TFC のダッシュボードから変更することも可能ですが、コード管理を推奨します。


> **Tip**: リソースIDはAWSコンソールまたは `aws ec2 describe-instances` 等で確認できます。

### 2. 環境変数の設定

プロジェクトルートに `.env.production` を作成し、シークレットを記述します。

```bash
# .env.production (例)
DATABASE_URL=postgresql://postgres:PASSWORD@db.xxx.supabase.co:5432/postgres?sslmode=require
OPENROUTER_API_KEY=sk-or-xxx
```

> **Note**: `NEXT_PUBLIC_` 変数は `flake.nix` に記述してください（ビルド時に埋め込まれます）。

### 3. アプリケーションデプロイ (Colmena)

```bash
nix run github:zhaofengli/colmena -- apply --on cartographer-prod
```

初回は EC2 上でのビルドに数分〜数十分かかります。

Colmena が自動的に以下をデプロイします:
- Haskell バックエンド
- Next.js フロントエンド
- nginx (SSL 終端 + リバースプロキシ)
- Cloudflare Origin Certificate

## 確認

デプロイ完了後、ブラウザで `https://app.baisoku-kaigi.com/` にアクセスできることを確認してください。

> **Note**: DNS 伝播に数分〜数時間かかる場合があります。

## 参考

- トラブルシューティング: [troubleshooting.md](./troubleshooting.md)

