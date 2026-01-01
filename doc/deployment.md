# Cartographer デプロイメントガイド

AWS ARM インスタンス上の NixOS システムへのデプロイ手順です。

通常は 2. 以降のみで良いと思います(AWS自体をいじる必要がない場合。)

## 前提条件

- **Nix** (Flakes 有効化済み)
- **AWS CLI** (`aws configure` 済み)
- **SSH 鍵** (Terraform で指定したもの)

## デプロイ手順

### 1. インフラ構築 (Terraform)

```bash
cd infra/terraform
cp terraform.tfvars.example terraform.tfvars
# terraform.tfvars を編集 (ssh_public_key など)

terraform init
terraform apply
```

出力された `instance_public_ip` を `flake.nix` の `targetHost` に設定してください。

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

## 確認

デプロイ完了後、ブラウザで `http://<instance_public_ip>/` にアクセスできることを確認してください。

## 参考

- トラブルシューティング: [troubleshooting.md](./troubleshooting.md)
