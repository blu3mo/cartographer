# Cartographer デプロイメントガイド

## アーキテクチャ

```
ユーザー → Cloudflare (HTTPS) → EC2 (NixOS)
                                   ├── Next.js (:80)
                                   └── Haskell (:8080)
                                        └── EFS (M36 データ)
```

## 前提条件

- AWS CLI 設定済み `aws configure`
- Nix インストール済み
- SSH 鍵ペア作成済み

## 初回セットアップ

### 1. Terraform でインフラ作成

```bash
cd infra/terraform
cp terraform.tfvars.example terraform.tfvars
# terraform.tfvars の ssh_public_key を編集

terraform init
terraform plan
terraform apply
```

### 2. SSH 設定

`~/.ssh/config` に追加：
```
Host cartographer-prod
  HostName <Terraform 出力の instance_public_ip>
  User root
  IdentityFile ~/.ssh/cartographer
```

### 3. Cloudflare DNS

Terraform 出力の `instance_public_ip` を Cloudflare DNS に設定

## デプロイ

### インフラのみ（Mac から実行可能）

OS 設定、SSH 鍵、ファイアウォール等の変更：

```bash
nix run github:zhaofengli/colmena -- apply --on cartographer-infra
```

### フルデプロイ（GitHub Actions から）

アプリケーション込みのデプロイ（x86_64-linux ビルドが必要）：

```bash
nix run github:zhaofengli/colmena -- apply --on cartographer-prod
```

## ファイル構成

| パス | 説明 |
|------|------|
| `infra/terraform/` | AWS インフラ定義 |
| `nixos/infrastructure.nix` | OS 設定（SSH, EFS 等） |
| `nixos/application.nix` | アプリ設定（systemd サービス） |
| `.github/workflows/deploy.yml` | CI/CD ワークフロー |

## GitHub Secrets

| 名前 | 内容 |
|------|------|
| `DEPLOY_SSH_KEY` | SSH 秘密鍵 |
| `SERVER_IP` | EC2 パブリック IP |

## SSH 鍵の追加

新しい SSH 鍵を追加する場合：

1. `nixos/infrastructure.nix` の `authorizedKeys` に追加
2. `nix run github:zhaofengli/colmena -- apply --on cartographer-infra`
