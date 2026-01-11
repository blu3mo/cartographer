# Staging Environment 構築 - 引き継ぎドキュメント

> [!CAUTION]
> ## 本番環境を絶対に壊さないための注意事項
> 
> 1. **`TF_WORKSPACE` を必ず指定する**
>    - `terraform plan` や `apply` の前に必ず `TF_WORKSPACE=staging` を付ける
>    - `terraform workspace select` は**使用禁止**（状態が残り事故の元）
> 
> 2. **prod.tfvars は触らない**
>    - staging 作業中は `staging.tfvars` のみ使用
>    - 変更するファイルを確認してから保存
> 
> 3. **Colmena ターゲットを確認**
>    - `--on cartographer-staging` を必ず指定
>    - `--on cartographer-prod` は staging テスト完了後のみ
> 
> 4. **plan を必ず確認してから apply**
>    - `terraform plan` の出力で**本番リソースが含まれていないこと**を確認
>    - staging リソースのみが表示されることを確認
> 
> 5. **EFS (データベース) の分離を確認**
>    - `staging.tfvars` の `efs_creation_token = "cartographer-staging"` を確認
>    - 本番 EFS (`cartographer-m36`) とは別であること

---

## 目的

本番 EC2 に影響を与えずにデプロイをテストできる staging 環境を構築する。

---

## 完了済みの作業

### Terraform: Workspace + tfvars 方式の準備

- [x] `infra/terraform/variables.tf` に `efs_creation_token`, `dns_subdomain` を追加
- [x] `infra/terraform/storage.tf` を `var.efs_creation_token` を使用するよう修正
- [x] `infra/terraform/cloudflare.tf` を `var.dns_subdomain` を使用するよう修正
- [x] `infra/terraform/prod.tfvars` を作成
- [x] `infra/terraform/staging.tfvars` を作成
- [x] `.gitignore` を更新（`prod.tfvars`, `staging.tfvars` を許可）

### 確認済み

- `terraform plan -var-file=prod.tfvars` で**変数化による差分なし**を確認
- 検出されたドリフト（Cloudflare証明書等）は今回の変更とは無関係

---

### 3. 設定の動的連携とデプロイの実現

- [x] `flake.nix` に `cartographer-staging` を追加し、JSON からの自動読み込みを実装
- [x] Terraform outputs を整理し、秘密鍵を含まないメタデータ JSON を生成
- [x] `colmena apply --on cartographer-staging --impure` でのデプロイ成功を確認

---

## 運用手順 (Daily Operations)

## 重要ファイル

| ファイル | 説明 |
|---------|------|
| `infra/terraform/prod.tfvars` | 本番用変数 |
| `infra/terraform/staging.tfvars` | Staging用変数 (`dns_subdomain = "staging"`) |
| `flake.nix` | Colmena ターゲット定義（staging 追加必要） |

---

## 現在の plan 出力で検出されたドリフト

以下は**今回の変更とは無関係**なドリフト：

1. **Cloudflare Origin Certificate** - TF 外で削除された（HTTPS は動作中）
2. **local_file リソース** - TFC 環境では毎回作成される
3. **aws_instance 属性** - 計算済み属性の差分（実害なし）

---

## コマンドメモ

```bash
# 本番 plan
TF_WORKSPACE=default terraform plan -var-file=prod.tfvars

# staging plan
TF_WORKSPACE=staging terraform plan -var-file=staging.tfvars

# Colmena デプロイ
nix run github:zhaofengli/colmena -- apply --on cartographer-prod
nix run github:zhaofengli/colmena -- apply --on cartographer-staging
```
