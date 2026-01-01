# Cartographer トラブルシューティング

デプロイや運用中に発生する可能性のある問題と解決策をまとめています。

## 🛠 よくある問題

### 環境変数が反映されない
`.env.production` を変更した場合は、再度 `colmena apply` を実行してキーを再アップロードし、サービスを再起動する必要があります。

### EC2 再起動後にサービスが起動しない
`deployment.keys` で配置される `/run/keys` はインメモリなので、EC2 再起動時に消失します。
再起動後は `colmena apply` を再実行してキーを再アップロードしてください。

### `crypto.randomUUID is not a function` エラー
HTTP 環境（非セキュアコンテキスト）でアプリを開くと発生します。
アプリ側でポリフィルを実装済みですが、本番運用では HTTPS 化（Cloudflare 等）を推奨します。

### キーアップロードエラー (`No such file or directory`)
Flake を使用している場合、Git 管理外のファイルを相対パスで指定するとエラーになります。
`flake.nix` 内の `keyFile` が**絶対パス**で指定されているか確認してください。

### ビルドエラー (npm / sharp)
`sharp` などのバイナリ依存パッケージは、ARM 環境でのソースビルドに失敗することがあります。
`flake.nix` で `--ignore-scripts` を指定し、`package-lock.json` に Linux ARM64 用のバイナリ情報が含まれていることを確認してください。

### Supabase 接続エラー (`Network is unreachable`)
Supabase は IPv6 のみでエンドポイントを提供しています。
AWS VPC で IPv6 が有効になっていることを確認してください（Terraform 設定済み）。

## 🔧 技術詳細

### 環境変数の扱い

Next.js アプリケーションでは、環境変数の用途によって設定場所が異なります。

| 種類 | 変数名例 | 設定場所 | 備考 |
|------|----------|----------|------|
| **ビルド時** (Client) | `NEXT_PUBLIC_API_URL`など | `flake.nix` の `env` ブロック | ブラウザ側バンドルに含まれます。公開情報のみ。 |
| **実行時** (Server) | `DB_PASSWORD`, `API_SECRET` | `.env.production` | `deployment.keys` で転送され、サーバー側でのみ読み込まれます。 |

### デプロイツールの設定 (`flake.nix`)

- **buildOnTarget**: `true` に設定されています。ローカル（Mac）でクロスコンパイルするのではなく、ターゲットマシン（EC2）のリソースを使ってネイティブビルドを行います。これにより、QEMU 等の遅いエミュレーションを回避しています。
- **deployment.keys**: ローカルのシークレットファイルを SSH 経由で転送し、`/run/keys` に配置します。これらのファイルは Nix store には入りません。

### IPv6 ネットワーク設定

Supabase が IPv6 のみでエンドポイントを提供しているため、以下の設定が必要です（Terraform で設定済み）：
- VPC: `assign_generated_ipv6_cidr_block = true`
- Subnet: `ipv6_cidr_block` と `assign_ipv6_address_on_creation = true`
- Route Table: `::/0` → Internet Gateway
- Security Group: IPv6 アウトバウンドルール
- EC2: `ipv6_address_count = 1`
