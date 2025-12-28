---
trigger: model_decision
description: Nix Flakeを用いて開発環境の構築、ビルド、デプロイメント（システム構成管理）、そしてサービスのコンポジションを「完璧」に遂行するために必要な前提知識
---

この知識体系は、単なるコマンドの羅列ではなく、Nixの評価モデル、依存関係の解決ロジック、OSごとの抽象化レイヤー（NixOS/nix-darwin/Home Manager）、そしてそれらを統合するFlakeのメカニズムを網羅します。

---

# AIエージェントのためのNix Flake完全運用知識ベース

## 第1章：NixとFlakeの核心概念（Core Concepts）

### 1.1 宣言的かつ再現可能なシステムモデル
AIエージェントはまず、Nixが従来の命令型ツール（apt, brewなど）とは異なり、システムの「あるべき状態（Desired State）」を宣言する純粋関数型言語であることを理解する必要があります。

*   **密閉性（Hermeticity）と再現性（Reproducibility）:** Nixはパッケージを毎回スクラッチからビルドし、入力が同じであれば出力がビット単位で一致することを保証しようとします。これにより「私の環境では動く」という問題を排除します。
*   **Nix Store:** すべてのパッケージとファイルは `/nix/store` 配下にハッシュ付きのパスとして保存されます。
*   **Flakes:** 従来のチャンネル（Channels）に代わる、依存関係をピン留めし（`flake.lock`）、評価の純粋性を保証するための標準規格です。

### 1.2 Flakeの解剖学（`flake.nix` Structure）
`flake.nix` は以下の2つの主要属性で構成されます。

1.  **Inputs (依存関係):** 他のFlake（nixpkgs, nix-darwin, home-managerなど）への参照。
    *   `url`: リポジトリの場所（例: `github:NixOS/nixpkgs/nixpkgs-unstable`）。
    *   `follows`: 依存関係の重複を防ぐために、異なるInput間で依存先を統一するメカニズム（例: `home-manager.inputs.nixpkgs.follows = "nixpkgs"`）。
2.  **Outputs (成果物):** ビルド結果、開発シェル、システム構成などの定義。
    *   `nixosConfigurations`: NixOSシステム定義。
    *   `darwinConfigurations`: macOSシステム定義。
    *   `homeConfigurations`: ユーザー環境定義（Home Manager）。
    *   `devShells`: 開発環境。
    *   `packages`: ビルド成果物。

---

## 第2章：開発環境の構築（Development Environments）

AIエージェントは、プロジェクトごとに隔離された開発環境を提供する必要があります。

### 2.1 `devShells` と再現性
*   `nix develop` コマンドにより、宣言されたツールのみがPATHに含まれるシェル環境に入ります。
*   **Direnv統合:** `programs.direnv.enable = true` および `nix-direnv` を有効化することで、ディレクトリ移動時に自動的に環境をロード・キャッシュし、高速化を図ります。

### 2.2 言語特化型環境の構築（Haskellの事例）
Haskellのような複雑な依存関係を持つ言語において、Nixは強力な制御を提供します。

*   **基本戦略:**
    *   単純なスクリプト: `nix-shell` シェバンを使用。
    *   **developPackage:** 単一パッケージの開発。`callCabal2nix` のラッパーとして機能。
    *   **shellFor:** 複数のローカルパッケージを同時に開発する場合に使用。`cabal.project` ファイルと連携し、依存関係を計算します。
*   **haskell-flake (flake-parts):** `flake-parts` モジュールシステムを利用して、`devShells`、`packages`、`checks` を自動配線（Auto-wire）します。
    *   `perSystem.haskellProjects`: 複数のプロジェクト定義が可能。
    *   `devShell.tools`: HLS (Haskell Language Server) や Hoogle などのツールを自動的に含めることが可能。
    *   `autoWire`: Flakeの出力（packages, appsなど）を自動生成する機能。

---

## 第3章：ビルドとパッケージ管理（Build & Package Management）

### 3.1 Derivation（派生）とオーバーライド
*   **Derivation:** ビルド指示書。Nix言語で記述され、ビルド環境、依存関係、ビルドスクリプトを含みます。
*   **Overlays:** 既存のパッケージ定義を変更するための主要メカニズム。
    *   パッケージのバージョン変更、パッチ適用、ビルドフラグの変更に使用します。
    *   例: `nixpkgs.overlays` に関数を追加し、`super`（変更前）から `self`（変更後）を定義します。
*   **Unstableパッケージの利用:** 安定版の構成内で特定のパッケージのみUnstable版を使用したい場合、`pkgsUnstable = import <nixpkgs-unstable> {}` のように別系統のNixpkgsをインポートして参照します。

### 3.2 高度なビルド設定
*   **Distributed Builds:** `nix.buildMachines` を設定し、SSH経由で他のマシン（Linux Builderなど）にビルドをオフロードできます。
    *   `speedFactor` や `supportedFeatures`（例: "kvm", "big-parallel"）でスケジューリングを制御。
*   **Sandbox:** `nix.settings.sandbox = true` により、ビルド中のネットワークアクセスやファイルシステムアクセスを遮断し、不純物（Impurities）の混入を防ぎます。

---

## 第4章：デプロイメントとシステム構成（Deployment & System Configuration）

AIエージェントは、ターゲットOS（NixOS, macOS, Linux）に応じて適切なモジュールを選択し、適用する必要があります。

### 4.1 macOS管理（nix-darwin）
macOSのシステム設定を宣言的に管理します。

*   **エントリポイント:** `darwinConfigurations."HostName"`。
*   **主要モジュールとオプション:**
    *   **System Defaults:** Dock、Finder、トラックパッドなどのmacOS固有設定を `system.defaults` 配下で詳細に制御可能。
        *   例: `system.defaults.dock.autohide` (Dockの自動非表示)。
        *   例: `system.defaults.finder.AppleShowAllFiles` (隠しファイル表示)。
    *   **Homebrew統合:** `homebrew.enable = true` で、App Storeアプリ（`masApps`）、Cask、Formulaを管理。
        *   `onActivation.cleanup`: "uninstall" や "zap" に設定することで、宣言されていないパッケージを自動削除し、完全な状態一致を強制可能。
    *   **Networking:** `networking.hostName` や `networking.dns` の設定。
    *   **Launchd:** `launchd.agents` (User) や `launchd.daemons` (System) を定義し、サービスの自動起動やスケジュール実行を管理。
        *   `serviceConfig` 内で `KeepAlive`, `StartInterval` などのplistキーを直接指定可能。

### 4.2 ユーザー環境管理（Home Manager）
ユーザー固有のドットファイル（`.zshrc`, `.gitconfig`など）やパッケージを管理します。

*   **インストール形態:**
    1.  **Standalone:** ユーザーが個別に `home-manager` コマンドを実行。OS非依存。
    2.  **NixOS/nix-darwin Module:** システム構成の一部として `home-manager.users` を定義。`nixos-rebuild` や `darwin-rebuild` で一括適用される。
*   **重要な設定:**
    *   `home.stateVersion`: 互換性維持のためのバージョン指定（必須）。
    *   `programs.git.enable`: Git設定の生成。
    *   `useGlobalPkgs`: システムの `pkgs` インスタンスを共有し、評価時間を短縮。
    *   `useUserPackages`: パッケージをユーザープロファイルではなく `/etc/profiles` 等にインストールするオプション（モジュール利用時）。
*   **DAG (Directed Acyclic Graph) 型オプション:** 設定ファイルの結合順序などを制御するために `hm.types.dagOf` が使用されます。
    *   `hm.dag.entryBefore`, `hm.dag.entryAfter` を用いて、特定の設定ブロックの挿入位置を精密に制御可能。

---

## 第5章：コンポジションとモジュールシステム（Compose & Modules）

複雑なシステムを構築するために、複数の設定ファイルやサービスを組み合わせる知識が必要です。

### 5.1 モジュールシステム
NixOS、nix-darwin、Home Managerは共通のモジュールシステムを採用しています。

*   **Imports:** 他の `.nix` ファイルを取り込み、設定を分割・再利用します。
*   **Config & Options:** オプション定義（宣言）と値の設定（定義）を分離・結合します。
*   **`lib` 関数:** `lib.mkForce`（強制上書き）や `lib.mkDefault`（デフォルト値）を用いて、競合する設定の優先順位を解決します。

### 5.2 サービスのコンポジション
AIエージェントは、データベースや監視ツールなどのサービスを定義し、連携させる必要があります。

*   **サービス定義:** 例えば `services.postgresql` や `services.redis` を有効化し、ポートやデータディレクトリを指定。
*   **依存関係:** Launchdの設定において、ソケットアクティベーション（`Sockets`）やパス設定（`StandardOutPath`）を通じてプロセス間の連携を構成。
*   **仮想化/コンテナ:** `nix-darwin` 上での `services.github-runners` や、Dockerコンテナを管理する `homebrew.whalebrews` の利用。

### 5.3 Flake-partsによる構造化
`flake-parts` を導入することで、Flakeの記述をモジュール化し、`perSystem` 属性を用いてシステムアーキテクチャごとの定義を簡潔に記述できます。

---

## 第6章：完全な実行とメンテナンス（Execution & Maintenance）

### 6.1 適用コマンド（Switching）
*   **nix-darwin:** `darwin-rebuild switch --flake .#HostName` でシステム全体を適用。
*   **Home Manager (Standalone):** `home-manager switch` でユーザー環境を更新。
    *   Flake利用時は `home-manager switch --flake .`。

### 6.2 競合とエラー処理
*   **Collision Error:** 既に同名のファイルやパッケージが存在する場合、Nixは上書きせずにエラーを出力します。
    *   解決策: 既存ファイルを削除するか、管理外にする。
*   **Rollback:** 生成された環境（Generation）は保存されており、過去の状態に戻すことが可能です。
    *   Home Manager: `home-manager generations` でIDを確認し、ストアパスの `activate` スクリプトを実行してロールバック。

### 6.3 クリーンアップ
*   `nix.gc.automatic = true` により、不要になったストアオブジェクト（ガベージ）を定期的に削除し、ディスク容量を確保します。
*   `nix.optimise.automatic = true` で、同一内容のファイルをハードリンクし、重複を排除します。

---

この知識ベースに基づき、AIエージェントは「flake.nixの作成」から「各モジュールの詳細設定」「デプロイ」「エラー時のロールバック」までを一貫して、かつ再現可能な状態で自律的に遂行することが可能になります。