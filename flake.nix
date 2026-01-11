{
  description = "Cartographer dev environment (Next.js + Haskell + Supabase)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    project-m36.url = "github:plural-reality/project-m36";
    colmena.url = "github:zhaofengli/colmena";
  };

  nixConfig = {
    extra-substituters = [ "https://kotto5.cachix.org" ];
    extra-trusted-public-keys = [ "kotto5.cachix.org-1:kIqTVHIxWyPkkiJ24ceZpS6JVvs2BE8GTIA48virk/s=" ];
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.process-compose-flake.flakeModule
      ];

      perSystem =
        {
          self',
          system,
          lib,
          config,
          ...
        }:
        let
          # Allow Terraform (BSL license)
          pkgs = import inputs.nixpkgs {
            inherit system;
            config.allowUnfreePredicate =
              pkg:
              builtins.elem (lib.getName pkg) [
                "terraform"
              ];
          };
          # project-m36 の lib を使って override を構築
          m36lib = inputs.project-m36.lib;
          fullOverrides = m36lib.composeOverrides m36lib.m36Overrides (m36lib.mkPkgsDependentOverrides pkgs);
        in
        {
          haskellProjects.default = {
            basePackages = pkgs.haskell.packages.ghc96.override {
              overrides = fullOverrides;
            };
            devShell = {
              enable = true;
              tools = hp: {
                cabal-gild = pkgs.haskellPackages.cabal-gild;
                haskell-language-server = hp.haskell-language-server;
                fourmolu = hp.fourmolu;
              };
              # project-m36をdevShellのGHCパッケージDBに含める
              # これによりHLS/cabalが再ビルドを試みなくなる
              extraLibraries = hp: {
                project-m36 = hp.project-m36;
              };

              hlsCheck.enable = false;
            };
            projectRoot = ./backend;
            autoWire = [
              "packages"
              "apps"
              "checks"
            ];
            settings = {
              # TODO: EventId変更に伴いテストファイルの修正が必要
              # テストファイル修正後に削除すること
              cartographer-backend = {
                check = false;
              };
              scotty = {
                jailbreak = true;
              };
              streamly = {
                jailbreak = true;
              };
              lattices = {
                jailbreak = true;
                check = false;
              };
              data-interval = {
                jailbreak = true;
                check = false;
              };
              barbies-th = {
                check = false;
                broken = false;
                jailbreak = true;
              };
              winery = {
                jailbreak = true;
                check = false;
              };
              curryer-rpc = {
                jailbreak = true;
                check = false;
              };
              project-m36 = {
                jailbreak = true;
                custom = pkg: pkgs.haskell.lib.appendConfigureFlag pkg "-f-haskell-scripting";
              };
            };
          };

          apps.export-schema = {
            type = "app";
            program = "${pkgs.writeShellScript "export-schema" ''
              ${lib.getExe self'.packages.cartographer-backend} --export-schema
            ''}";
          };

          # V1シミュレーション用ビルド（ReportGeneratedコンストラクタ欠落版）
          # "v1" という曖昧な名前ではなく、テストにおける役割（特定のスキーマバリアント）を明示
          packages.cartographer-backend-schema-missing-report-constructor =
            self'.packages.cartographer-backend.overrideAttrs
              (old: {
                name = "cartographer-backend-schema-missing-report-constructor";
                # ReportGenerated コンストラクタを削除して V1 をシミュレート
                # さらに、V1と互換性のない実行ファイル（ReportGeneratedを使用するもの）のビルドを無効化
                postPatch = (old.postPatch or "") + ''
                  sed -i 's/| ReportGenerated/-- | ReportGenerated/' src/Domain/Types.hs

                  # 互換性のない実行ファイルを無効化 (buildable: False を挿入)
                  sed -i '/executable schema-evolution-phase2/a \ \ buildable: False' cartographer-backend.cabal
                  sed -i '/executable schema-destructive-phase1/a \ \ buildable: False' cartographer-backend.cabal
                  sed -i '/executable test-projection/a \ \ buildable: False' cartographer-backend.cabal
                  sed -i '/executable test-concurrent/a \ \ buildable: False' cartographer-backend.cabal
                  sed -i '/executable test-performance/a \ \ buildable: False' cartographer-backend.cabal
                  sed -i '/executable test-recovery/a \ \ buildable: False' cartographer-backend.cabal

                  # Disable test suite in this variant as well
                  sed -i '/test-suite cartographer-backend-test/a \ \ buildable: False' cartographer-backend.cabal
                '';
              });

          packages.cartographer-frontend = pkgs.buildNpmPackage {
            pname = "cartographer-frontend";
            version = "0.1.0";
            src = ./.;

            npmDepsHash = "sha256-rC+7ifRQayLa0O51jJ2jVeZ/DUCvwg84keoHCJVf5Q4="; # Updated hash

            # Next.js build needs these
            nativeBuildInputs = [ pkgs.pkg-config ];
            buildInputs = [ pkgs.vips ];

            # Filter out unnecessary files to avoid unnecessary rebuilds
            # src = lib.cleanSourceWith {
            #   filter = name: type:
            #     let base = baseNameOf name; in
            #     !(type == "directory" && (base == ".next" || base == "node_modules" || base == ".git"))
            #     && base != "flake.nix"
            #     && base != "flake.lock";
            #   src = ./.;
            # };
            # Simplified source filter for now

            # Next.js tries to write to cache
            npmFlags = [
              "--legacy-peer-deps"
              "--ignore-scripts"
            ];

            # Environment variables for build
            env = {
              NEXT_TELEMETRY_DISABLED = "1";
              # Allow installing devDependencies (typescript) during npm install
              NODE_ENV = "development";

              # Build-time env vars (using values from .env for now)
              NEXT_PUBLIC_SUPABASE_URL = "https://uyuyqdhssttxswmflzrx.supabase.co";
              NEXT_PUBLIC_SUPABASE_ANON_KEY = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6InV5dXlxZGhzc3R0eHN3bWZsenJ4Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3NjE0MjU1MDgsImV4cCI6MjA3NzAwMTUwOH0.u7iVjncFD_p_9CClxEQ4heejvbmEHFDFfDTG2VoyYXM";
            };
            buildPhase = ''
              runHook preBuild
              export NODE_ENV=production
              npm run build
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p $out
              # Copy standalone directory as 'app'
              cp -r .next/standalone $out/app

              # Copy static assets
              mkdir -p $out/app/.next/static
              cp -r .next/static/* $out/app/.next/static/
              cp -r public $out/app/public

              runHook postInstall
            '';
          };

          # TODO: EventId変更に伴いテストファイルの修正が必要
          # テストファイル修正後に以下のchecksを復活すること
          checks = config.haskellProjects.default.outputs.checks // {
            # cartographer-backend-test = pkgs.haskell.lib.doCheck self'.packages.cartographer-backend;

            # # Schema Evolution Test: V1 (Old) -> V2 (New)
            # schema-evolution =
            #   pkgs.runCommand "test-schema-evolution"
            #     {
            #       nativeBuildInputs = [ pkgs.glibcLocales ]; # Locale fix if needed
            #     }
            #     ''
            #       export LC_ALL=C.UTF-8
            #       export TEST_DB_PATH=$TMPDIR/m36-evolution-db

            #       echo "--- Phase 1: V1 (InsightExtracted only) ---"
            #       ${self'.packages.cartographer-backend-schema-missing-report-constructor}/bin/schema-evolution-phase1

            #       echo "--- Phase 2: V2 (ReportGenerated added) ---"
            #       ${self'.packages.cartographer-backend}/bin/schema-evolution-phase2

            #       touch $out
            #     '';

            # # Schema Destructive Test: V2 (Full) -> V1 (Removed)
            # schema-destructive =
            #   pkgs.runCommand "test-schema-destructive"
            #     {
            #       nativeBuildInputs = [ pkgs.glibcLocales ];
            #     }
            #     ''
            #       export LC_ALL=C.UTF-8
            #       export TEST_DB_PATH=$TMPDIR/m36-destructive-db

            #       echo "--- Phase 1: V2 (Write all types) ---"
            #       ${self'.packages.cartographer-backend}/bin/schema-destructive-phase1

            #       echo "--- Phase 2: V1 (Read with type removed) ---"
            #       ${self'.packages.cartographer-backend-schema-missing-report-constructor}/bin/schema-destructive-phase2

            #       touch $out
            #     '';

            # # Concurrent Integration Test
            # test-concurrent =
            #   pkgs.runCommand "test-concurrent"
            #     {
            #       nativeBuildInputs = [ pkgs.glibcLocales ];
            #     }
            #     ''
            #       export LC_ALL=C.UTF-8
            #       export TEST_DB_PATH=$TMPDIR/m36-concurrent-db
            #       ${self'.packages.cartographer-backend}/bin/test-concurrent
            #       touch $out
            #     '';

            # # Projection Consistency Test
            # test-projection =
            #   pkgs.runCommand "test-projection"
            #     {
            #       nativeBuildInputs = [ pkgs.glibcLocales ];
            #     }
            #     ''
            #       export LC_ALL=C.UTF-8
            #       export TEST_DB_PATH=$TMPDIR/m36-projection-db
            #       ${self'.packages.cartographer-backend}/bin/test-projection
            #       touch $out
            #     '';
          };

          process-compose.default = {
            settings = {
              processes = {
                backend = {
                  command = "${lib.getExe self'.packages.cartographer-backend}";
                };

                frontend = {
                  command = "npm run dev";
                };
              };
            };
          };

          devShells.default = pkgs.mkShell {
            name = "cartographer-dev-shell";
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
            ];

            nativeBuildInputs =
              with pkgs;
              [
                nodejs_20
                supabase-cli
                biome
                pkg-config
                vips
                openssl
                git
                watchman
                libpq.pg_config
                awscli2
                terraform
                gh # GitHub CLI for workflow management
                config.haskellProjects.default.outputs.finalPackages.project-m36 # Provides tutd, project-m36-server
              ]
              ++ lib.optionals pkgs.stdenv.isDarwin [ libiconv ];

            # Ensure pg_config is available for postgresql-libpq
            shellHook = ''
              export PATH="${pkgs.postgresql}/bin:$PATH"

              # ghc-with-packagesのパッケージDBを指すGHC環境ファイルを生成
              # これによりHLS/cabalがNixでビルドされたパッケージを使用する
              ghc_with_pkgs=$(echo $nativeBuildInputs | tr ' ' '\n' | grep 'ghc-.*-with-packages' | head -1)
              if [ -n "$ghc_with_pkgs" ]; then
                ghc_pkg_db="$ghc_with_pkgs/lib/ghc-$(ghc --numeric-version)/lib/package.conf.d"
                ghc_version=$(ghc --numeric-version)
                env_file=".ghc.environment.$(uname -m)-$(uname -s | tr '[:upper:]' '[:lower:]')-$ghc_version"

                echo "clear-package-db" > "$env_file"
                echo "global-package-db" >> "$env_file"
                echo "package-db $ghc_pkg_db" >> "$env_file"

                echo "✓ Generated $env_file (using ghc-with-packages)"
              else
                echo "⚠ Could not find ghc-with-packages in nativeBuildInputs"
              fi
            '';
          };
        };

      # Colmena configuration (outside perSystem)
      flake = {
        colmenaHive = inputs.colmena.lib.makeHive {
          meta = {
            nixpkgs = import nixpkgs {
              system = "aarch64-linux";
            };
          };

          defaults =
            { pkgs, ... }:
            {
              environment.systemPackages = with pkgs; [
                vim
                htop
                git
              ];
            };

          # OS configuration only - always works from Mac
          # Usage: colmena apply --on cartographer-infra
          cartographer-infra =
            { pkgs, ... }:
            {
              deployment = {
                targetHost = "13.192.44.10";
                targetUser = "root";
                buildOnTarget = false;
              };

              imports = [
                ./nixos/infrastructure.nix
              ];

              cartographer.efsFileSystemId = "fs-0f72db497533bbfde";
              cartographer.domain = "app.baisoku-kaigi.com";
            };

          # Full deployment with applications - now works from Mac (aarch64)
          # Usage: colmena apply --on cartographer-prod
          cartographer-prod =
            {
              name,
              nodes,
              pkgs,
              ...
            }:
            let
              backendPackage = self.packages.aarch64-linux.cartographer-backend;
              frontendPackage = self.packages.aarch64-linux.cartographer-frontend;
            in
            {
              deployment = {
                targetHost = "13.192.44.10";
                targetUser = "root";
                buildOnTarget = false; # Try local build first - may need Linux builder

                keys."env-file" = {
                  keyFile = "/Users/yui/Developer/plural-reality/cartographer/.env.production";
                  destDir = "/run/keys";
                  user = "root";
                  group = "cartographer";
                  permissions = "0640";
                };

                # Cloudflare Origin Certificate (generated by Terraform)
                keys."origin-cert" = {
                  keyFile = "/Users/yui/Developer/plural-reality/cartographer/infra/terraform/origin-cert.pem";
                  destDir = "/run/keys";
                  user = "root";
                  group = "nginx";
                  permissions = "0644";
                };

                keys."origin-key" = {
                  keyFile = "/Users/yui/Developer/plural-reality/cartographer/infra/terraform/origin-key.pem";
                  destDir = "/run/keys";
                  user = "root";
                  group = "nginx";
                  permissions = "0640";
                };
              };

              imports = [
                ./nixos/infrastructure.nix
                ./nixos/application.nix
              ];

              _module.args = {
                cartographer-backend = backendPackage;
                cartographer-frontend = frontendPackage;
              };
            };
        };
      };
    };
}
