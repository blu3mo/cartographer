{
  description = "Cartographer dev environment (Next.js + Haskell + Supabase)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    project-m36.url = "github:plural-reality/project-m36";
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
          pkgs,
          ...
        }:
        let
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
    };
}
