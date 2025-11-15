{
  description = "Cartographer dev environment (Next.js + Supabase + OpenRouter)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        lib = pkgs.lib;

        # Stage 1: builder (includes devDependencies; produces .next with standalone output)
        cartographerBuilder = pkgs.buildNpmPackage {
          pname = "cartographer-builder";
          version = "0.1.0";
          src = ./.;
          # Set to lib.fakeHash first to capture real hash from nix build error, then replace.
          npmDepsHash = "sha256-8XmtEuCfCCt99PyLpnT522R1WI5fTRrdxf9+90TGi6U=";
          npmBuildScript = "build"; # runs next build (should use output: 'standalone')
          dontNpmPrune = true; # keep devDependencies: typescript, etc.
          NPM_CONFIG_PRODUCTION = "false"; # ensure devDeps installed inside build
          # Build-time dummy environment to satisfy env checks without leaking secrets into the store.
          # Runtime should override these with real values via systemd, docker, etc.
          NEXT_PUBLIC_SUPABASE_URL = "https://build.example.supabase.co";
          NEXT_PUBLIC_SUPABASE_ANON_KEY = "build-anon-key";
          SUPABASE_SERVICE_ROLE_KEY = "build-service-role-key";
          OPENROUTER_API_KEY = "build-openrouter-key";
          NEXT_TELEMETRY_DISABLED = "1";
          nativeBuildInputs = [
            pkgs.python3
            pkgs.pkg-config
            pkgs.vips
            pkgs.openssl
          ];
          installPhase = ''
            runHook preInstall
            mkdir -p $out/work
            cp -r .next $out/work/.next
            cp -r public $out/work/public 2>/dev/null || true
            cp -r next.config.* $out/work/ 2>/dev/null || true
            cp package.json $out/work/package.json
            runHook postInstall
          '';
        };

        # Stage 2: runtime (minimal closure using standalone server.js + static assets)
        cartographerRuntime = pkgs.stdenv.mkDerivation {
          pname = "cartographer";
          version = "0.1.0";
          src = cartographerBuilder;
          installPhase = ''
            runHook preInstall
            mkdir -p $out
            # Copy standalone Node runtime produced by next build
            cp -r $src/work/.next/standalone/* $out/
            mkdir -p $out/.next
            cp -r $src/work/.next/static $out/.next/static
            if [ -d "$src/work/public" ]; then
              cp -r $src/work/public $out/public
            fi
            mkdir -p $out/bin
            cat > $out/bin/start-cartographer <<'EOF'
            #!${pkgs.runtimeShell}
            set -euo pipefail
            export NODE_ENV=production
            # Required runtime environment variables (inject externally):
            #   NEXT_PUBLIC_SUPABASE_URL
            #   NEXT_PUBLIC_SUPABASE_ANON_KEY
            #   OPENROUTER_API_KEY
            #   SUPABASE_SERVICE_ROLE_KEY
            cd "$(dirname "$0")/.."
            exec ${pkgs.nodejs_20}/bin/node server.js -p "''${PORT:-3000}" --hostname 0.0.0.0
            EOF
            chmod +x $out/bin/start-cartographer
            runHook postInstall
          '';
        };
      in
      {
        packages = {
          builder = cartographerBuilder;
          runtime = cartographerRuntime;
          default = cartographerRuntime; # nix build .# で最小閉包
        };

        apps.default = {
          type = "app";
          program = "${cartographerRuntime}/bin/start-cartographer"; # nix run で起動
        };

        devShells.default = pkgs.mkShell {
          packages =
            with pkgs;
            [
              nodejs_20
              supabase-cli
              postgresql
              python3
              pkg-config
              vips
              openssl
              git
              watchman

            ]
            ++ lib.optionals pkgs.stdenv.isDarwin [ libiconv ];

          # 任意: 自動 npm インストールをしたい場合はここに shellHook を置く
          # （以前提案の lockfile ハッシュ判定版を入れられます）
        };
      }
    );
}
