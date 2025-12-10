{
  description = "Cartographer dev environment (Next.js + Haskell + Supabase)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
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
          # From Incoming: Runner script wrapper
          # Ensure scripts/db-up.sh exists in your repo
          dbUpScript = pkgs.writeShellScriptBin "db-up" ''
            export PG18_BIN="${pkgs.postgresql_18}/bin"
            ${self}/scripts/db-up.sh
          '';
        in
        {
          # --- Haskell Project Configuration ---
          haskellProjects.default = {
            devShell = {
              enable = true;
              tools = hp: {
                inherit (pkgs)
                  supabase-cli
                  nodejs_20
                  biome
                  ;

                # Using Postgres 18 as per provided config
                postgresql = pkgs.postgresql_18;

                cabal-gild = hp.cabal-gild;
                haskell-language-server = hp.haskell-language-server;
              };
              hlsCheck.enable = true;
            };
            projectRoot = ./backend;
            autoWire = [
              "packages"
              "apps"
              "checks"
            ];
          };

          # --- Process Compose Configuration ---
          process-compose.default = {
            settings = {
              processes = {
                # 1. Database (Postgres 18 via script)
                db = {
                  command = "${dbUpScript}/bin/db-up";
                  availability = {
                    restart = "always";
                  };
                  environment = {
                    PG18_BIN = "${pkgs.postgresql_18}/bin";
                  };
                };

                # 2. Backend (Haskell)
                backend = {
                  command = "cd backend && cabal run";
                  depends_on.db = {
                    condition = "process_started";
                  };
                };

                # 3. Frontend (Next.js)
                frontend = {
                  command = "npm run dev";
                };
              };
            };
          };

          # --- Shell Configuration ---
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

                # Postgres 18
                postgresql_18

                # Allow running db-up manually
                dbUpScript

                # Process Compose
                config.process-compose.default.outputs.package
              ]
              ++ lib.optionals pkgs.stdenv.isDarwin [ libiconv ];

            shellHook = ''
              echo "Cartographer Dev Environment (Postgres 18 + Haskell)"
              export PG18_BIN="${pkgs.postgresql_18}/bin"
            '';
          };

          # Apps
          apps.db-up = {
            type = "app";
            program = "${dbUpScript}/bin/db-up";
          };
        };
    };
}
