{
  description = "Cartographer dev environment (Next.js + Haskell + Supabase)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
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
        {
          # --- Haskell Project Configuration (using haskell-flake) ---
          haskellProjects.default = {
            # The base package set representing the stackage snapshot or standard package set.
            # basePackages = pkgs.haskellPackages;

            # configuration for development shell
            devShell = {
              enable = true;
              tools = hp: {
                inherit (pkgs)
                  postgresql # CLI tools (psql)
                  supabase-cli
                  nodejs_20
                  biome
                  ;

                # Haskell tools
                ghcid = null; # Don't install ghcid from haskellPackages, we might want to let hls handle verify
                cabal-gild = hp.cabal-gild;
                haskell-language-server = hp.haskell-language-server;
              };

              hlsCheck.enable = true; # Check with HLS on shell startup
            };

            # Root of the Haskell project (where the .cabal file is)
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
                # 1. Database
                # Note: For local dev, we might assume Supabase is running or start a local postgres
                # If we want to use the existing supabase setup:
                # db.command = "supabase start";

                # For now, we assume the user has a DB running or uses `supabase start` outside.
                # We define a dummy process that just checks or waits, so 'backend' dependency is satisfied.
                db = {
                  command = "echo 'Database readiness check...'; sleep 10000";
                  availability = {
                    restart = "always";
                  };
                };

                # 2. Backend (Haskell)
                # Using ghcid for hot-reloading
                backend = {
                  command = "cd backend && cabal run";
                  # availability.restart = "on_failure";
                  depends_on.db = {
                    condition = "process_started";
                  };

                };

                # 3. Frontend (Next.js)
                frontend = {
                  command = "npm run dev";
                  environment = {
                    # Add any env vars needed
                  };
                };
              };
            };
          };

          # --- Shell Configuration ---
          # merging haskell devShell with other tools
          devShells.default = pkgs.mkShell {
            name = "cartographer-dev-shell";

            # Import inputs from the managed haskell shell
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
            ];

            nativeBuildInputs =
              with pkgs;
              [
                # Node.js & Frontend tools
                nodejs_20
                supabase-cli
                biome

                # General utilities
                pkg-config
                vips
                openssl
                git
                watchman

                # Process Compose
                config.process-compose.default.outputs.package
              ]
              ++ lib.optionals pkgs.stdenv.isDarwin [ libiconv ];

            shellHook = ''
              echo "Cartographer Dev Environment"
              echo "Run 'nix run .#default' (or just 'nix run') to start the stack with process-compose."
            '';
          };
        };
    };
}
