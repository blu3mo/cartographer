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
        {
          process-compose.default = {
            settings = {
              environment = {
                M36_DB_DIR = "./database/m36_data";
                M36_PORT = "6543";
              };

              processes = {
                project-m36 = {
                  command = ''
                    # Initialize database directory if not exists
                    if [ ! -d "$M36_DB_DIR" ]; then
                      echo "Initializing Project M36 data directory at $M36_DB_DIR..."
                      mkdir -p "$M36_DB_DIR"
                    fi

                    # Start Project M36 Server
                    # --database-name maps to a directory
                    # --port sets the listening port
                    # We start it in background to check readiness, but process-compose handles foreground processes better.
                    # M36 doesn't have a simple "init and exit" mode like initdb + single-user.
                    # So we will run the server, and have a separate 'setup' process or use wait_for logic.

                    # Simpler approach: Run the server. The schema application will be a separate 'oneshot' process (or manual for now).
                    # Actually, we can use a wrapper to apply schema on start if we want.

                    ${pkgs.haskellPackages.project-m36.bin}/bin/project-m36-server \
                      --database-directory "$M36_DB_DIR" \
                      --port "$M36_PORT" \
                      --fsync
                  '';
                  readiness_probe = {
                    exec = {
                      # tutd check to connect to the server
                      command = "${pkgs.haskellPackages.project-m36.bin}/bin/tutd -p $M36_PORT -h localhost -e ':showrelvar false'";
                    };
                    initial_delay_seconds = 2;
                    period_seconds = 5;
                    timeout_seconds = 3;
                    success_threshold = 1;
                    failure_threshold = 5;
                  };
                  availability = {
                    restart = "always";
                  };
                };

                # Auto-apply schema (depends on project-m36 being ready)
                m36-schema-apply = {
                  command = ''
                    echo "Applying TutorialD schema..."
                    ${pkgs.haskellPackages.project-m36.bin}/bin/tutd -p $M36_PORT -h localhost -f database/schema.ud
                  '';
                  depends_on = {
                    project-m36.condition = "process_healthy";
                  };
                  availability = {
                    restart = "on_failure";
                    max_restarts = 5;
                  };
                };
              };
            };
          };

          devShells.default = pkgs.mkShell {
            name = "cartographer-dev-shell";

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
                haskellPackages.project-m36
                # postgresWithExtensions # Removed
                config.process-compose.default.outputs.package
              ]
              ++ lib.optionals pkgs.stdenv.isDarwin [ libiconv ];
          };
        };
    };
}
