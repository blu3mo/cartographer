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
          pg_jsonschema = pkgs.callPackage ./nix/pg_jsonschema.nix {
            inherit (pkgs) buildPgrxExtension cargo-pgrx;
          };
          postgresWithExtensions = pkgs.postgresql_16.withPackages (p: [ pg_jsonschema ]);

        in
        {
          process-compose.default = {
            settings = {
              environment = {
                PG16_BIN = "${postgresWithExtensions}/bin";
                PG16_DATA = "./database/.data";
              };

              processes = {
                postgres = {
                  command = ''
                    if [ ! -d "$PG16_DATA" ]; then
                      echo "Initializing Postgres 16 data directory at $PG16_DATA..."
                      $PG16_BIN/initdb -D "$PG16_DATA" -U postgres --auth=trust --no-locale --encoding=UTF8
                    fi
                    $PG16_BIN/postgres -D "$PG16_DATA" -p 5433
                  '';
                  readiness_probe = {
                    exec = {
                      command = "$PG16_BIN/pg_isready -p 5433 -h localhost -U postgres";
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

                migrate = {
                  command = ''
                    echo "Applying database/schema.sql..."
                    $PG16_BIN/psql -h localhost -p 5433 -U postgres -d postgres -f database/schema.sql
                  '';
                  depends_on = {
                    postgres = {
                      condition = "process_healthy";
                    };
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

                postgresWithExtensions

                config.process-compose.default.outputs.package
              ]
              ++ lib.optionals pkgs.stdenv.isDarwin [ libiconv ];

            shellHook = ''
              echo "Cartographer Dev Environment (Postgres 16 + Haskell)"
              export PG16_BIN="${postgresWithExtensions}/bin"
            '';
          };
        };
    };
}
