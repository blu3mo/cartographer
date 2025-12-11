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
          # From Incoming: Runner script wrapper
          # Ensure scripts/db-up.sh exists in your repo
          dbUpScript = pkgs.writeShellScriptBin "db-up" ''
            export PG16_BIN="${postgresWithExtensions}/bin"
            ${self}/scripts/db-up.sh
          '';
        in
        {
          process-compose.default = {
            settings = {
              processes = {
                db = {
                  command = "${dbUpScript}/bin/db-up";
                  availability = {
                    restart = "always";
                  };
                  environment = {
                    PG16_BIN = "${postgresWithExtensions}/bin";
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
                dbUpScript

                config.process-compose.default.outputs.package
              ]
              ++ lib.optionals pkgs.stdenv.isDarwin [ libiconv ];

            shellHook = ''
              echo "Cartographer Dev Environment (Postgres 18 + Haskell)"
              export PG18_BIN="${postgresWithExtensions}/bin"
            '';
          };

          apps.db-up = {
            type = "app";
            program = "${dbUpScript}/bin/db-up";
          };

        };
    };
}
