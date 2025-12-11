{
  description = "Cartographer dev environment (Next.js + Supabase + OpenRouter)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
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

        # Custom package for pg_jsonschema
        pg_jsonschema = pkgs.callPackage ./nix/pg_jsonschema.nix {
          inherit (pkgs) buildPgrxExtension cargo-pgrx;
        };

        # Custom Postgres with extensions
        postgresWithExtensions = pkgs.postgresql_16.withPackages (p: [ pg_jsonschema ]);

        # Runner script wrapper to ensure dependencies are available
        # It calls the external script `scripts/db-up.sh`
        dbUpScript = pkgs.writeShellScriptBin "db-up" ''
          export PG16_BIN="${postgresWithExtensions}/bin"
          export PATH="${pkgs.process-compose}/bin:$PATH"
          ${self}/scripts/db-up.sh
        '';
      in
      {
        # 1. Dev Shell (nix develop)
        devShells.default = pkgs.mkShell {
          packages =
            with pkgs;
            [
              nodejs_20
              supabase-cli
              python3
              pkg-config
              vips
              openssl
              git
              watchman
              biome
              postgresWithExtensions
              dbUpScript
            ]
            ++ lib.optionals pkgs.stdenv.isDarwin [ libiconv ];

          shellHook = ''
            export PG16_BIN="${postgresWithExtensions}/bin"
          '';
        };

        # 2. Apps (nix run)
        apps.default = flake-utils.lib.mkApp {
          drv = dbUpScript;
        };
        apps.db-up = flake-utils.lib.mkApp {
          drv = dbUpScript;
        };
      }
    );
}
