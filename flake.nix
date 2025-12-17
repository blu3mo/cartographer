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
            '';
          };
        };
    };
}
