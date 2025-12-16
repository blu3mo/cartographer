{
  description = "Cartographer dev environment (Next.js + Haskell + Supabase)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    barbies-th = {
      url = "github:fumieval/barbies-th/46c7b8c68634b219ff12e7966983f9b46a5976d4";
      flake = false;
    };
    project-m36-src = {
      url = "github:agentm/project-m36";
      flake = false;
    };
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
          haskellProjects.default = {
            basePackages = pkgs.haskell.packages.ghc96.override {
              overrides = self: super: {
                project-m36 = self.callCabal2nix "project-m36" inputs.project-m36-src { };

                curryer-rpc = self.callHackageDirect {
                  pkg = "curryer-rpc";
                  ver = "0.4.0";
                  sha256 = "sha256-rGNTiZBJjDA1HpXoxQIsupvgQ5HpYh0U8JZVTdVDnIk=";
                } { };

                streamly = self.callHackageDirect {
                  pkg = "streamly";
                  ver = "0.10.1";
                  sha256 = "sha256-9tWZ/8YteD9ljhEmj8oYKIAyFcbQflX0D20j/NTe3qM=";
                } { };

                streamly-core = self.callHackageDirect {
                  pkg = "streamly-core";
                  ver = "0.2.2";
                  sha256 = "sha256-Ggo5ius3dp/TJFfrZSk31A6gSZHA6kLMtxFKe9MIvqQ=";
                } { };

                streamly-bytestring = self.callHackageDirect {
                  pkg = "streamly-bytestring";
                  ver = "0.2.2";
                  sha256 = "sha256-E/sMAvaJ5zGYwb5KAXa2KQo3FqyB+T2mRO6zOTCXpoY=";
                } { };

                lockfree-queue = self.callHackageDirect {
                  pkg = "lockfree-queue";
                  ver = "0.2.4";
                  sha256 = "sha256-h1s/tiBq5Gzl8FtenQacmxJp7zPJPnmZXtKDPvxTSa4=";
                } { };

                unicode-data = self.callHackageDirect {
                  pkg = "unicode-data";
                  ver = "0.2.0";
                  sha256 = "14crb68g79yyw87fgh49z2fn4glqx0zr53v6mapihaxzkikhkkc3";
                } { };

                barbies-th = self.callHackageDirect {
                  pkg = "barbies-th";
                  ver = "0.1.11";
                  sha256 = "sha256-U9mHuHAA0v74dKB2w2kLGx9dBKU6w8CRObtYQF97Gao=";
                } { };

                scotty = self.callHackageDirect {
                  pkg = "scotty";
                  ver = "0.22";
                  sha256 = "sha256-DY4lKmAmqGTrzKq93Mft9bu9Qc0QcsEVpKzgoWcBL2I=";
                } { };

                wai = self.callHackageDirect {
                  pkg = "wai";
                  ver = "3.2.4";
                  sha256 = "sha256-NARmVhT5G1eMdtMM1xp7RFpevunThAB4tltCMih+qu8=";
                } { };

                wai-extra = self.callHackageDirect {
                  pkg = "wai-extra";
                  ver = "3.1.14";
                  sha256 = "sha256-wMI9eTituRbMvYvbcA9pgIwFxkbdL1+2Xw78lghfWaU=";
                } { };

                foldable1-classes-compat = pkgs.haskell.lib.dontCheck (
                  pkgs.haskell.lib.doJailbreak (
                    pkgs.haskell.packages.ghc94.callCabal2nix "foldable1-classes-compat" (pkgs.fetchzip {
                      url = "https://hackage.haskell.org/package/foldable1-classes-compat-0.1/foldable1-classes-compat-0.1.tar.gz";
                      sha256 = "sha256-Om6/w38G4ZaBZAGzlFb6ElvU4BCU3aOCXogpIZsm4RE=";
                    }) { }
                  )
                );

                lattices = pkgs.haskell.lib.addBuildDepend super.lattices self.foldable1-classes-compat;
              };
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
