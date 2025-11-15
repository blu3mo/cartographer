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
      in
      {
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
