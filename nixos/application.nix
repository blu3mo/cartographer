# Cartographer Application Services
# アプリケーション固有の設定のみ（systemd サービス等）
# infrastructure.nix と組み合わせて使用
{
  pkgs,
  lib,
  cartographer-backend,
  cartographer-frontend,
  ...
}:

{
  # Haskell backend service
  systemd.services.cartographer-backend = {
    description = "Cartographer Haskell Backend";
    after = [
      "network.target"
      "mnt-efs.mount"
    ];
    wantedBy = [ "multi-user.target" ];

    environment = {
      M36_DATA_PATH = "/mnt/efs/m36-data";
    };

    serviceConfig = {
      Type = "simple";
      User = "cartographer";
      Group = "cartographer";
      WorkingDirectory = "/var/lib/cartographer";
      ExecStart = "${lib.getExe cartographer-backend}";
      Restart = "always";
      RestartSec = 5;
    };
  };

  # Next.js frontend service
  systemd.services.cartographer-frontend = {
    description = "Cartographer Next.js Frontend";
    after = [
      "network.target"
      "cartographer-backend.service"
    ];
    wantedBy = [ "multi-user.target" ];

    environment = {
      NODE_ENV = "production";
      PORT = "80";
      HASKELL_BACKEND_URL = "http://localhost:8080";
    };

    serviceConfig = {
      Type = "simple";
      User = "root"; # Port 80 requires root or CAP_NET_BIND_SERVICE
      WorkingDirectory = "${cartographer-frontend}/app";
      ExecStart = "${pkgs.nodejs_20}/bin/node ${cartographer-frontend}/app/server.js";
      Restart = "always";
      RestartSec = 5;
    };
  };
}
