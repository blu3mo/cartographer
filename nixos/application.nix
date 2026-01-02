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
  # SSL certificates from Cloudflare Origin CA (deployed via Colmena keys)
  # These are populated by `terraform output` and stored in .env.production
  security.acme.acceptTerms = true; # Required even though we don't use ACME

  # Add nginx user to keys group so it can access /run/keys/
  users.users.nginx.extraGroups = [ "keys" ];

  # nginx reverse proxy with SSL
  services.nginx = {
    enable = true;
    recommendedTlsSettings = true;
    recommendedOptimisation = true;
    recommendedGzipSettings = true;
    recommendedProxySettings = true;

    virtualHosts."baisoku-kaigi.com" = {
      forceSSL = true;
      sslCertificate = "/run/keys/origin-cert";
      sslCertificateKey = "/run/keys/origin-key";

      locations."/" = {
        proxyPass = "http://127.0.0.1:3000";
        proxyWebsockets = true;
      };
    };

    # Redirect www to non-www
    virtualHosts."www.baisoku-kaigi.com" = {
      forceSSL = true;
      sslCertificate = "/run/keys/origin-cert";
      sslCertificateKey = "/run/keys/origin-key";
      globalRedirect = "baisoku-kaigi.com";
    };
  };

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
      EnvironmentFile = "/run/keys/env-file";
    };
  };

  # Next.js frontend service (port 3000, behind nginx)
  systemd.services.cartographer-frontend = {
    description = "Cartographer Next.js Frontend";
    after = [
      "network.target"
      "cartographer-backend.service"
    ];
    wantedBy = [ "multi-user.target" ];

    environment = {
      NODE_ENV = "production";
      PORT = "3000";
      HASKELL_BACKEND_URL = "http://localhost:8080";
    };

    serviceConfig = {
      Type = "simple";
      User = "cartographer";
      Group = "cartographer";
      WorkingDirectory = "${cartographer-frontend}/app";
      ExecStart = "${pkgs.nodejs_20}/bin/node ${cartographer-frontend}/app/server.js";
      Restart = "always";
      RestartSec = 5;
      EnvironmentFile = "/run/keys/env-file";
    };
  };

  # Ensure nginx starts after keys are available
  systemd.services.nginx.after = [
    "origin-cert-key.service"
    "origin-key-key.service"
  ];
  systemd.services.nginx.wants = [
    "origin-cert-key.service"
    "origin-key-key.service"
  ];

  # Open firewall for HTTP/HTTPS
  networking.firewall.allowedTCPPorts = [
    80
    443
  ];
}
