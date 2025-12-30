# Cartographer NixOS Server Module
{ pkgs, ... }:

{
  # System packages
  environment.systemPackages = with pkgs; [
    vim
    htop
    git
    nfs-utils
  ];

  # Enable SSH
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "prohibit-password";
      PasswordAuthentication = false;
    };
  };

  # Firewall
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      22
      80
    ];
  };

  # EFS mount point
  fileSystems."/mnt/efs" = {
    device = "fs-PLACEHOLDER.efs.ap-northeast-1.amazonaws.com:/";
    fsType = "nfs4";
    options = [
      "nfsvers=4.1"
      "rsize=1048576"
      "wsize=1048576"
      "hard"
      "timeo=600"
      "retrans=2"
      "_netdev"
    ];
  };

  # Create application user
  users.users.cartographer = {
    isSystemUser = true;
    group = "cartographer";
    home = "/var/lib/cartographer";
    createHome = true;
  };
  users.groups.cartographer = { };

  # M36 data directory
  systemd.tmpfiles.rules = [
    "d /mnt/efs/m36-data 0755 cartographer cartographer -"
  ];

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
      # DATABASE_URL is set via deployment.keys
    };

    serviceConfig = {
      Type = "simple";
      User = "cartographer";
      Group = "cartographer";
      WorkingDirectory = "/var/lib/cartographer";
      ExecStart = "/run/current-system/sw/bin/cartographer-backend";
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
      WorkingDirectory = "/var/lib/cartographer/frontend";
      ExecStart = "${pkgs.nodejs_20}/bin/node server.js";
      Restart = "always";
      RestartSec = 5;
    };
  };

  # System state version
  system.stateVersion = "24.05";
}
