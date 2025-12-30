# Cartographer NixOS Server Module
# This module is imported by the Colmena configuration in flake.nix
# This module is imported by the Colmena configuration in flake.nix
# Application packages are passed via specialArgs
{ pkgs, modulesPath, lib, cartographer-backend, cartographer-frontend, ... }:

{
  imports = [
    "${modulesPath}/virtualisation/amazon-image.nix"
  ];

  # Nix configuration for Cachix
  nix.settings = {
    substituters = [ "https://kotto5.cachix.org" ];
    trusted-public-keys = [ "kotto5.cachix.org-1:kIqTVHIxWyPkkiJ24ceZpS6JVvs2BE8GTIA48virk/s=" ];
  };

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
      8080 # Haskell backend
    ];
  };

  # EFS mount point
  fileSystems."/mnt/efs" = {
    device = "fs-0f72db497533bbfde.efs.ap-northeast-1.amazonaws.com:/";
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
  # NOTE: The actual binary path will be set when we integrate the package
  # For now, this expects the binary to be in the system PATH
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
  # For now, using a standalone Next.js server
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

  # System state version
  system.stateVersion = "24.05";
}
