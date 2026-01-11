# Cartographer NixOS Server Module - Base Configuration
# OS-level settings only (SSH, firewall, EFS mount, etc.)
# Application services are defined separately
{ pkgs, modulesPath, config, lib, ... }:

{
  imports = [
    "${modulesPath}/virtualisation/amazon-image.nix"
  ];

  options.cartographer.efsFileSystemId = lib.mkOption {
    type = lib.types.str;
    description = "The ID of the EFS file system to mount.";
  };

  config = {
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
      nodejs_20
    ];

    # Enable SSH
    services.openssh = {
      enable = true;
      settings = {
        PermitRootLogin = "prohibit-password";
        PasswordAuthentication = false;
      };
    };

    # SSH authorized keys for root
    users.users.root.openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJnYOFNcOn+q9TXSZg/PmON97ryakpnkBOvBHBOceELP cartographer-deploy"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHk5S3FIh2mtFzBZvumg/no+AYrC10zAJVUtteheQTNj cartographer-deploy-akiba"
    ];

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
      device = "${config.cartographer.efsFileSystemId}.efs.ap-northeast-1.amazonaws.com:/";
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

    # System state version
    system.stateVersion = "24.05";
  };
}
