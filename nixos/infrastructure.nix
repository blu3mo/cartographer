# Cartographer NixOS Server Module - Base Configuration
# OS-level settings only (SSH, firewall, EFS mount, etc.)
# Application services are defined separately
{ pkgs, modulesPath, ... }:

{
  imports = [
    "${modulesPath}/virtualisation/amazon-image.nix"
  ];

  # Nix configuration for Cachix
  nix.settings = {
    substituters = [ "https://kotto5.cachix.org" ];
    trusted-public-keys = [ "kotto5.cachix.org-1:kIqTVHIxWyPkkiJ24ceZpS6JVvs2BE8GTIA48virk/s=" ];

    # Post-build hook to automatically push to Cachix
    # Token is fetched from AWS SSM Parameter Store using EC2 IAM Role
    post-build-hook = pkgs.writeShellScript "cachix-push-hook" ''
      set -euf

      # Check if we're on EC2 with IAM role (IMDS available)
      if ${pkgs.curl}/bin/curl -s -m 1 http://169.254.169.254/latest/meta-data/ > /dev/null 2>&1; then
        # Fetch token from AWS SSM Parameter Store
        CACHIX_AUTH_TOKEN=$(${pkgs.awscli2}/bin/aws ssm get-parameter \
          --name "/cartographer/cachix-auth-token" \
          --with-decryption \
          --query "Parameter.Value" \
          --output text \
          --region ap-northeast-1 2>/dev/null || true)

        if [ -n "$CACHIX_AUTH_TOKEN" ]; then
          export CACHIX_AUTH_TOKEN
          echo "Pushing to Cachix: $OUT_PATHS"
          ${pkgs.cachix}/bin/cachix push kotto5 $OUT_PATHS
        else
          echo "Cachix token not available, skipping push"
        fi
      fi
    '';
  };

  # System packages
  environment.systemPackages = with pkgs; [
    vim
    htop
    git
    nfs-utils
    nodejs_20
    awscli2  # Required for SSM Parameter Store access
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
  # Uses automount to handle cases where DNS resolution is delayed at boot
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
      "nofail"                    # Don't fail boot if mount fails
      "x-systemd.automount"       # Mount on first access
      "x-systemd.idle-timeout=60" # Unmount after 60s of inactivity (optional)
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

  # System state version
  system.stateVersion = "24.05";
}
