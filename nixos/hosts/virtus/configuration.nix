# Virtus - Development Desktop Configuration
{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix

    # Enable desktop modules
    ../../modules/desktop.nix
    ../../modules/gaming.nix
    ../../modules/security.nix
    ../../modules/syncthing.nix
    ../../modules/development.nix
  ];

  # Bootloader
  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
    systemd-boot.configurationLimit = 10;
  };

  # All modules enabled by import

  # Development desktop packages
  environment.systemPackages = with pkgs; [
    # Development tools
    docker-compose
    postman

    # Communication
    discord
    slack

    # Media
    spotify
    obs-studio

    # Extra development
    dbeaver-bin
    wireshark
  ];

  # Development optimizations
  boot.kernel.sysctl = {
    # Better for development workloads
    "vm.swappiness" = 60;
    "fs.inotify.max_user_watches" = 524288;
  };

  # Additional firewall ports for development
  networking.firewall.allowedTCPPorts = [
    3000  # Development servers
    8000  # Alternative dev servers
    8080  # HTTP alternative
    9000  # Various dev tools
  ];

  # This value determines the NixOS release
  system.stateVersion = "25.05";
}
