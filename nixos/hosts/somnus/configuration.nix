# Somnus - Gaming Desktop Configuration
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

  # Bootloader (dual boot with Debian)
  boot.loader = {
    timeout = 1;
    efi.canTouchEfiVariables = true;
    systemd-boot = {
      enable = true;
      configurationLimit = 9;
      # Dual boot with Debian
      extraEntries."debian.conf" = ''
        title Debian
        efi   /efi/debian/grubx64.efi
        sort-key 1
      '';
    };
  };

  # All modules enabled by import

  # Gaming desktop packages
  environment.systemPackages = with pkgs; [
    # Gaming extras
    discord

    # Multimedia
    obs-studio
    audacity
    gimp

    # Communication
    slack
    zoom-us
  ];

  # Audio optimization for multimedia work
  services.pipewire = {
    jack.enable = true;
  };

  # Additional firewall ports for gaming/development
  networking.firewall.allowedTCPPorts = [
    3000  # Development servers
    8080  # Alternative web servers
  ];

  # This value determines the NixOS release
  system.stateVersion = "25.05";
}
