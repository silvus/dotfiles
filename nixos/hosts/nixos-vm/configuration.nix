# NixOS VM - Testing Configuration
{ config, pkgs, modulesPath, ... }:

{
  imports = [
    # Include the default incus configuration
    ./incus-virtual-machine.nix

    # Enable modules for testing
    ../../modules/desktop.nix
    ../../modules/gaming.nix
    ../../modules/laptop.nix
    ../../modules/security.nix
    ../../modules/syncthing.nix
    ../../modules/development.nix
  ];

  # Bootloader
  boot.loader = {
    timeout = 1;
    systemd-boot = {
      enable = true;
      configurationLimit = 9;
    };
  };

  # Host-specific networking
  networking = {
    hostName = "nixos-vm";
    dhcpcd.enable = false;
    useDHCP = false;
    useHostResolvConf = false;
  };

  # VM-specific network configuration
  systemd.network = {
    enable = true;
    networks."50-enp5s0" = {
      matchConfig.Name = "enp5s0";
      networkConfig = {
        DHCP = "ipv4";
        IPv6AcceptRA = true;
      };
      linkConfig.RequiredForOnline = "routable";
    };
  };

  # All modules enabled by import

  # VM-specific packages
  environment.systemPackages = with pkgs; [
    neofetch
    spice-vdagent
  ];

  # VM optimizations
  services.spice-vdagentd.enable = true;
  services.qemuGuest.enable = true;

  # This value determines the NixOS release
  system.stateVersion = "25.05";
}
