# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, modulesPath, ... }:

{
  imports = [
    # Include the default incus configuration.
    "${modulesPath}/virtualisation/incus-virtual-machine.nix"
  ];

  # Bootloader
  boot.loader.timeout = 1;
  boot.loader.systemd-boot.configurationLimit = 9;

  networking.hostName = "nixos-vm";

  networking = {
    dhcpcd.enable = false;
    useDHCP = false;
    useHostResolvConf = false;
  };

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

  # Login init
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --asterisks --greet-align left --time --cmd sway";
        user = "greeter";
      };
    };
  };

  security.polkit.enable = true;
  security.pam.services.swaylock = {};

  environment.sessionVariables.NIXOS_OZONE_WL = "1";
}

