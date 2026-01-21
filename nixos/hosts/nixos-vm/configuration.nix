# NixOS VM - Testing Configuration
{ lib, pkgs, modulesPath, ... }:

{
  imports = [
    # Include the default incus configuration.
    "${modulesPath}/virtualisation/incus-virtual-machine.nix"
    # TODO: should I move this to the dotfiles?
    # ./incus-virtual-machine.nix

    ../../modules/base.nix
    ../../modules/keyboard.nix
    ../../modules/syncthing.nix
    ../../modules/desktop_base.nix
    # ../../modules/desktop_sway.nix
    ../../modules/desktop_awesome.nix
    ../../modules/laptop.nix
    # ../../modules/gaming.nix
    # ../../modules/printing.nix
    # ../../modules/security.nix
    ../../modules/mnt_movies.nix
    ../../modules/mnt_tvshows.nix
    # ../../modules/mnt_doc.nix
    ../../modules/development.nix
  ];

  # Host-specific networking
  networking = {
    hostName = "nixos-vm";
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

  # VM-specific packages
  # environment.systemPackages = with pkgs; [
  #   neofetch
  #   spice-vdagent
  # ];

  # Override greetd from desktop.nix for auto-login
  # services.greetd = {
  #   enable = true;
  #   settings.default_session = {
  #     # command = lib.mkForce "${pkgs.sway}/bin/sway";
  #     user = lib.mkForce "silvus";
  #   };
  # };

  # VM optimizations
  # services.spice-vdagentd.enable = true;
  # services.qemuGuest.enable = true;
}
