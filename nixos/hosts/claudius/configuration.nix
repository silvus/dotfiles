# Claudius
{ pkgs, lib, llm-agents, modulesPath,... }:

{
  imports = [
    # Include the default incus configuration.
    "${modulesPath}/virtualisation/incus-virtual-machine.nix"
    # ./incus-virtual-machine.nix

    ../../modules/base.nix
    # ../../modules/keyboard.nix
    # ../../modules/syncthing.nix
    # ../../modules/desktop_base.nix
    # ../../modules/desktop_sway.nix
    # ../../modules/desktop_niri.nix
    # ../../modules/desktop_awesome.nix
    # ../../modules/laptop.nix
    # ../../modules/gaming.nix
    # ../../modules/printing.nix
    # ../../modules/security.nix
    # ../../modules/mnt_movies.nix
    # ../../modules/mnt_tvshows.nix
    # ../../modules/mnt_doc.nix
    # ../../modules/development.nix
    # ../../modules/development_php.nix
  ];

  # Per-host, not shared via base.nix.
  system.stateVersion = "26.05";

  # Host-specific networking
  networking = {
    hostName = "claudius";
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

  environment.systemPackages = with pkgs; [
    llm-agents.packages.${stdenv.hostPlatform.system}.claude-code
    # llm-agents.packages.${stdenv.hostPlatform.system}.codex
  ];

  # More aggressive GC
  nix.gc.dates = lib.mkForce "hourly";
  nix.gc.options = lib.mkForce "--delete-older-than 3d";

  # Reclaim freed blocks from the host-side storage pool image
  services.fstrim = {
    enable = true;
    interval = "*:0/15"; # every 15 minutes
  };

  swapDevices = [ {
    device = "/var/lib/swapfile";
    size = 8*1024; # 8GB
  } ];
}
