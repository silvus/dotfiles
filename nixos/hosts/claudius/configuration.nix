# Claudius - VM
{
  pkgs,
  lib,
  llm-agents,
  ...
}:

{
  imports = [
    ../../modules/profiles/vm.nix
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
    # ../../modules/mounts/movies.nix
    # ../../modules/mounts/tvshows.nix
    # ../../modules/mounts/doc.nix
    # ../../modules/development
    # ../../modules/development/go.nix
    # ../../modules/development/rust.nix
    # ../../modules/development/python.nix
    # ../../modules/development/nix.nix
    # ../../modules/development/lua.nix
    # ../../modules/development/bash.nix
    # ../../modules/development/markdown.nix
    # ../../modules/development/php.nix
  ];

  # Per-host, not shared via base.nix.
  system.stateVersion = "26.05";

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

  swapDevices = [
    {
      device = "/var/lib/swapfile";
      size = 8 * 1024; # 8GB
    }
  ];
}
