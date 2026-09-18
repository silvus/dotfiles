# Arcus
{ lib, ... }:

{
  imports = [
    ./hardware-configuration.nix

    ../../modules/base.nix
    # ../../modules/keyboard.nix
    ../../modules/syncthing.nix
    # ../../modules/desktop_base.nix
    # ../../modules/desktop_sway.nix
    # ../../modules/desktop_awesome.nix
    # ../../modules/gaming.nix
    # ../../modules/printing.nix
    ../../modules/security.nix
    # ../../modules/mnt_movies.nix
    # ../../modules/mnt_tvshows.nix
    # ../../modules/mnt_torrents.nix
    # ../../modules/mnt_doc.nix
    # ../../modules/development
    # ../../modules/development/go.nix
    # ../../modules/development/rust.nix
    # ../../modules/development/python.nix
    # ../../modules/development/nix.nix
    # ../../modules/development/lua.nix
    # ../../modules/development/bash.nix
    # ../../modules/development/markdown.nix
    ../../modules/development/php.nix
    # ../../modules/development/containers.nix
    # ../../modules/mdorg.nix
  ];

  # Per-host, not shared via base.nix.
  system.stateVersion = "25.11";

  # environment.systemPackages = with pkgs; [
  # ];

  # For a DO droplet, use grub only
  boot.loader.systemd-boot.enable = lib.mkForce false;
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/vda";
}
