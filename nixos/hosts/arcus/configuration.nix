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
    # ../../modules/development.nix
    ../../modules/development_php.nix
    # ../../modules/development_containers.nix
  ];

  # environment.systemPackages = with pkgs; [
  # ];

  # For a DO droplet, use grub only
  boot.loader.systemd-boot.enable = lib.mkForce false;
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/vda";
}
