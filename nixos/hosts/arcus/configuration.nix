# Arcus - Server
{ lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../modules/profiles/server.nix
    ../../modules/development
    ../../modules/development/php.nix
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
