# Arcus - Server
{ lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../modules/profiles/server.nix
    # ../../modules/development
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
  boot.loader.grub.configurationLimit = 5;

  # Small disk (25 GiB): keep the store lean
  nix.settings = {
    # Don't keep build deps of live derivations
    keep-outputs = lib.mkForce false;
    # GC during builds when free space drops below 2 GiB, until 6 GiB are free
    min-free = 2 * 1024 * 1024 * 1024;
    max-free = 6 * 1024 * 1024 * 1024;
  };
}
