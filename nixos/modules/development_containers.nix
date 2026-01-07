{ pkgs, lib, ... }:

with lib;

{
  environment.systemPackages = with pkgs; [
    # Containers
    incus
  ];

  # Enable Incus containers
  virtualisation.incus = {
    enable = true;
    ui.enable = true;
  };

  # Incus env
  users.users.silvus.extraGroups = [ "incus-admin" ];
  # Enable nftables (required for Incus)
  networking.nftables.enable = true;
}
