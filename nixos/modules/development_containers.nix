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
    # Don't start incus.service at boot, let incus.socket start it on first use instead
    socketActivation = true;
  };

  # Incus env
  users.users.silvus.extraGroups = [ "incus-admin" ];
  # Enable nftables (required for Incus)
  networking.nftables.enable = true;

  networking.firewall = {
    # Required for container routing
    trustedInterfaces = [ "incusbr0" ];
    checkReversePath = false;
  };

  # Enable IPv4 forwarding
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = 1;
  };
}
