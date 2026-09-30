{ pkgs, lib, ... }:

with lib;

{
  environment.systemPackages = with pkgs; [
    # Containers
    incus
  ];

  # Podman
  # virtualisation.podman.enable = true;
  # Docker
  # virtualisation.docker.enable = true;

  # Enable Incus containers
  virtualisation.incus = {
    enable = true;
    # ui.enable = true;
    # Don't start incus.service at boot, let incus.socket start it on first use instead
    socketActivation = true;
  };

  # Incus starts virtiofsd with --cache=never and no --allow-mmap, so shared mmap of files in a VM's disk shares fails with ENODEV.
  # libgit2 (used by nix for git+file flakes) mmaps pack files that way and then can't see any packed object ("object not found" from `nix develop` in the VM).
  nixpkgs.overlays = [
    (final: prev: {
      virtiofsd = prev.writeShellScriptBin "virtiofsd" ''
        exec ${prev.virtiofsd}/bin/virtiofsd --allow-mmap "$@"
      '';
    })
  ];

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
