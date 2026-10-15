# NixOS VM
{ ... }:

{
  imports = [
    ../../modules/profiles/vm.nix
    ../../modules/keyboard.nix
    # ../../modules/syncthing.nix
    ../../modules/desktop_base.nix
    # ../../modules/desktop_sway.nix
    ../../modules/desktop_niri.nix
    # ../../modules/desktop_awesome.nix
    # ../../modules/laptop.nix
    # ../../modules/gaming.nix
    # ../../modules/printing.nix
    # ../../modules/security.nix
    # ../../modules/mounts/movies.nix
    # ../../modules/mounts/tvshows.nix
    # ../../modules/mounts/doc.nix
    ../../modules/development
    ../../modules/development/go.nix
    ../../modules/development/rust.nix
    ../../modules/development/python.nix
    ../../modules/development/nix.nix
    ../../modules/development/lua.nix
    ../../modules/development/bash.nix
    ../../modules/development/markdown.nix
    ../../modules/development/php.nix
  ];

  # Per-host, not shared via base.nix.
  system.stateVersion = "26.05";

  # services.xserver = {
  #   enable = true;
  #   resolutions = [
  #     {x = 1920; y = 1080;}
  #   ];
  # };

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
