# Claudius - VM
{
  config,
  pkgs,
  lib,
  llm-agents,
  solidcraft,
  ...
}:

let
  # Only the MCP client: `solidcraft-cli mcp --connect` talks to the app running on the host.
  # Skips the desktop app (egui/winit window + wrapper), so a lot less to compile.
  solidcraft-cli =
    solidcraft.packages.${pkgs.stdenv.hostPlatform.system}.default.overrideAttrs
      (old: {
        pname = "solidcraft-cli";
        cargoBuildFlags = [
          "-p"
          "solidcraft-cli"
        ];
        cargoInstallFlags = [
          "-p"
          "solidcraft-cli"
        ];
        postInstall = "";
        postFixup = "";
      });
in
{
  imports = [
    ../../modules/profiles/vm.nix
    # Off by default: built from source (no binary cache), too long on a laptop.
    # Enable it per machine in custom/local.nix (see the hostname guard there).
    { options.claudius.solidcraft = lib.mkEnableOption "the SolidCraft MCP CLI (see .mcp.json)"; }
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

  environment.systemPackages =
    with pkgs;
    [
      llm-agents.packages.${stdenv.hostPlatform.system}.claude-code
      # llm-agents.packages.${stdenv.hostPlatform.system}.codex
    ]
    ++ lib.optional config.claudius.solidcraft solidcraft-cli;

  # More aggressive GC
  nix.gc.dates = lib.mkForce "hourly";
  nix.gc.options = lib.mkForce "--delete-older-than 3d";

  # Reclaim freed blocks from the host-side storage pool image
  services.fstrim = {
    enable = true;
    interval = "*:0/15"; # every 15 minutes
  };

  # Compressed swap in RAM instead of a swap file on disk
  zramSwap.enable = true;

  # Don't keep build deps of live derivations (base.nix): ~10 GiB in a throwaway VM
  nix.settings.keep-outputs = lib.mkForce false;
}
