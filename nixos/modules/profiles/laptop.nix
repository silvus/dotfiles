# Laptop profile
{ pkgs, ... }:

{
  imports = [
    ../base.nix
    ../keyboard.nix
    ../syncthing.nix
    ../desktop_base.nix
    ../desktop_awesome.nix
    ../desktop_office.nix
    # ../desktop_3dmodel.nix
    # ../desktop_sway.nix
    # ../desktop_niri.nix
    ../laptop.nix
    # ../gaming.nix
    # ../printing.nix
    # ../desktop_udev.nix
    # ../security.nix
    ../mounts/movies.nix
    ../mounts/tvshows.nix
    ../mounts/torrents.nix
    # ../mounts/doc.nix
    ../development
    ../development/go.nix
    ../development/rust.nix
    ../development/python.nix
    ../development/nix.nix
    ../development/lua.nix
    ../development/bash.nix
    ../development/markdown.nix
    # ../development/php.nix
    ../development/containers.nix
    ../mdorg.nix
    ../movies.nix
    # ../transmission.nix
  ];

  environment.systemPackages = with pkgs; [
    steam
  ];

  # Steam configuration
  # Just Steam for laptops, not the whole gaming stuff
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
    gamescopeSession.enable = true;
  };

}
