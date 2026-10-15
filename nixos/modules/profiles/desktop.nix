# Desktop profile
{ pkgs, ... }:

{
  imports = [
    ../base.nix
    ../keyboard.nix
    ../syncthing.nix
    ../desktop_base.nix
    # ../desktop_sway.nix
    # ../desktop_niri.nix
    ../desktop_awesome.nix
    ../desktop_office.nix
    ../desktop_3dmodel.nix
    ../gaming.nix
    ../printing.nix
    ../desktop_udev.nix
    # ../security.nix
    ../mounts/movies.nix
    ../mounts/tvshows.nix
    ../mounts/torrents.nix
    # ../mounts/doc.nix
    ../development
    # ../development/go.nix
    ../development/rust.nix
    ../development/python.nix
    ../development/nix.nix
    ../development/lua.nix
    ../development/bash.nix
    ../development/markdown.nix
    ../development/php.nix
    ../development/containers.nix
    ../mdorg.nix
    ../movies.nix
    # ../transmission.nix
  ];

  environment.systemPackages = with pkgs; [
    kcc # convert comic/manga to EPUB
    rpi-imager # Flash image to rpi sd card

    # ios files access
    # libimobiledevice
    # ifuse # optional, to mount using 'ifuse'

    # godot

    obsidian
    # micro-full
  ];

  # Android notifications
  # programs.kdeconnect.enable = true;

  # IOS files access
  # services.usbmuxd.enable = true;
}
