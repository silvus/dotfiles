# Noctus - Laptop Configuration
{ pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix

    ../../modules/base.nix
    ../../modules/keyboard.nix
    ../../modules/syncthing.nix
    ../../modules/desktop_base.nix
    ../../modules/desktop_awesome.nix
    ../../modules/desktop_office.nix
    # ../../modules/desktop_3dmodel.nix
    # ../../modules/desktop_sway.nix
    # ../../modules/desktop_niri.nix
    ../../modules/laptop.nix
    # ../../modules/gaming.nix
    # ../../modules/printing.nix
    # ../../modules/desktop_udev.nix
    # ../../modules/security.nix
    ../../modules/mnt_movies.nix
    ../../modules/mnt_tvshows.nix
    ../../modules/mnt_torrents.nix
    # ../../modules/mnt_doc.nix
    ../../modules/development.nix
    # ../../modules/development_php.nix
    # ../../modules/development_containers.nix
    ../../modules/mdorg.nix
    ../../modules/movies.nix
    # ../../modules/transmission.nix
  ];

  # Bootloader (dual boot with Debian)
  # https://wiki.nixos.org/wiki/Systemd/boot
  boot.loader = {
    systemd-boot = {
      # The sort key used for Nix entries
      # sortKey = "a_01";

      # Dual boot with Debian
      # Use "d" key to change the default entry (the arrow)!
      extraEntries."debian.conf" = ''
        title Debian
        efi   /efi/debian/grubx64.efi
      '';
        # sort-key z_99_debian
    };
  };

  environment.systemPackages = with pkgs; [
    steam
  ];

  # Steam configuration
  # Just Steam for Noctus, not the whole gaming stuff
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
    gamescopeSession.enable = true;
  };

}
