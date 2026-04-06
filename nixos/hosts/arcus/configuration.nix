# Arcus
{ ... }:

{
  imports = [
    ./hardware-configuration.nix

    ../../modules/base.nix
    # ../../modules/keyboard.nix
    ../../modules/syncthing.nix
    # ../../modules/desktop_base.nix
    # ../../modules/desktop_sway.nix
    # ../../modules/desktop_awesome.nix
    # ../../modules/gaming.nix
    # ../../modules/printing.nix
    ../../modules/security.nix
    # ../../modules/mnt_movies.nix
    # ../../modules/mnt_tvshows.nix
    # ../../modules/mnt_torrents.nix
    # ../../modules/mnt_doc.nix
    ../../modules/development.nix
    ../../modules/development_php.nix
    # ../../modules/development_containers.nix
  ];

  # environment.systemPackages = with pkgs; [
  # ];


}
