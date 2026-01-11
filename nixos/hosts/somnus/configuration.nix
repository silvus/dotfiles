# Somnus
{ ... }:

{
  imports = [
    ./hardware-configuration.nix

    ../../modules/base.nix
    ../../modules/keyboard.nix
    # ../../modules/syncthing.nix
    ../../modules/desktop_base.nix
    # ../../modules/desktop_sway.nix
    ../../modules/desktop_awesome.nix
    # ../../modules/gaming.nix
    # ../../modules/security.nix
    ../../modules/mnt_movies.nix
    ../../modules/mnt_tvshows.nix
    ../../modules/mnt_doc.nix
    ../../modules/development.nix
  ];

  services.xserver = {
    enable = true;
    resolutions = [
      {x = 1920; y = 1080;}
    ];
  };

}
