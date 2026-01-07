# Virtus - Development Desktop Configuration
{ ... }:

{
  imports = [
    ./hardware-configuration.nix

    ../../modules/base.nix
    ../../modules/keyboard.nix
    ../../modules/syncthing.nix
    # Enable desktop modules
    ../../modules/desktop_base.nix
    # ../../modules/desktop_sway.nix
    ../../modules/desktop_awesome.nix
    ../../modules/gaming.nix
    # ../../modules/security.nix
    ../../modules/development.nix
    ../../modules/development_containers.nix
  ];
}
