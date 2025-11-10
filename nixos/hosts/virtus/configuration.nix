# Virtus - Development Desktop Configuration
{ ... }:

{
  imports = [
    ./hardware-configuration.nix

    # Enable desktop modules
    ../../modules/desktop.nix
    ../../modules/gaming.nix
    # ../../modules/security.nix
    ../../modules/syncthing.nix
    ../../modules/development.nix
  ];
}
