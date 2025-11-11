# Virtus - Development Desktop Configuration
{ ... }:

{
  imports = [
    ./hardware-configuration.nix

    ../../modules/base.nix
    # Enable desktop modules
    ../../modules/desktop.nix
    ../../modules/gaming.nix
    # ../../modules/security.nix
    ../../modules/development.nix
  ];
}
