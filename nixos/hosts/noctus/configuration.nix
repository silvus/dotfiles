# Noctus - Laptop Configuration
{ ... }:

{
  imports = [
    ./hardware-configuration.nix

    # Enable laptop modules
    ../../modules/desktop.nix
    ../../modules/laptop.nix
    # ../../modules/security.nix
    ../../modules/syncthing.nix
    ../../modules/development.nix
  ];

  # Bootloader (dual boot with Debian)
  boot.loader = {
    timeout = 1;
    efi.canTouchEfiVariables = true;
    systemd-boot = {
      enable = true;
      configurationLimit = 9;
      # Dual boot with Debian
      extraEntries."debian.conf" = ''
        title Debian
        efi   /efi/debian/grubx64.efi
        sort-key 0
      '';
    };
  };
}
