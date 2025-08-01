# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, ... }:

# let
  # home-manager = builtins.fetchTarball https://github.com/nix-community/home-manager/archive/release-25.05.tar.gz;
# in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      # (import "${home-manager}/nixos")
    ];

  # Bootloader (Only for dualboots)
  boot.loader.timeout = 1;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 9;
  # See /boot/EFI/debian
  boot.loader.systemd-boot.extraEntries."debian.conf" = ''
    title Debian
    efi   /efi/debian/grubx64.efi
    sort-key 0
  '';

  # boot.loader.systemd-boot.enable = false;
  # boot.loader.efi.canTouchEfiVariables = true;
  # boot.loader.grub.enable = true;
  # boot.loader.grub.device = [ "nodev" ];
  # boot.loader.grub.useOSProber = true;
  # boot.loader.grub.efiSupport = true;

  networking.hostName = "noctus";
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Laptop power management (https://nixos.wiki/wiki/Laptop)
  services.tlp.enable = true;

  # Login init
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --asterisks --greet-align left --time --cmd sway";
        user = "greeter";
      };
    };
  };

  security.polkit.enable = true;
  security.pam.services.swaylock = {};

  environment.sessionVariables.NIXOS_OZONE_WL = "1";
}

