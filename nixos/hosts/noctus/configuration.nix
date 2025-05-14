# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

let
  # home-manager = builtins.fetchTarball https://github.com/nix-community/home-manager/archive/release-24.11.tar.gz;
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      # (import "${home-manager}/nixos")
    ];
  
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.warn-dirty = false;

  # Bootloader.
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

  networking.hostName = "noctus"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";
 
#  services.greetd = {
#    enable = true;
#    settings = {
#      initial_session = {
#	command = "${pkgs.hyprland}/bin/Hyprland";
#	user = "silvus";
#	};
#      default_session = {
#        # command = "${pkgs.greetd.tuigreet}/bin/tuigreet --asterisks --remember --time --cmd \"dbus-run-session Hyprland\"";
#        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --asterisks --remember --remenber-user-session --time --cmd \"${pkgs.hyprland}/bin/Hyprland\"";
# 	user = "greeter";
#        # user = "silvus";
#      };
#    };
#  };

# programs.hyprland.xwayland.enable = true;

  programs.dconf.enable = true;

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd sway";
        user = "greeter";
      };
    };
  };

  security.polkit.enable = true;
  security.pam.services.swaylock = {};

  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };
}
