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

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Paris";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "fr_FR.UTF-8";
    LC_IDENTIFICATION = "fr_FR.UTF-8";
    LC_MEASUREMENT = "fr_FR.UTF-8";
    LC_MONETARY = "fr_FR.UTF-8";
    LC_NAME = "fr_FR.UTF-8";
    LC_NUMERIC = "fr_FR.UTF-8";
    LC_PAPER = "fr_FR.UTF-8";
    LC_TELEPHONE = "fr_FR.UTF-8";
    LC_TIME = "fr_FR.UTF-8";
  };

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.silvus = {
    isNormalUser = true;
    description = "Silvus";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [];
    shell = pkgs.fish;
  };

# home-manager.users.silvus = { pkgs, ... }: {
#   home.packages = [ pkgs.atool pkgs.httpie ];
#  programs.bash.enable = true;

  # This value determines the Home Manager release that your configuration is 
  # compatible with. This helps avoid breakage when a new Home Manager release 
  # introduces backwards incompatible changes. 
  #
  # You should not change this value, even if you update Home Manager. If you do 
  # want to update the value, then make sure to first check the Home Manager 
  # release notes. 
#  home.stateVersion = "24.11"; # Please read the comment before changing. 

#};

  # home-manager.useUserPackages = true;
  # home-manager.useGlobalPkgs = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
    wget
    curl
    fish
    git
    python3
    tmux
    dfc
    ncdu
    which
    most
    htop
    btop
    fzf
    bat
    fd
    gawk
    mosh
    unzip
    unrar
    firefox
    mpv
    rxvt-unicode
    wezterm

    dconf
    grim
    mako
    slurp
    sway
    wl-clipboard

    # wallust
    everforest-gtk-theme
    adwaita-icon-theme
  ];

  programs.vim.enable = true;
  programs.vim.defaultEditor = true;
  # programs.hyprland.enable = true;
  programs.fish.enable = true;
  environment.shells = with pkgs; [ fish ];
  environment.variables.EDITOR = "vim";
  environment.variables.GTK_THEME = "Everforest-Dark-B-LB";
  environment.variables.GTK_ICON_THEME = "Everforest-Dark";

  # https://nixos.wiki/wiki/Fonts
  fonts.packages = with pkgs; [
    nerd-fonts.hack
    nerd-fonts.dejavu-sans-mono
    nerd-fonts.inconsolata
    nerd-fonts.liberation
    nerd-fonts.ubuntu-mono
    nerd-fonts.jetbrains-mono
    noto-fonts
    noto-fonts-emoji
    font-awesome
  ];

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

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?

}
