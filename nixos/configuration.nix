# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix

      # User and roles creation
      ./user.nix

      # Localisation
      ./locale.nix
    ];

  # Bootloader.
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";
  boot.loader.grub.useOSProber = true;
  # Grub timeout
  boot.loader.timeout = 1;

  # Configure keymap in X11
  services.xserver = {
    enable = true;
    autorun = true;

    # dpi = 96;
    # windowManager.awesome.enable = true;
    desktopManager.xterm.enable = false;

    windowManager.awesome.enable = true;
    displayManager.defaultSession = "none+awesome";

    #windowManager.i3.enable = true;
    #windowManager.i3.extraPackages = with pkgs; [
    #  dmenu
    #  i3lock
    #  i3status
    #];
    #displayManager.defaultSession = "none+i3";

    displayManager.sddm.enable = false;
    displayManager.lightdm = {
      enable = true;
      greeter.enable = false;
      autoLogin = {
        enable = true;
        user = "silvus";
      };
    };


    # Enable the use of startx
    displayManager.startx.enable = true;
    # TODO: set custom resolution
    # displayManager.setupCommands = "xrandr --output Virtual1 --primary --mode 1440x900 --pos 0x0 --rotate normal";
    # resolutions = lib.mkOverride 9 { x = 1440; y = 900; };
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    bashInteractive
    bash-completion
    fishPlugins.tide
    fishPlugins.fzf-fish
    vim
    emacs
    wget
    curl
    git
    tmux
    which
    most
    htop
    dfc
    ncdu
    fzf
    gawk
    python3

    mosh
    syncthing

    unzip
    unrar
    p7zip

    xdotool
    xclip
    xsel

    firefox
    chromium
    vscodium
    obsidian

    rxvt-unicode-emoji
    alacritty

    tdrop
    dmenu
    rofi

    autorandr
    mpv
    lxappearance
    pavucontrol
    qjackctl
  ];
  programs.vim.defaultEditor = true;
  programs.nm-applet.enable = true;
  programs.steam.enable = true;

  programs.fish.enable = true;
  environment.shells = with pkgs; [ fish ];

  # Save volume state on shutdown (Alsa)
  # sound.enable = true;
  # Enable pulsaudio
  # hardware.pulseaudio.enable = true;
  # Enable Pipewire
  security.rtkit.enable = true;
  services.pipewire.enable = true;
  services.pipewire.alsa.enable = true;
  services.pipewire.pulse.enable = true;
  services.pipewire.jack.enable = true;

  fonts.fonts = with pkgs; [
    dejavu_fonts
    liberation_ttf
    inconsolata
    terminus_font
    noto-fonts
    fira-code
    hack-font
    symbola
  ];


  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };


  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = true;

  networking.hostName = "nixos-testing";
  # networking.wireless.enable = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?

}
