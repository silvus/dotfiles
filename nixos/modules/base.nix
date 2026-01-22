{ pkgs, ... }:

{
  # Base system configuration shared by all hosts

  # Bootloader
  boot.loader = {
    timeout = 1;
    efi.canTouchEfiVariables = true;
    systemd-boot = {
      enable = true;
      configurationLimit = 9;
    };
  };
  # optional, but ensures rpc-statsd is running for on demand mounting
  boot.supportedFilesystems = [ "nfs" ];

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone
  time.timeZone = "Europe/Paris";

  # Internationalization
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocales = [
      "en_US.UTF-8/UTF-8"
      "en_GB.UTF-8/UTF-8"
      "fr_FR.UTF-8/UTF-8"
    ];
    extraLocaleSettings = {
      LANG = "en_US.UTF-8"; # default locale for all the LC_* variables that are not explicitly set
      # LANGUAGE = ""; # fallback locales
      # LC_ALL = ""; # override LANG and all the other LC_* variables
      LC_IDENTIFICATION = "en_US.UTF-8"; # metadata for the locale
      LC_MESSAGES = "en_US.UTF-8"; # sets the language in which messages are displayed
      LC_COLLATE = "en_US.UTF-8"; # Collation rules used for sorting and regular expressions
      LC_CTYPE = "en_US.UTF8"; # if change a text to uppercase

      LC_TIME = "fr_FR.UTF-8"; # date and time format
      LC_ADDRESS = "fr_FR.UTF-8"; # postal addresses format
      LC_MEASUREMENT = "fr_FR.UTF-8"; # settings relating to the measurement system in the locale
      LC_MONETARY = "fr_FR.UTF-8"; # formatting used for monetary-related numeric values
      LC_NAME = "fr_FR.UTF-8"; # formats used to address persons, general salutation for men or women
      LC_NUMERIC = "fr_FR.UTF-8"; # formatting rules used for nonmonetary numeric, like the thousands separator
      LC_PAPER = "fr_FR.UTF-8"; # dimensions of the standard paper size
      LC_TELEPHONE = "fr_FR.UTF-8"; # formats to be used with telephone services
    };
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Nix configuration
  nix = {
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      warn-dirty = false;
      auto-optimise-store = true;
      # trusted-users = [ "root" "@wheel" ];
    };

    # Garbage collection
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };

  # Define user account
  users.users.silvus = {
    isNormalUser = true;
    description = "Silvus";
    extraGroups = [
      "networkmanager"
      "wheel"
      "input"
      "audio"
      "video"
      "storage"
      "optical"
      "scanner"
      "lp"
    ];
    shell = pkgs.fish;
  };

  # Base system packages (minimal, essential only)
  environment.systemPackages = with pkgs; [
    # Editor
    vim
    neovim
    nano
    helix
    emacs
    emacsPackages.doom

    # Network
    wget
    curl
    mosh
    nfs-utils

    # Shell and terminal tools
    fish
    tmux

    # Basic utilities
    coreutils  # Provides date command
    which
    most
    file
    tree
    fzf
    bat
    fd
    jq
    gawk
    unzip
    unrar
    git
    lazygit
    yazi

    # Scripts
    python3

    # Security
    gnupg
    # For Yubikey
    # pinentry-curses
    # openssh
    # libfido2

    # System information
    ncdu
    dfc
    htop
    btop

  ];

  # Enable essential programs
  programs = {
    vim = {
      enable = true;
      defaultEditor = true;
    };
    fish.enable = true;
    git.enable = true;
  };
  environment.variables.EDITOR = "vim";

  # Set default shell
  environment.shells = with pkgs; [ fish bash ];

  # Enable OpenSSH daemon
  services.openssh = {
    enable = true;
    # settings = {
    #   PasswordAuthentication = false;
    #   PermitRootLogin = "no";
    #   X11Forwarding = false;
    # };
    # openFirewall = true;
  };


  # Basic security settings
  # security = {
  #   sudo = {
  #     enable = true;
  #     wheelNeedsPassword = true;
  #   };
  #   polkit.enable = true;
  # };

  # System state version
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.11";
}
