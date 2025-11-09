{ pkgs, lib, ... }:

{
  # Base system configuration shared by all hosts

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone
  time.timeZone = "Europe/Paris";

  # Internationalization
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "fr_FR.UTF-8";
      LC_IDENTIFICATION = "fr_FR.UTF-8";
      LC_MEASUREMENT = "fr_FR.UTF-8";
      LC_MONETARY = "fr_FR.UTF-8";
      LC_NAME = "fr_FR.UTF-8";
      LC_NUMERIC = "fr_FR.UTF-8";
      LC_PAPER = "fr_FR.UTF-8";
      LC_TELEPHONE = "fr_FR.UTF-8";
      LC_TIME = "fr_FR.UTF-8";
      LC_CTYPE = "fr_FR.UTF8";
      LC_MESSAGES = "fr_FR.UTF-8";
      LC_COLLATE = "fr_FR.UTF-8";
    };
  };

  # Console keymap
  console = {
    # keyMap = lib.mkDefault "us";
    keyMap = "us";
    useXkbConfig = true;
  };

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    variant = "";
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
    nano
    helix

    # Network
    wget
    curl
    mosh

    # Shell and terminal tools
    fish
    tmux

    # Basic utilities
    which
    most
    file
    git
    fzf
    bat
    fd
    jq
    gawk
    unzip
    unrar

    # Scripts
    python3

    # Security
    gnupg

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
  system.stateVersion = "25.05";
}
