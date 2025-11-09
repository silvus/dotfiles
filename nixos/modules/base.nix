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
    };
  };

  # Console keymap
  console = {
    keyMap = lib.mkDefault "us";
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
      trusted-users = [ "root" "@wheel" ];
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
      "incus-admin"
    ];
    shell = pkgs.fish;
  };

  # Base system packages (minimal, essential only)
  environment.systemPackages = with pkgs; [
    # Essential system tools
    vim
    nano
    wget
    curl

    # Shell and terminal tools
    fish

    # Basic utilities
    which
    file

    # Security
    gnupg

    # System information
    lshw
    usbutils
    pciutils

    # Containers
    incus
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

  # Set default shell
  environment.shells = with pkgs; [ fish bash ];

  # Enable Incus containers
  virtualisation.incus = {
    enable = true;
    ui.enable = true;
  };

  # Enable nftables for Incus
  networking.nftables.enable = true;



  # Enable OpenSSH daemon
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
      X11Forwarding = false;
    };
    openFirewall = true;
  };

  # Basic security settings
  security = {
    sudo = {
      enable = true;
      wheelNeedsPassword = true;
    };
    polkit.enable = true;
  };

  # System state version
  system.stateVersion = "25.05";
}
