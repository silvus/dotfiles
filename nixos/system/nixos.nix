{ pkgs, ... }:

{
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

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.warn-dirty = false;

  # Deduplicate store files
  nix.settings.auto-optimise-store = true;

  # Keep store blobs for old generations up to 30 days
  # nix.gc.options = "--delete-older-than 30d";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.silvus = {
    isNormalUser = true;
    description = "Silvus";
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
    # packages = with pkgs; [ ];
    shell = pkgs.fish;
  };

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
    jq
    gawk
    mosh
    unzip
    unrar
    gnupg
    syncthing
  ];

  programs.vim.enable = true;
  programs.vim.defaultEditor = true;
  programs.fish.enable = true;
  environment.shells = with pkgs; [ fish ];
  environment.variables.EDITOR = "vim";

  # https://nixos.wiki/wiki/Fonts
  # fonts.packages = with pkgs; [
  #   nerd-fonts.hack
  #   nerd-fonts.dejavu-sans-mono
  #   noto-fonts
  #   noto-fonts-emoji
  #   font-awesome
  # ];

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
  system.stateVersion = "25.05"; # Did you read the comment?
}

