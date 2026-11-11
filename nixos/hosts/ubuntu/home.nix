# Ubuntu (GNOME) - Home Manager standalone
{
  config,
  pkgs,
  lib,
  mdorg,
  ...
}:

{
  home = {
    # Override in custom/home.nix (not versioned), e.g. on the work laptop
    username = lib.mkDefault "silvus";
    homeDirectory = lib.mkDefault "/home/${config.home.username}";
    stateVersion = "26.05";
  };

  # Enable home-manager to manage itself
  programs.home-manager.enable = true;

  # Non-NixOS integration (XDG_DATA_DIRS for .desktop files, locales, ...)
  # Also enables targets.genericLinux.gpu: GL/Vulkan drivers for nix GUI apps (ghostty, zed...)
  # After the first switch, run once: sudo $(which non-nixos-gpu-setup)
  targets.genericLinux.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Nix configuration (same as base.nix)
  nix = {
    package = pkgs.nix;
    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      warn-dirty = false;
      # 500 MiB
      download-buffer-size = 524288000;
      # Keep build deps of live derivations (no re-download after GC)
      keep-outputs = true;
    };
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
  };

  home.packages = with pkgs; [
    # --- base.nix ---
    # Editor
    # vim
    neovim
    helix

    # Network
    # wget
    # curl
    # mosh
    # Shell and terminal tools
    # tmux
    # Basic utilities
    # clang
    # gnumake
    # most
    # file
    # tree
    fzf
    bat
    fd
    ripgrep
    jq

    # gawk
    # zip
    # unzip
    # unrar
    # p7zip
    # git
    lazygit
    yazi
    # python3
    # Security
    # gnupg
    # age
    # System information
    # ncdu
    # dfc
    # htop
    # btop

    # --- development ---
    # cmake
    # pkg-config
    dprint
    dprint-plugins.dprint-plugin-markdown
    # lsof
    # dig
    # tokei
    # Python
    ty
    ruff
    # Nix
    nil
    nixd
    nixfmt
    # Lua
    lua
    lua-language-server
    stylua
    # Bash
    bash-language-server
    shfmt
    # Markdown
    marksman
    markdown-oxide

    # --- desktop_base.nix ---
    ghostty # Terminal emulator
    wezterm # Terminal emulator
    # mpv # Media player
    # unstable.yt-dlp # Media downloader
    # unstable.deno # JS runtime required by yt-dlp
    playerctl # MPRIS control interface
    # moc # Music player
    # mot.packages.${pkgs.stdenv.hostPlatform.system}.default # music on terminal
    # monolith # Website archiver
    # bc # Calculator
    rink # Calculator

    # libnotify # Notifications
    # keepassxc # Password manager
    # vscodium # Editor
    zed-editor # Editor

    # --- mdorg.nix ---
    mdorg.packages.${pkgs.stdenv.hostPlatform.system}.default

    # --- desktop_office.nix ---
    # thunderbird # Mail and calendar
    # signal-desktop # Communication
    # libreoffice # Office suite
    # gimp # Painting
    drawio # Diagrams
    zathura # PDF
    # translate-shell # Command-line translator
    # typst
    # typstyle

    # jetbrains.phpstorm
    teams-for-linux
    # teams


    # --- Fonts ---
    dejavu_fonts
    libertinus # For Typst
    noto-fonts
    noto-fonts-color-emoji
    font-awesome
    terminus_font_ttf # For terminals
    nerd-fonts.hack
    nerd-fonts.dejavu-sans-mono
    nerd-fonts.inconsolata
    nerd-fonts.jetbrains-mono
    nerd-fonts.liberation
  ];

  # Make nix fonts visible to fontconfig
  fonts.fontconfig = {
    enable = true;
    defaultFonts = {
      serif = [ "DejaVu Serif" ];
      sansSerif = [ "DejaVu Sans" ];
      monospace = [ "DejaVu Sans Mono" ];
      emoji = [ "Noto Color Emoji" ];
    };
  };

  # Load flake dev shells on cd (direnv.toml comes from the dotfiles,
  # the fish hook is in config.fish)
  # programs.direnv = {
  #   enable = true;
  #   nix-direnv.enable = true;
  # };

  # Syncthing as a user service (same GUI address as syncthing.nix)
  # TODO: hum?
  services.syncthing = {
    enable = true;
    guiAddress = "0.0.0.0:5001";
  };
}
