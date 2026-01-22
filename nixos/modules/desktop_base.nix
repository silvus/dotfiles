{ pkgs, ... }:

{
  # System packages — core userland tools and desktop utilities
  environment.systemPackages = with pkgs; [
    # Core user applications
    firefox                # Web browser
    firefox-devedition
    chromium

    wezterm                # Terminal emulator
    # rxvt-unicode         # Terminal emulator
    ghostty                # Terminal emulator

    xfce.thunar            # File manager
    xfce.thunar-volman
    xfce.thunar-archive-plugin
    xfce.thunar-media-tags-plugin

    # Media and sound
    mpv                    # Media player
    yt-dlp                 # Media downloader
    spotdl                 # Music downloader
    alsa-utils             # Sound utility
    pulseaudio             # Sound server
    pavucontrol            # Volume mixer GUI
    playerctl              # MPRIS control interface
    moc                    # Music player
    # termusic               # Music player

    # System utilities
    xdg-utils              # Desktop integration
    gparted                # Partition manager
    gnome-disk-utility     # Disk management
    # dconf-editor           # GTK configuration editor
    libnotify              # Notifications
    ksnip                  # Screenshots
    lxappearance           # GTK theme switcher GUI
    keepassxc              # Password manager
    blink1-tool            # Command line client for the blink(1) notification light

    # Code
    vscodium               # Editor

    # Office
    thunderbird            # Mail and calendar
    signal-desktop         # Communication
    libreoffice            # Office suite
    simple-scan            # Document scanner
    gimp                   # Painting
    drawio                 # Diagrams
    zathura                # PDF
    cheese                 # Webcam testing
    mediaelch              # TVshow manager

    # 3D
    # bambu-studio # 2026-01-02 Cannot login
    orca-slicer
    freecad
    # blender

  ];

  # Reduce systemd timeout
  systemd.settings.Manager = {
    DefaultTimeoutStartSec = "10s";
    DefaultTimeoutStopSec = "10s";
  };
  systemd.user.extraConfig = ''
    DefaultTimeoutStartSec=10s
    DefaultTimeoutStopSec=10s
  '';

  # Disable NetworkManager from starting at boot
  systemd.services.NetworkManager-wait-online.enable = false;

  # Audio
  # security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    # alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Desktop support
  programs.dconf.enable = true;
  # services.udisks2.enable = true;
  # services.tumbler.enable = true;
  # hardware.sane.enable = true;
  # hardware.bluetooth.enable = true;
  # services.blueman.enable = true;

  # Enable libinput for touchpad and input device support
  services.libinput.enable = true;

  # Hardware acceleration
  hardware.graphics.enable = true;

  # Enable D-Bus
  services.dbus.enable = true;

  # Fonts
  # https://nixos.wiki/wiki/Fonts
  fonts = {
    packages = with pkgs; [
      dejavu_fonts
      noto-fonts
      noto-fonts-color-emoji
      font-awesome
    ] ++ (with pkgs.nerd-fonts; [
      hack
      dejavu-sans-mono
      inconsolata
      jetbrains-mono
      liberation
    ]);

    fontconfig.defaultFonts = {
      serif = [ "DejaVu Serif" ];
      sansSerif = [ "DejaVu Sans" ];
      monospace = [ "DejaVu Sans Mono" ];
      emoji = [ "Noto Color Emoji" ];
    };
  };

  # Yubikey
  services.udev.packages = [ pkgs.yubikey-personalization ];
  services.pcscd.enable = true;
  # hardware.fido2.enable = true;
}
