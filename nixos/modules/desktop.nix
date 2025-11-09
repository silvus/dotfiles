{ pkgs, ... }:

{
  # Wayland and Sway
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  };

  services.greetd = {
    enable = true;
    settings.default_session = {
      command = "${pkgs.greetd.tuigreet}/bin/tuigreet --asterisks --greet-align left --time --cmd '${pkgs.sway}/bin/sway'";
      user = "greeter";
    };
  };

  # Audio
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  # Desktop support
  programs.dconf.enable = true;
  # services.udisks2.enable = true;
  # services.tumbler.enable = true;
  services.printing = {
    enable = true;
    drivers = with pkgs; [ hplip gutenprint ];
  };
  hardware.sane.enable = true;
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Security
  security.polkit.enable = true;
  security.pam.services.swaylock = {};

  # Hardware acceleration
  hardware.graphics.enable = true;

  # XDG portals
  xdg.portal = {
    enable = true;
    wlr.enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };

  # Environment variables (system-wide, single-user system)
  environment.sessionVariables = {
    NIXOS_OZONE_WL = "1";
    MOZ_ENABLE_WAYLAND = "1";
    QT_QPA_PLATFORM = "wayland";
    SDL_VIDEODRIVER = "wayland";
    _JAVA_AWT_WM_NONREPARENTING = "1";
    GTK_THEME = "Everforest-Dark-B-LB";
    XCURSOR_THEME = "Adwaita";
    XCURSOR_SIZE = "24";
  };

  # System-wide GTK configuration
  environment.etc = {
    "gtk-3.0/settings.ini".text = ''
      [Settings]
      gtk-theme-name=Everforest-Dark-B-LB
      gtk-icon-theme-name=Adwaita
      gtk-cursor-theme-name=Adwaita
      gtk-cursor-theme-size=24
      gtk-font-name=DejaVu Sans 11
      gtk-application-prefer-dark-theme=1
    '';

    "gtk-4.0/settings.ini".text = ''
      [Settings]
      gtk-theme-name=Everforest-Dark-B-LB
      gtk-icon-theme-name=Adwaita
      gtk-cursor-theme-name=Adwaita
      gtk-cursor-theme-size=24
      gtk-font-name=DejaVu Sans 11
      gtk-application-prefer-dark-theme=1
    '';
  };

  # Fonts
  fonts = {
    packages = with pkgs; [
      liberation_ttf
      dejavu_fonts
      noto-fonts
      noto-fonts-emoji
      font-awesome
    ] ++ (with pkgs.nerd-fonts; [
      hack
      dejavu-sans-mono
      jetbrains-mono
    ]);

    fontconfig.defaultFonts = {
      serif = [ "DejaVu Serif" ];
      sansSerif = [ "DejaVu Sans" ];
      monospace = [ "DejaVu Sans Mono" ];
      emoji = [ "Noto Color Emoji" ];
    };
  };

  # System packages — core userland tools and desktop utilities
  environment.systemPackages = with pkgs; [
    # Core user applications
    firefox                # Web browser
    wezterm                # Terminal emulator
    thunar                 # File manager

    # Sway and Wayland ecosystem
    waybar                 # Status bar
    fuzzel                 # Application launcher
    grim                   # Screenshot tool
    slurp                  # Region selector
    wl-clipboard           # Clipboard integration
    kanshi                 # Auto layout switching (multi-monitor)

    # Media and sound
    mpv                    # Media player
    pavucontrol            # Volume mixer GUI
    playerctl              # MPRIS control interface

    # System utilities
    gparted                # Partition manager
    gnome-disk-utility     # Disk management
    dconf-editor           # GTK configuration editor

    # Appearance and theming
    everforest-gtk-theme   # GTK theme
    adwaita-icon-theme     # Icon theme
  ];
}
