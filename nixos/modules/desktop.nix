{ pkgs, ... }:

{
  # System packages — core userland tools and desktop utilities
  environment.systemPackages = with pkgs; [
    # Core user applications
    firefox                # Web browser
    # firefox-devedition
    wezterm                # Terminal emulator
    # rxvt-unicode         # Terminal emulator
    ghostty                # Terminal emulator
    xfce.thunar            # File manager
    xfce.thunar-volman
    xfce.thunar-archive-plugin
    xfce.thunar-media-tags-plugin
    thunderbird            # Mail and calendar
    signal-desktop         # Communication
    zathura                # PDF

    # Sway and Wayland ecosystem
    waybar                 # Status bar
    fuzzel                 # Application launcher
    grim                   # Screenshot tool
    slurp                  # Region selector
    wl-clipboard           # Clipboard integration
    wtype                  # For simulating key presses
    kanshi                 # Auto layout switching (multi-monitor)
    wev                    # xev for wayland
    swaynotificationcenter # Notification daemon for sway

    # Media and sound
    mpv                    # Media player
    yt-dlp                 # Media downloader
    pulseaudio             # Sound server
    pavucontrol            # Volume mixer GUI
    playerctl              # MPRIS control interface
    # moc                    # Music player
    termusic               # Music player
    drawio                 # Diagrams

    # System utilities
    xdg-utils              # Desktop integration
    gparted                # Partition manager
    gnome-disk-utility     # Disk management
    dconf-editor           # GTK configuration editor
    libnotify              # Notifications

    # Systray
    networkmanagerapplet   # get nm-applet
    pasystray              # Sound applet

    # Appearance and theming
    everforest-gtk-theme   # GTK theme
    adwaita-icon-theme     # Icon theme
    bibata-cursors         # Cursor theme

    # For Waybar. Gtk css interfer and force a with of 28px (which is huge)
    # to force a small size, fake a smaller gtk theme
    adapta-gtk-theme
  ];

  # Wayland and Sway
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true;
  };

  # https://github.com/NixOS/nixpkgs/blob/nixos-25.11/nixos/modules/services/display-managers/greetd.nix
  services.greetd = {
    enable = true;
    settings.default_session = {
      # command = "${pkgs.greetd.tuigreet}/bin/tuigreet --asterisks --greet-align left --time --cmd sway";
      command = "${pkgs.tuigreet}/bin/tuigreet --asterisks --greet-align left --time --cmd '${pkgs.sway}/bin/sway'";
      user = "greeter";
    };
    # Fix for systemd logs on top
    useTextGreeter = true;
  };

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
  # services.printing = {
  #   enable = true;
  #   drivers = with pkgs; [ hplip gutenprint ];
  # };
  # hardware.sane.enable = true;
  # hardware.bluetooth.enable = true;
  # services.blueman.enable = true;

  # Security
  # security.polkit.enable = true;
  security.pam.services.swaylock = {};

  # Hardware acceleration
  hardware.graphics.enable = true;

  # XDG portals
  # xdg.portal = {
  #   enable = true;
  #   wlr.enable = true;
  #   extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  #   # config = {
  #   #   common = {
  #   #     default = [ "gtk" ];
  #   #   };
  #   #   sway = {
  #   #     default = [ "wlr" "gtk" ];
  #   #   };
  #   # };
  # };

  # Enable D-Bus
  services.dbus.enable = true;

  # Systemd user services for sway ecosystem
  systemd.user.services = {
    waybar = {
      description = "Highly customizable Wayland bar for Sway and Wlroots based compositors";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.waybar}/bin/waybar";
        ExecReload = "${pkgs.coreutils}/bin/kill -SIGUSR2 $MAINPID";
        KillMode = "mixed";
      };
    };

    kanshi = {
      description = "Dynamic display configuration for Wayland";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.kanshi}/bin/kanshi";
      };
    };

    swaync = {
      description = "Simple notification daemon with a GUI built for Sway";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.swaynotificationcenter}/bin/swaync";
      };
    };

    nm-applet = {
      description = "NetworkManager Applet";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.networkmanagerapplet}/bin/nm-applet";
      };
    };
  };

  # System services needed for portals
  # systemd.user.services = {
  #   xdg-desktop-portal = {
  #     wantedBy = [ "graphical-session.target" ];
  #     wants = [ "xdg-desktop-portal-wlr.service" ];
  #     before = [ "xdg-desktop-portal-wlr.service" ];
  #   };
  #   xdg-desktop-portal-wlr = {
  #     wantedBy = [ "graphical-session.target" ];
  #   };
  # };

  # Environment variables (system-wide, single-user system)
  environment.sessionVariables = {
    # NIXOS_OZONE_WL = "1";
    # MOZ_ENABLE_WAYLAND = "1";
    # QT_QPA_PLATFORM = "wayland";
    # SDL_VIDEODRIVER = "wayland";
    # _JAVA_AWT_WM_NONREPARENTING = "1";
    GTK_THEME = "Everforest-Dark-B-LB";
    XCURSOR_THEME = "Bibata-Original-Classic";
    XCURSOR_SIZE = "12";
    # XDG Portal variables
    # XDG_CURRENT_DESKTOP = "sway";
    # XDG_SESSION_DESKTOP = "sway";
    # XDG_SESSION_TYPE = "wayland";
  };

  environment.variables.GTK_THEME = "Everforest-Dark-B-LB";
  environment.variables.GTK_ICON_THEME = "Everforest-Dark";

  # System-wide GTK configuration
  environment.etc = {
    "gtk-3.0/settings.ini".text = ''
      [Settings]
      gtk-theme-name=Everforest-Dark-B-LB
      gtk-icon-theme-name=Adwaita
      gtk-cursor-theme-name=Bibata-Original-Classic
      gtk-cursor-theme-size=12
      gtk-font-name=DejaVu Sans 11
      gtk-application-prefer-dark-theme=1
    '';

    "gtk-4.0/settings.ini".text = ''
      [Settings]
      gtk-theme-name=Everforest-Dark-B-LB
      gtk-icon-theme-name=Adwaita
      gtk-cursor-theme-name=Bibata-Original-Classic
      gtk-cursor-theme-size=12
      gtk-font-name=DejaVu Sans 11
      gtk-application-prefer-dark-theme=1
    '';
  };

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
