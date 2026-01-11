{ pkgs, ... }:

{
  # System packages — X11 tools and desktop utilities
  environment.systemPackages = with pkgs; [
    # Window manager
    awesome

    # X11 core utilities (equivalents to: xorg xinit xserver-common xserver-xorg)
    xorg.xinit
    xorg.xorgserver      # X.Org X11 server - core display server
    xorg.xrandr          # Resize and rotate screen utility for multi-monitor setup
    xorg.xev             # Event viewer for X11
    xorg.xprop           # X property utility
    xorg.xwininfo        # Window information utility
    xorg.setxkbmap       # Set X keyboard mapping

    # Display and monitor management
    arandr             # GUI for xrandr (monitor configuration)
    # autorandr          # Automatic display configuration

    # Clipboard managers (xclip, xsel equivalents)
    xclip
    xsel

    # X11 automation tools (xdotool, wmctrl equivalents)
    xdotool            # Command-line X11 automation tool
    wmctrl             # Control window manager from command line

    # Screen locker (i3lock equivalent)
    i3lock

    # Notification daemon and utilities (zenity equivalent)
    zenity             # Display GTK dialogs from shell scripts
    libnotify          # Send desktop notifications
    # dunst              # Lightweight notification daemon

    # Suckless tools (suckless-tools equivalent: dmenu)
    dmenu
    rofi

    # Terminal emulator
    rxvt-unicode
    rxvt-unicode-emoji

    # System tray applications
    networkmanagerapplet   # NetworkManager applet (nm-applet)
    pasystray              # PulseAudio system tray

    # Screen color temperature adjustment (redshift-gtk equivalent)
    redshift

    # Appearance and theming
    arc-theme              # GTK theme (Arc Dark)
    numix-icon-theme       # Icon theme (Numix)
    adwaita-icon-theme     # Adwaita icon theme (includes cursor theme)
  ];

  # Enable X11 windowing system
  services.xserver = {
    enable = true;

    # Display manager configuration
    displayManager = {
      lightdm = {
        enable = true;
        greeters.gtk = {
          enable = true;
          # Theme configuration
          theme = {
            name = "Arc-Dark";
            package = pkgs.arc-theme;
          };
          iconTheme = {
            name = "Numix";
            package = pkgs.numix-icon-theme;
          };
          cursorTheme = {
            name = "Adwaita";
            package = pkgs.adwaita-icon-theme;
          };
          # Additional customization
          extraConfig = ''
            # Disable user background
            user-background=false
            # Hide the user list for privacy (optional)
            hide-user-image=true
            # Set indicators (optional: ~host;~spacer;~clock;~spacer;~session;~a11y;~power)
            indicators=~host;~spacer;~session;~power
          '';
        };
        # Optional: set a custom background image
        # background = "/path/to/your/wallpaper.jpg";
      };
      # Sleep time
      sessionCommands = ''
        xset s 3600 3600
        xset dpms 3600 3600 3600
      '';
    };

    # Awesome window manager
    windowManager.awesome = {
      enable = true;
      # luaModules = with pkgs.luaPackages; [
        # luarocks  # package manager for Lua modules
        # luadbi-mysql  # optional, for awesome-extra functionality
      # ];
    };
  };

  # Greetd display manager with tuigreet
  # https://github.com/NixOS/nixpkgs/blob/nixos-25.11/nixos/modules/services/display-managers/greetd.nix
  # services.greetd = {
  #   enable = true;
  #   settings.default_session = {
  #     command = "${pkgs.tuigreet}/bin/tuigreet --asterisks --greet-align left --time --cmd '${pkgs.awesome}/bin/awesome'";
  #     user = "greeter";
  #   };
  #   # Fix for systemd logs on top
  #   useTextGreeter = true;
  # };

  # Enable XDG portals for better desktop integration
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    config = {
      common = {
        default = [ "gtk" ];
      };
    };
  };

  # Systemd user services for X11 session
  systemd.user.services = {
    # NetworkManager Applet
    nm-applet = {
      description = "NetworkManager Applet";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.networkmanagerapplet}/bin/nm-applet";
        Restart = "on-failure";
      };
    };

    # PulseAudio System Tray
    pasystray = {
      description = "PulseAudio System Tray";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.pasystray}/bin/pasystray";
        Restart = "on-failure";
      };
    };

    # Redshift with GTK systray icon (screen color temperature)
    redshift = {
      description = "Redshift screen color temperature adjustment";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.redshift}/bin/redshift-gtk";  # -gtk variant runs in systray
        Restart = "on-failure";
      };
    };
  };

  # Environment variables for theming and X11
  environment.sessionVariables = {
    # GTK theming - Overrides the theme specified in config files at runtime
    # GTK_THEME = "Arc-Dark";

    # Icon theme - Overrides the theme specified in config files at runtime
    # XCURSOR_THEME = "Adwaita";
    # XCURSOR_SIZE = "24";

    # X11 session type
    XDG_SESSION_TYPE = "x11";
    XDG_CURRENT_DESKTOP = "Awesome";
  };

  # Update environment.etc section
  # Not used! The config in HOME is more logical!
  # environment.etc = {
  #   # GTK 2.0 settings
  #   "xdg/gtk-2.0/gtkrc".text = ''
  #     gtk-theme-name="Arc-Dark"
  #     gtk-icon-theme-name="Numix"
  #     gtk-cursor-theme-name="Adwaita"
  #     gtk-cursor-theme-size=24
  #     gtk-font-name="DejaVu Sans 11"
  #   '';

  #   # GTK 3.0 settings
  #   "gtk-3.0/settings.ini".text = ''
  #     [Settings]
  #     gtk-theme-name=Arc-Dark
  #     gtk-icon-theme-name=Numix
  #     gtk-cursor-theme-name=Adwaita
  #     gtk-cursor-theme-size=24
  #     gtk-font-name=DejaVu Sans 11
  #     gtk-application-prefer-dark-theme=1
  #   '';

  #   # GTK 4.0 settings
  #   "gtk-4.0/settings.ini".text = ''
  #     [Settings]
  #     gtk-theme-name=Arc-Dark
  #     gtk-icon-theme-name=Numix
  #     gtk-cursor-theme-name=Adwaita
  #     gtk-cursor-theme-size=24
  #     gtk-font-name=DejaVu Sans 11
  #     gtk-application-prefer-dark-theme=1
  #   '';
  # };

}
