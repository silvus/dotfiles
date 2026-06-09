
{ pkgs, ... }:

{
  # system packages — core userland tools and desktop utilities
  environment.systemPackages = with pkgs; [
    niri
    waybar
    xwayland

    fuzzel                 # application launcher
    # grim                   # screenshot tool
    # slurp                  # region selector
    wl-clipboard           # clipboard integration
    wtype                  # for simulating key presses
    kanshi                 # auto layout switching (multi-monitor)
    wev                    # xev for wayland
    swaynotificationcenter # notification daemon for sway
    swaylock               # lock session

    # systray
    networkmanagerapplet   # get nm-applet
    pasystray              # sound applet

    # Appearance and theming
    everforest-gtk-theme   # gtk theme
    adwaita-icon-theme     # Adwaita icon theme (includes cursor theme)

    glib                   # To get gsettings (wayland specific)
  ];

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-wlr
    ];
  };

  # niri
  programs.niri.enable = true;

  # x11 compatibility
  programs.xwayland.enable = true;

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
            name = "Everforest-Dark";
            package = pkgs.everforest-gtk-theme;
          };
          iconTheme = {
            name = "Everforest-Dark";
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
      };
    };
  };

  # Systemd user services for niri ecosystem
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

  services.dbus.enable = true;
  security.polkit.enable = true;

  # not compatible with `programs.ssh.startagent`
  services.gnome.gcr-ssh-agent.enable = false;
}
