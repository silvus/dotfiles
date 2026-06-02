
{ pkgs, ... }:

{
  # System packages — core userland tools and desktop utilities
  environment.systemPackages = with pkgs; [
    # Niri, Noctalia and Wayland ecosystem
    niri
    noctalia-shell
    xwayland-satellite

    # fuzzel                 # Application launcher
    # grim                   # Screenshot tool
    # slurp                  # Region selector
    # wl-clipboard           # Clipboard integration
    # wtype                  # For simulating key presses
    # kanshi                 # Auto layout switching (multi-monitor)
    # wev                    # xev for wayland
    # swaynotificationcenter # Notification daemon for sway

    # Systray
    # networkmanagerapplet   # get nm-applet
    # pasystray              # Sound applet

    # Appearance and theming
    # everforest-gtk-theme   # GTK theme
    # adwaita-icon-theme     # Icon theme
    # bibata-cursors         # Cursor theme

    # For Waybar. Gtk css interfer and force a with of 28px (which is huge)
    # to force a small size, fake a smaller gtk theme
    # adapta-gtk-theme
  ];

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-wlr
    ];
  };

  # Niri
  programs.niri.enable = true;

  # Noctalia
  # programs.noctalia-shell.enable = true;

  # X11 compatibility
  programs.xwayland.enable = true;

  services.xserver = {
    enable = true;

    # displayManager.lightdm.enable = true;
    # displayManager.lightdm.greeters.gtk.enable = true;
  };


  services.dbus.enable = true;
  security.polkit.enable = true;

}
