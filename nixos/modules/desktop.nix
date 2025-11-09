{ config, pkgs, lib, ... }:

with lib;

{
  config = {
    # Enable X11 and Wayland support
    services.xserver.enable = true;

    # Audio support
    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    # Enable dconf for GTK configuration
    programs.dconf.enable = true;

    # Sway window manager
    programs.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
    };

    # Display manager
    services.greetd = {
      enable = true;
      settings = {
        default_session = {
          command = "${pkgs.greetd.tuigreet}/bin/tuigreet --asterisks --greet-align left --time --cmd sway";
          user = "greeter";
        };
      };
    };

    # Desktop packages
    environment.systemPackages = with pkgs; [
      # Core desktop
      firefox
      wezterm
      nautilus

      # Sway ecosystem
      sway
      swaylock
      swayidle
      swaybg
      waybar
      fuzzel
      grim
      slurp
      wl-clipboard
      kanshi

      # Media
      mpv
      pavucontrol
      playerctl

      # System tools
      gparted
      gnome-disk-utility
      dconf-editor

      # Themes and fonts
      everforest-gtk-theme
      adwaita-icon-theme
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

    # Font configuration
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

      fontconfig = {
        enable = true;
        defaultFonts = {
          serif = [ "DejaVu Serif" ];
          sansSerif = [ "DejaVu Sans" ];
          monospace = [ "DejaVu Sans Mono" ];
          emoji = [ "Noto Color Emoji" ];
        };
      };
    };

    # Security for desktop
    security.polkit.enable = true;
    security.pam.services.swaylock = {};

    # Hardware acceleration
    hardware.graphics = {
      enable = true;
      enable32Bit = true;
    };

    # XDG desktop portal
    xdg.portal = {
      enable = true;
      wlr.enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-gtk
      ];
    };

    # Wayland environment variables
    environment.sessionVariables = {
      NIXOS_OZONE_WL = "1";
      MOZ_ENABLE_WAYLAND = "1";
      QT_QPA_PLATFORM = "wayland";
      SDL_VIDEODRIVER = "wayland";
      _JAVA_AWT_WM_NONREPARENTING = "1";
    };

    # GTK theming
    environment.sessionVariables = {
      GTK_THEME = "Everforest-Dark-B-LB";
      XCURSOR_THEME = "Adwaita";
      XCURSOR_SIZE = "24";
    };

    environment.etc = {
      "gtk-3.0/settings.ini".text = ''
        [Settings]
        gtk-theme-name=Everforest-Dark-B-LB
        gtk-icon-theme-name=Everforest-Dark
        gtk-cursor-theme-name=Adwaita
        gtk-cursor-theme-size=24
        gtk-font-name=DejaVu Sans 11
        gtk-application-prefer-dark-theme=1
      '';

      "gtk-4.0/settings.ini".text = ''
        [Settings]
        gtk-theme-name=Everforest-Dark-B-LB
        gtk-icon-theme-name=Everforest-Dark
        gtk-cursor-theme-name=Adwaita
        gtk-cursor-theme-size=24
        gtk-font-name=DejaVu Sans 11
        gtk-application-prefer-dark-theme=1
      '';
    };

    # Desktop services
    services.udisks2.enable = true;
    services.tumbler.enable = true;
    services.printing.enable = true;
    services.printing.drivers = with pkgs; [ hplip gutenprint ];
    hardware.sane.enable = true;
    hardware.bluetooth.enable = true;
    services.blueman.enable = true;
    services.geoclue2.enable = true;
  };
}
