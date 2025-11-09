{ pkgs, ... }:

{

  home.packages = with pkgs; [
    everforest-gtk-theme
    adwaita-icon-theme
  ];

  home.sessionVariables = {
    XCURSOR_THEME = "Adwaita";
    XCURSOR_SIZE = "24";
  };
  home.pointerCursor = {
    gtk.enable = true;
    # x11.enable = true;
    package = pkgs.adwaita-icon-theme;
    name = "Adwaita";
    size = 24;
  };

  gtk = {
    enable = true;
    theme = {
      name = "Everforest-Dark-B-LB";
      package = pkgs.everforest-gtk-theme;
    };
    iconTheme = {
      name = "Everforest-Dark";
      package = pkgs.everforest-gtk-theme;
    };
    cursorTheme = {
      name = "Adwaita";
      size = 24;
    };
  };

}

