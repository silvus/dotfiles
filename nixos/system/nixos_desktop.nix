{ pkgs, ... }:

{
  programs.dconf.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    everforest-gtk-theme
    libnotify
    firefox
    mpv
    moc
    pulseaudio
    playerctl
    rxvt-unicode
    wezterm
    drawio
    lazygit
    pragtical

    dconf

    grim # screenshot
    slurp # screenshot region
    wl-clipboard
    wev # xev for wayland

    wezterm

    pavucontrol
    kanshi
    fuzzel
    ghostty
    sway
    waybar
    # gtk css interfer and force a with of 28px (which is huge)
    # to force a small size, fake a smaller gtk theme
    adapta-gtk-theme
  ];

  environment.variables.GTK_THEME = "Everforest-Dark-B-LB";
  environment.variables.GTK_ICON_THEME = "Everforest-Dark";

  # https://nixos.wiki/wiki/Fonts
  fonts.packages = with pkgs; [
    nerd-fonts.hack
    nerd-fonts.dejavu-sans-mono
    nerd-fonts.inconsolata
    nerd-fonts.liberation
    nerd-fonts.ubuntu-mono
    nerd-fonts.jetbrains-mono
    noto-fonts
    noto-fonts-emoji
    font-awesome
  ];
}

