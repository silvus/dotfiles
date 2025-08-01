{ pkgs, ... }:

{

  home.packages = with pkgs; [
    # Fuzzy finder
    fzf
    # Cat alternative
    bat
    # Find alternative
    fd
    # Grep alternative
    ripgrep
    # Code statistics
    tokei
    # Editor
    neovim
    # File manager
    yazi
    # Git TUI
    lazygit

    # Render graphs in Neovim
    # mermaid-cli  # mmdc
    # imagemagick
    # luajitPackages.magick

    # draw plans
    drawio

    # music
    tauon

    # Fonts
    terminus_font
    nerd-fonts.dejavu-sans-mono
    nerd-fonts.fira-mono
    nerd-fonts.hack
  ];
}

