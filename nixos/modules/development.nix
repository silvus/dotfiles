{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # Core development tools
    gnumake
    cmake
    pkg-config
    gcc
    clang

    # Formatter
    dprint
    # dprint-plugins.dprint-plugin-toml
    # dprint-plugins.dprint-plugin-json
    # dprint-plugins.g-plane-malva
    # dprint-plugins.dprint-plugin-ruff
    # dprint-plugins.g-plane-markup_fmt
    # dprint-plugins.g-plane-pretty_yaml
    dprint-plugins.dprint-plugin-markdown

    # Markdown
    marksman
    markdown-oxide
    # harper

    lua
    # lua lsp
    lua-language-server
    # lua formatter
    stylua

    python3
    # python lsp
    ty
    ruff
    python313Packages.ruff

    rustc
    cargo
    # rust lsp
    rust-analyzer
    # rustup component add rust-analyzer
    rustfmt
    # rustup

    # nix lsp
    nil
    # nix formatter
    nixfmt

    # bash lsp
    bash-language-server
    # bash formatter
    shfmt

    # JS
    # bun

    # Utilities
    lsof
    # iftop
    # nethogs
    dig

    # Static generator
    # zola

    # Android
    # NIXPKGS_ALLOW_UNFREE=1 nix-shell -p androidenv.androidPkgs.platform-tools
    # android-tools
  ];

}
