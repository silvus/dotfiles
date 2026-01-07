{ pkgs, lib, ... }:

with lib;

{
  environment.systemPackages = with pkgs; [
    # Core development tools
    git
    lazygit
    gnumake
    cmake
    pkg-config
    gcc
    clang

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
    pyright
    # ty
    # python formatter
    ruff

    rustc
    cargo
    # rust lsp
    # rust-analyzer
    # rustup component add rust-analyzer

    # nix lsp
    nil
    # nix formatter
    nixfmt-rfc-style

    # bash lsp
    bash-language-server
    # bash formatter
    shfmt

    # JS
    bun

    # GUI Editors
    # vscodium

    # Utilities
    jq
    curl
    wget
    lsof
    sqlite
  ];

}
