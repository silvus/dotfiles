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

    # Containers
    # incus

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

  # Enable Incus containers
  virtualisation.incus = {
    enable = true;
    ui.enable = true;
  };

  # Incus env
  users.users.silvus.extraGroups = [ "incus-admin" ];
  # Enable nftables (required for Incus)
  networking.nftables.enable = true;
}
