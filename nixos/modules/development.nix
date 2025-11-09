{ pkgs, lib, ... }:

with lib;

{
  config = {
    environment.systemPackages = with pkgs; [
      # Core development tools
      git
      lazygit
      gnumake
      cmake
      pkg-config

      # Containers
      incus

      # lua lsp
      lua-language-server
      # lua formatter
      stylua

      # python lsp
      pyright
      # ty
      # python formatter
      ruff

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

      # Languages
      python3
      rustc
      cargo
      lua
      gcc
      clang

      # Editors
      neovim
      vscodium

      # Utilities
      jq
      curl
      wget
      lsof
      strace
      sqlite
    ];

    # Enable Incus containers
    virtualisation.incus = {
      enable = true;
      ui.enable = true;
    };

    users.users.silvus.extraGroups = [ "incus-admin" ];
  };
}
