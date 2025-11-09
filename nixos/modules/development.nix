{ config, pkgs, lib, ... }:

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

      # Language servers and formatters
      nil
      nixfmt-rfc-style
      pyright
      ruff
      rust-analyzer
      lua-language-server
      stylua
      shellcheck
      shfmt
      bash-language-server

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
