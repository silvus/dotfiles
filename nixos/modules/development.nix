{ pkgs, lib, ... }:

with lib;

{
  environment.systemPackages = with pkgs; [
    # Core development tools
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

    # PHP
    php85
    php85Extensions.mbstring
    php85Extensions.intl
    php85Extensions.curl
    php85Extensions.bcmath
    php85Extensions.gd
    php85Extensions.xml
    # php85Extensions.pdo
    # php85Extensions.pdo_sqlite
    php85Extensions.sqlite3
    php85Extensions.openssl
    php85Extensions.dom
    php85Extensions.bz2
    php85Extensions.zip
    # php85Extensions.zlib
    # php85Extensions.yaml
    # php85Extensions.uuid
    # php85Extensions.sodium
    # php85Extensions.readline

    # JS
    bun

    # Utilities
    lsof
    sqlite

    # Static generator
    zola
  ];

}
