{ pkgs, ... }:

{

  home.packages = with pkgs; [
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
    rust-analyzer

    # nix lsp
    nil
    # nix formatter
    nixfmt-rfc-style

    # bash lsp
    bash-language-server
    # bash formatter
    shfmt
  ];

}

