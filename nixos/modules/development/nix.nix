{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # nix lsp
    nil
    # nix lsp (for zed)
    nixd
    # nix formatter
    nixfmt
  ];
}
