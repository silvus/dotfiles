{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    lua
    # lua lsp
    lua-language-server
    # lua formatter
    stylua
  ];
}
