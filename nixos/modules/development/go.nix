{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # Golang
    go
    # Golang LSP
    gopls
  ];
}
