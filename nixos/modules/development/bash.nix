{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # bash lsp
    bash-language-server
    # bash formatter
    shfmt
  ];
}
