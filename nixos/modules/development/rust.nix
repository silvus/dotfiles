{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    rustc
    cargo
    # rust lsp
    rust-analyzer
    # rustup component add rust-analyzer
    rustfmt
    # rustup
  ];
}
