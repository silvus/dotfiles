{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # Python already in base
    # python3
    # python lsp
    ty
    ruff
    python313Packages.ruff
  ];
}
