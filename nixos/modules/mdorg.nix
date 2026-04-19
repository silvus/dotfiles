{ pkgs, mdorg, ... }:

{
  environment.systemPackages = [
    mdorg.packages.${pkgs.system}.default
  ];
}
