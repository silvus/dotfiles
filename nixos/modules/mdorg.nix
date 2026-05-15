{ pkgs, mdorg, ... }:

{
  environment.systemPackages = [
    mdorg.packages.${pkgs.stdenv.hostPlatform.system}.default
  ];
}
