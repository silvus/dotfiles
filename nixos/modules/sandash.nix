{ pkgs, sandash, ... }:

{
  environment.systemPackages = [
    sandash.packages.${pkgs.stdenv.hostPlatform.system}.default
  ];
}
