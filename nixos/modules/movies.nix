{ pkgs, movies, ... }:

{
  environment.systemPackages = [
    movies.packages.${pkgs.stdenv.hostPlatform.system}.default
  ];
}
