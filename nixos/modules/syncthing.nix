{ pkgs, ... }:

{
  # Base system packages (minimal, essential only)
  environment.systemPackages = with pkgs; [
    syncthing
  ];

  # Enable Syncthing service
  # https://wiki.nixos.org/wiki/Syncthing
  # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/services/networking/syncthing.nix
  # Setttings are handled by a curl command who failed silently... maybe it doesn't get the api key from the config xml.
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    user = "silvus";
    group = "users";
    dataDir = "/home/silvus/.local/share/syncthing";
    configDir = "/home/silvus/.config/syncthing";
    guiAddress = "0.0.0.0:5001";
  };

  # Open Syncthing ports in firewall
  networking.firewall = {
    allowedTCPPorts = [ 22000 ];
    allowedUDPPorts = [ 21027 22000 ];
  };
}
