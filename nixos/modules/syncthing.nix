{ pkgs, ... }:

let
  syncthingUser = "silvus";
  syncthingGroup = "users";

  # Base dir for automatic creation
  baseDirs = [
    "/data"
  ];

in
{

  # Create directories before Syncthing starts
  systemd.tmpfiles.rules = map (dir:
    "d ${dir} 0700 ${syncthingUser} ${syncthingGroup} -"
  ) baseDirs;

  # Ensure Syncthing service waits for directories
  systemd.services."syncthing@${syncthingUser}" = {
    after = [ "systemd-tmpfiles-setup.service" ];
    requires = [ "systemd-tmpfiles-setup.service" ];
  };

  # Base system packages
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
    user = syncthingUser;
    group = syncthingGroup;
    dataDir = "/home/${syncthingUser}/.local/share/syncthing";
    configDir = "/home/${syncthingUser}/.config/syncthing";
    guiAddress = "0.0.0.0:5001";
  };

  # Open Syncthing ports in firewall
  networking.firewall = {
    allowedTCPPorts = [ 5001];
    # allowedTCPPorts = [ 22000 ];
    # allowedUDPPorts = [ 21027 22000 ];
  };
}
