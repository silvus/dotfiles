{ config, pkgs, lib, ... }:

with lib;

let
  # All hosts in the network with their device IDs
  hosts = {
    nixos-vm = "NIXOSVM-ABCD-EFGH-IJKL-MNOPQRSTUVWX";
    noctus = "NOCTUS01-ABCD-EFGH-IJKL-MNOPQRSTUVWX";
    somnus = "SOMNUS01-ABCD-EFGH-IJKL-MNOPQRSTUVWX";
    virtus = "VIRTUS01-ABCD-EFGH-IJKL-MNOPQRSTUVWX";
    servius = "SERVIUS1-ABCD-EFGH-IJKL-MNOPQRSTUVWX";
  };

  # Include all hosts - syncthing will ignore the current host automatically
  allHostDevices = hosts;

  # Shared folders configuration
  sharedFolders = {
    phone = {
      path = "/data/phone";
      devices = attrNames hosts;
    };
    dev = {
      path = "/data/dev";
      devices = attrNames hosts;
    };
    music = {
      path = "/data/music";
      devices = attrNames hosts;
    };
  };

in
{
  config = {
    services.syncthing = {
      enable = true;
      user = "silvus";
      dataDir = "/home/silvus/.local/share/syncthing";
      configDir = "/home/silvus/.config/syncthing";

      settings = {
        gui = if config.desktop.enable or false then {
          enabled = true;
          address = "127.0.0.1:8384";
          theme = "dark";
        } else {
          enabled = false;
        };

        # Configure all devices
        devices = mapAttrs (name: id: {
          id = id;
          addresses = [ "dynamic" ];
        }) allHostDevices;

        # Configure shared folders
        folders = mapAttrs (name: folderCfg: {
          path = folderCfg.path;
          devices = folderCfg.devices;
          type = "sendreceive";
          ignorePerms = false;
        }) sharedFolders;

        options = {
          urAccepted = -1;
          globalAnnounceEnabled = true;
          localAnnounceEnabled = true;
          relaysEnabled = true;
          natEnabled = true;
          startBrowser = false;
        };
      };
    };

    # Open firewall ports
    networking.firewall = {
      allowedTCPPorts = [ 8384 22000 ];
      allowedUDPPorts = [ 21027 22000 ];
    };

    # Ensure directories exist
    systemd.tmpfiles.rules = [
      "d /home/silvus/.local/share/syncthing 0755 silvus users -"
      "d /home/silvus/.config/syncthing 0755 silvus users -"
      # Create shared data directories
      "d /data 0755 silvus users -"
      "d /data/phone 0755 silvus users -"
      "d /data/dev 0755 silvus users -"
      "d /data/music 0755 silvus users -"
    ];

    # Add syncthing package
    environment.systemPackages = [ pkgs.syncthing ];

    # Add user to syncthing group
    users.users.silvus.extraGroups = [ "syncthing" ];
    users.groups.syncthing = {};
  };
}
