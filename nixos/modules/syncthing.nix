{ config, pkgs, lib, ... }:

let
  hosts = {
    nixos-vm = "NIXOSVM-ABCD-EFGH-IJKL-MNOPQRSTUVWX";
    noctus   = "NOCTUS01-ABCD-EFGH-IJKL-MNOPQRSTUVWX";
    somnus   = "SOMNUS01-ABCD-EFGH-IJKL-MNOPQRSTUVWX";
    virtus   = "VIRTUS01-ABCD-EFGH-IJKL-MNOPQRSTUVWX";
    servius  = "SERVIUS1-ABCD-EFGH-IJKL-MNOPQRSTUVWX";
  };

  sharedFolders = [
    "phone"
    "dev"
    "music"
  ];

  mkFolder = name: {
    path = "/data/${name}";
    devices = builtins.attrNames hosts;
    type = "sendreceive";
    ignorePerms = false;
  };

in {
  services.syncthing = {
    enable = true;
    user = "silvus";
    dataDir = "/home/silvus/.local/share/syncthing";
    configDir = "/home/silvus/.config/syncthing";

    settings = {
      gui = {
        enabled = config.desktop.enable or false;
        address = "127.0.0.1:8384";
        theme = "dark";
      };

      devices = lib.mapAttrs (_: id: { inherit id; addresses = [ "dynamic" ]; }) hosts;
      folders = lib.listToAttrs (map (name: { name = name; value = mkFolder name; }) sharedFolders);

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

  networking.firewall.allowedTCPPorts = [ 8384 22000 ];
  networking.firewall.allowedUDPPorts = [ 21027 22000 ];

  systemd.tmpfiles.rules =
    [
      "d /home/silvus/.local/share/syncthing 0755 silvus users -"
      "d /home/silvus/.config/syncthing 0755 silvus users -"
      "d /data 0755 silvus users -"
    ]
    ++ map (n: "d /data/${n} 0755 silvus users -") sharedFolders;

  environment.systemPackages = [ pkgs.syncthing ];

  users.users.silvus.extraGroups = [ "syncthing" ];
  users.groups.syncthing = {};
}
