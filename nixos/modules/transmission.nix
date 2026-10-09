{
  pkgs,
  lib,
  config,
  ...
}:

let
  vpnService = "wg-quick-${config.killswitch.interface}.service";
in
{
  # Torrents only through the VPN
  imports = [ ./wireguard_kill_switch.nix ];

  # Transmission
  environment.systemPackages = with pkgs; [
    transmission_4
    libnatpmp
  ];

  # https://search.nixos.org/options?channel=25.11&query=transmission
  services.transmission = {
    enable = true;

    user = "silvus";
    group = "users";

    home = "/data/torrents";
    # downloadDirPermissions = "775";
    # credentialsFile = "";

    openPeerPorts = true;
    openRPCPort = true;

    settings = {
      download-dir = "/data/torrents/download";
      incomplete-dir-enabled = true;
      incomplete-dir = "/data/torrents/download-in-progress";
      watch-dir-enabled = true;
      watch-dir = "/data/torrents/watch";

      rpc-port = 9091;
      rpc-bind-address = "0.0.0.0";
      # Allow web UI from LAN (default is localhost only)
      # 10.100.0.*: WireGuard peers (wireguard_server.nix)
      rpc-whitelist = "127.0.0.1,192.168.1.*,10.100.0.*";
      # Hostnames allowed in the URL (DNS rebinding protection, IPs always pass)
      rpc-host-whitelist = "servius,servius.*";

      peer-port-random-on-start = false;
      peer-port = 45242;

      speed-limit-down-enabled = false;
      speed-limit-down = 500;
      speed-limit-up-enabled = true;
      speed-limit-up = 5000;
    };
  };

  systemd.services.transmission = {
    # Start once the VPN is up (the killswitch blocks everything else anyway)
    after = [ vpnService ];
    wants = [ vpnService ];
    # transmission 4.1.3 never sends READY=1: the daemon works but systemd
    # waits in "activating" until the start timeout kills it
    serviceConfig = {
      Type = lib.mkForce "simple";
      ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
      # The service runs in a chroot with only the download dirs mounted.
      # Seeded files are symlinks into the library, so expose it.
      BindPaths = [
        "/data/torrents"
        "/data/movies"
        "/data/series"
      ];
    };
  };

  # Port forward
  systemd.services.port-forward = {
    description = "ProtonVPN NAT-PMP Port Forward";

    after = [
      "network-online.target"
      vpnService
    ];
    wants = [
      "network-online.target"
      vpnService
    ];

    # Systemd services only get a minimal PATH (coreutils, grep, sed, ...)
    path = with pkgs; [ gawk ];

    serviceConfig = {
      ExecStart = pkgs.writeShellScript "port-forward.sh" ''
        while true; do
          output=$(${pkgs.libnatpmp}/bin/natpmpc -a 1 0 udp 60 -g 10.2.0.1 && \
                   ${pkgs.libnatpmp}/bin/natpmpc -a 1 0 tcp 60 -g 10.2.0.1)

          if [ $? -ne 0 ]; then
            sleep 60
            continue
          fi

          port=$(echo "$output" | grep 'Mapped public port' | awk '{print $4}' | head -n1)

          if [[ -n "$port" ]]; then
            ${pkgs.transmission_4}/bin/transmission-remote --port "$port"
          fi

          sleep 45
        done
      '';

      Restart = "always";
      RestartSec = 10;
    };

    wantedBy = [ "multi-user.target" ];
  };
}
