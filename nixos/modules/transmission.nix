{
  pkgs,
  lib,
  config,
  ...
}:

let
  # Run inside the VPN namespace
  confine = config.vpnNetns.confine;
  rpcPort = 9091;
in
{
  # Torrents only through the VPN
  imports = [ ./vpn_netns.nix ];

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

    # Both are in the VPN namespace, the host firewall doesn't apply there
    openPeerPorts = false;
    openRPCPort = false;

    settings = {
      download-dir = "/data/torrents/download";
      incomplete-dir-enabled = true;
      incomplete-dir = "/data/torrents/download-in-progress";
      watch-dir-enabled = true;
      watch-dir = "/data/torrents/watch";

      rpc-port = rpcPort;
      # The namespace's loopback: the LAN reaches it through transmission-rpc-proxy
      rpc-bind-address = "127.0.0.1";
      # No access control: every client comes through the proxy, so from 127.0.0.1. Filtering is done on the socket (IPAddressAllow).
      rpc-whitelist = "127.0.0.1";
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

  systemd.services.transmission = lib.mkMerge [
    confine
    {
      # transmission 4.1.3 never sends READY=1: the daemon works but systemd
      # waits in "activating" until the start timeout kills it
      serviceConfig = {
        Type = lib.mkForce "simple";
        # Without READY=1, "active" would mean "forked": wait for the RPC to listen,
        # so units ordered After= (the RPC proxy) find it up.
        # RootDirectoryStartOnly: runs outside the chroot, still in the namespace.
        ExecStartPost = pkgs.writeShellScript "transmission-wait-rpc" ''
          until (exec 3<>/dev/tcp/127.0.0.1/${toString rpcPort}) 2>/dev/null; do
            ${pkgs.coreutils}/bin/sleep 0.5
          done
        '';
        ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
        # The service runs in a chroot with only the download dirs mounted.
        # Seeded files are symlinks into the library, so expose it.
        BindPaths = [
          "/data/torrents"
          "/data/movies"
          "/data/series"
        ];
      };
    }
  ];

  # Web UI from the LAN: the socket is on the host, each connection is handed to a proxy inside the namespace (no veth, no route out of the namespace)
  systemd.sockets.transmission-rpc-proxy = {
    description = "Transmission RPC from the LAN";
    wantedBy = [ "sockets.target" ];
    listenStreams = [ "0.0.0.0:${toString rpcPort}" ];
    # The RPC has no password and Transmission sees every client as 127.0.0.1: only the host itself here, hosts add their LAN to IPAddressAllow.
    socketConfig = {
      IPAddressDeny = "any";
      IPAddressAllow = [ "localhost" ];
    };
  };
  systemd.services.transmission-rpc-proxy = lib.mkMerge [
    confine
    {
      description = "Transmission RPC proxy into the VPN namespace";
      # Requisite, not Requires: a web UI request never starts a stopped Transmission
      # (the proxy fails, the client gets a reset)
      requisite = [ "transmission.service" ];
      after = [ "transmission.service" ];
      # Transmission stops: so does the proxy
      partOf = [ "transmission.service" ];
      serviceConfig = {
        ExecStart = "${config.systemd.package}/lib/systemd/systemd-socket-proxyd --exit-idle-time=5min 127.0.0.1:${toString rpcPort}";
        DynamicUser = true;
        PrivateTmp = true;
      };
    }
  ];
  networking.firewall.allowedTCPPorts = [ rpcPort ];

  # Port forward
  # Inside the namespace: natpmpc reaches the gateway through the tunnel,
  # transmission-remote reaches the RPC on the namespace's loopback
  systemd.services.port-forward = lib.mkMerge [
    confine
    {
      description = "ProtonVPN NAT-PMP Port Forward";

      # Systemd services only get a minimal PATH (coreutils, grep, sed, ...)
      path = with pkgs; [ gawk ];

      serviceConfig = {
        ExecStart = pkgs.writeShellScript "port-forward.sh" ''
          while true; do
            output=$(${pkgs.libnatpmp}/bin/natpmpc -a 1 0 udp 60 -g ${config.vpnNetns.gateway} && \
                     ${pkgs.libnatpmp}/bin/natpmpc -a 1 0 tcp 60 -g ${config.vpnNetns.gateway})

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
    }
  ];
}
