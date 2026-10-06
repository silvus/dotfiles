{ pkgs, lib, ... }:

{

  # Transmission
  environment.systemPackages = with pkgs; [
    transmission_4
    wireguard-tools
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
      rpc-whitelist = "127.0.0.1,192.168.1.*";
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
    after = [ "wg-quick-torrent-NL.service" ];
    wants = [ "wg-quick-torrent-NL.service" ];
    # transmission 4.1.3 never sends READY=1: the daemon works but systemd
    # waits in "activating" until the start timeout kills it
    serviceConfig = {
      Type = lib.mkForce "simple";
      ExecReload = "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
      # The service runs in a chroot with only the download dirs mounted.
      # Seeded files are symlinks into the library, so expose it (read-only).
      BindReadOnlyPaths = [
        "/data/movies"
        "/data/series"
      ];
    };
  };

  # VPN
  networking.wg-quick.interfaces."torrent-NL" = {
    configFile = "/data/doc/security/vpn/wireguard/torrent-NL.conf";
  };

  # Port forward
  systemd.services.port-forward = {
    description = "ProtonVPN NAT-PMP Port Forward";

    after = [
      "network-online.target"
      "wg-quick-torrent-NL.service"
    ];
    wants = [
      "network-online.target"
      "wg-quick-torrent-NL.service"
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

  # Kill switch
  networking.firewall = {
    enable = true;

    # Open WireGuard UDP port on the physical interface.
    # Required for establishing the ProtonVPN tunnel.
    allowedUDPPorts = [ 51820 ];
    interfaces.enp2s0 = {
      allowedUDPPorts = [ 51820 ];
    };

    # Custom iptables killswitch rules.
    # Default policy:
    # - deny everything
    # - explicitly allow only safe traffic
    extraCommands = ''
      # Run a rule for both IPv4 and IPv6.
      # Note: with enableIPv6 = false the NixOS firewall skips ip6tables entirely,
      # but NetworkManager still brings IPv6 up on enp2s0, so we filter it ourselves.
      both() { iptables -w "$@"; ip6tables -w "$@"; }

      # Rules live in our own chains, flushed on every (re)load,
      # so firewall reloads don't stack duplicate rules in INPUT/OUTPUT.
      for chain in killswitch-in killswitch-out; do
        both -N "$chain" 2>/dev/null || true
        both -F "$chain"
      done
      both -D INPUT -j killswitch-in 2>/dev/null || true
      both -D OUTPUT -j killswitch-out 2>/dev/null || true
      both -A INPUT -j killswitch-in
      both -A OUTPUT -j killswitch-out

      # Drop all traffic unless explicitly allowed.
      # OUTPUT DROP is the core killswitch mechanism.
      both -P INPUT DROP
      both -P OUTPUT DROP
      ip6tables -w -P FORWARD DROP

      # Allow loopback traffic.
      # Required for local IPC and localhost services.
      both -A killswitch-in -i lo -j ACCEPT
      both -A killswitch-out -o lo -j ACCEPT

      # Allow all traffic through the WireGuard VPN interface.
      # Once the tunnel is established, all torrent traffic flows here.
      both -A killswitch-in -i torrent-NL -j ACCEPT
      both -A killswitch-out -o torrent-NL -j ACCEPT

      # Allow WireGuard handshake traffic on the physical NIC (IPv4 endpoint).
      # Without this the VPN tunnel cannot be established.
      iptables -w -A killswitch-out -o enp2s0 -p udp --dport 51820 -j ACCEPT
      iptables -w -A killswitch-in -i enp2s0 -p udp --sport 51820 -j ACCEPT

      # Allow LAN traffic (IPv4 only).
      iptables -w -A killswitch-in -i enp2s0 -s 192.168.1.0/24 -j ACCEPT
      iptables -w -A killswitch-out -o enp2s0 -d 192.168.1.0/24 -j ACCEPT

      # Reject (not drop) other IPv6 so apps fall back to IPv4 immediately.
      ip6tables -w -A killswitch-out -j REJECT
    '';

    # Cleanup when firewall reloads.
    # extraStopCommands = ''
    # iptables -P INPUT ACCEPT
    # iptables -P OUTPUT ACCEPT
    # '';
  };

  # Disable IPV6
  networking.enableIPv6 = false;

  boot.kernel.sysctl = {
    # Enable IPv4 forwarding.
    "net.ipv4.ip_forward" = 1;
    "net.ipv6.conf.all.disable_ipv6" = 1;
    "net.ipv6.conf.default.disable_ipv6" = 1;
    "net.ipv6.conf.lo.disable_ipv6" = 1;
    "net.ipv6.conf.tun0.disable_ipv6" = 1;
  };
}
