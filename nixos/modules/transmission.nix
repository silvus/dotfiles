
{ pkgs, ... }:

{

  # Transmission
  environment.systemPackages = with pkgs; [
    transmission_4
    wireguard-tools
    natpmpc

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

      peer-port-random-on-start = false;
      peer-port = 45242;

      speed-limit-down-enabled = false;
      speed-limit-down = 500;
      speed-limit-up-enabled = true;
      speed-limit-up = 5000;
    };
  };

  # VPN
  networking.wg-quick.interfaces."torrent-NL" = {
    configFile = "/data/doc/security/vpn/wireguard/torrent-NL.conf";
  };

  # Port forward
  systemd.services.port-forward = {
    description = "ProtonVPN NAT-PMP Port Forward";

    after = [ "network-online.target" "wg-quick-torrent-NL.service" ];
    wants = [ "wg-quick-torrent-NL.service" ];

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
      # Drop all incoming traffic unless explicitly allowed.
      iptables -P INPUT DROP

      # Drop all outgoing traffic unless explicitly allowed.
      # Core killswitch mechanism.
      iptables -P OUTPUT DROP

      # Allow loopback traffic.
      # Required for local IPC and localhost services.
      iptables -A INPUT -i lo -j ACCEPT
      iptables -A OUTPUT -o lo -j ACCEPT

      # Allow all traffic through the WireGuard VPN interface.
      # Once the tunnel is established, all torrent traffic flows here.
      iptables -A INPUT -i torrent-NL -j ACCEPT
      iptables -A OUTPUT -o torrent-NL -j ACCEPT

      # Allow WireGuard handshake traffic on the physical NIC.
      # Without this the VPN tunnel cannot be established.
      # iptables -A OUTPUT -o enp2s0 -p udp --dport 51820 -j ACCEPT
      # iptables -A INPUT -i enp2s0 -p udp --sport 51820 -j ACCEPT

      # Allow LAN traffic.
      iptables -A INPUT -i enp2s0 -s 192.168.1.0/24 -j ACCEPT
      iptables -A OUTPUT -o enp2s0 -d 192.168.1.0/24 -j ACCEPT
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
    # net.ipv6.conf.all.disable_ipv6 = 1
    # net.ipv6.conf.default.disable_ipv6 = 1
    # net.ipv6.conf.lo.disable_ipv6 = 1
    # net.ipv6.conf.tun0.disable_ipv6 = 1
  };
}
