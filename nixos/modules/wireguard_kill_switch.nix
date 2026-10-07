# WireGuard VPN kill switch: all traffic through the VPN, except the LAN
{
  pkgs,
  lib,
  config,
  ...
}:

let
  # This module's options
  cfg = config.killswitch;
  # wg-quick config (out of the repo)
  configFile = "/data/doc/security/vpn/wireguard/torrent-NL.conf";
  # VPN endpoint UDP port
  endpointPort = 51820;
  # Physical NIC
  nic = "enp2s0";
  lanSubnet = "192.168.1.0/24";
in
{
  options.killswitch = {
    interface = lib.mkOption {
      type = lib.types.str;
      default = "torrent-NL";
      description = "VPN interface.";
    };
    extraCommands = lib.mkOption {
      type = lib.types.lines;
      default = "";
      description = "Extra killswitch-in/out rules, run before the base ones.";
    };
  };

  config = {
    environment.systemPackages = with pkgs; [
      wireguard-tools
    ];

    networking.wg-quick.interfaces.${cfg.interface} = {
      inherit configFile;
    };

    networking.firewall = {
      enable = true;

      # Deny everything, allow only what's safe
      extraCommands = ''
        # IPv4 + IPv6 (NetworkManager brings IPv6 up anyway)
        both() { iptables -w "$@"; ip6tables -w "$@"; }

        # Own chains, flushed on reload (no duplicates)
        for chain in killswitch-in killswitch-out; do
          both -N "$chain" 2>/dev/null || true
          both -F "$chain"
        done
        both -D INPUT -j killswitch-in 2>/dev/null || true
        both -D OUTPUT -j killswitch-out 2>/dev/null || true
        both -A INPUT -j killswitch-in
        both -A OUTPUT -j killswitch-out

        ${cfg.extraCommands}

        # The kill switch itself
        both -P INPUT DROP
        both -P OUTPUT DROP
        ip6tables -w -P FORWARD DROP

        # Loopback
        both -A killswitch-in -i lo -j ACCEPT
        both -A killswitch-out -o lo -j ACCEPT

        # VPN
        both -A killswitch-in -i ${cfg.interface} -j ACCEPT
        both -A killswitch-out -o ${cfg.interface} -j ACCEPT

        # VPN handshake
        iptables -w -A killswitch-out -o ${nic} -p udp --dport ${toString endpointPort} -j ACCEPT
        iptables -w -A killswitch-in -i ${nic} -p udp --sport ${toString endpointPort} -j ACCEPT

        # LAN (IPv4 only)
        iptables -w -A killswitch-in -i ${nic} -s ${lanSubnet} -j ACCEPT
        iptables -w -A killswitch-out -o ${nic} -d ${lanSubnet} -j ACCEPT

        # No IPv6. Reject: fast fallback to IPv4
        ip6tables -w -A killswitch-out -j REJECT
      '';
    };

    # No IPv6
    networking.enableIPv6 = false;
    boot.kernel.sysctl = {
      "net.ipv6.conf.all.disable_ipv6" = 1;
      "net.ipv6.conf.default.disable_ipv6" = 1;
      "net.ipv6.conf.lo.disable_ipv6" = 1;
    };
  };
}
