# WireGuard VPN confined to a network namespace
#
# The interface is created in the root namespace (its encrypted UDP socket stays
# there and goes out through the physical NIC), then moved into the namespace.
# Inside, there is only lo and the tunnel: the only route is the tunnel, so if it
# goes down there is no route at all (no leak, no firewall rules needed).
# https://www.wireguard.com/netns/
#
# Services opt in with: systemd.services.<name> = config.vpnNetns.confine;

{
  pkgs,
  lib,
  config,
  ...
}:

let
  # This module's options
  cfg = config.vpnNetns;
  ip = "${pkgs.iproute2}/bin/ip";
  # The peer unit adds the default route in the namespace
  peerService = "wireguard-${cfg.interface}-peer-vpn.service";
in
{
  options.vpnNetns = {
    name = lib.mkOption {
      type = lib.types.str;
      default = "vpn";
      description = "Network namespace.";
    };
    interface = lib.mkOption {
      type = lib.types.str;
      default = "torrent-NL";
      description = "WireGuard interface, inside the namespace.";
    };
    privateKeyFile = lib.mkOption {
      type = lib.types.str;
      default = "/data/doc/security/vpn/wireguard/torrent-NL.key";
      description = "WireGuard private key (out of the store, root 600).";
    };
    address = lib.mkOption {
      type = lib.types.str;
      description = "Tunnel address ([Interface] Address).";
    };
    gateway = lib.mkOption {
      type = lib.types.str;
      default = "10.2.0.1";
      description = "VPN gateway, inside the tunnel (NAT-PMP port forwarding).";
    };
    dns = lib.mkOption {
      type = lib.types.str;
      default = cfg.gateway;
      defaultText = lib.literalExpression "config.vpnNetns.gateway";
      description = "DNS server, reached through the tunnel.";
    };
    mtu = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = "Tunnel MTU ([Interface] MTU), null for the WireGuard default.";
    };
    publicKey = lib.mkOption {
      type = lib.types.str;
      description = "VPN server public key ([Peer] PublicKey).";
    };
    endpoint = lib.mkOption {
      type = lib.types.str;
      description = "VPN server ip:port ([Peer] Endpoint).";
    };
    confine = lib.mkOption {
      type = lib.types.attrs;
      readOnly = true;
      description = "Systemd service settings that run a service inside the namespace.";
      default = {
        requires = [ peerService ];
        after = [ peerService ];
        # The tunnel stops: so does the service (it would have no route anyway)
        bindsTo = [ peerService ];
        serviceConfig = {
          # Fails to start if the namespace is missing: never falls back to the host network
          NetworkNamespacePath = "/run/netns/${cfg.name}";
          # DNS through the tunnel
          BindReadOnlyPaths = [ "/etc/netns/${cfg.name}/resolv.conf:/etc/resolv.conf" ];
          # glibc would resolve through these host sockets (nscd, resolved): DNS leak
          InaccessiblePaths = [
            "-/run/nscd"
            "-/run/systemd/resolve"
          ];
        };
      };
    };
  };

  config = {
    environment.systemPackages = with pkgs; [
      wireguard-tools
    ];

    networking.wireguard.interfaces.${cfg.interface} = {
      interfaceNamespace = cfg.name;
      inherit (cfg) privateKeyFile;
      ips = [ cfg.address ];
      inherit (cfg) mtu;
      # Created before the interface. Kept on stop: confined services keep their namespace.
      preSetup = ''
        ${ip} netns add ${cfg.name} 2>/dev/null || true
        ${ip} -n ${cfg.name} link set lo up
        ${ip} netns exec ${cfg.name} ${pkgs.procps}/bin/sysctl -qw net.ipv6.conf.all.disable_ipv6=1
      '';
      peers = [
        {
          name = "vpn";
          inherit (cfg) publicKey endpoint;
          # Default route in the namespace
          allowedIPs = [ "0.0.0.0/0" ];
          persistentKeepalive = 25;
        }
      ];
    };

    # Used by NetworkNamespacePath services (via confine) and `ip netns exec`
    environment.etc."netns/${cfg.name}/resolv.conf".text = ''
      nameserver ${cfg.dns}
    '';
  };
}
