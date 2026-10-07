# WireGuard LAN access + Gandi dynamic DNS
{
  pkgs,
  config,
  ...
}:

let
  # VPN interface (wireguard_kill_switch.nix)
  vpn = config.killswitch.interface;
  # Physical NIC
  nic = "enp2s0";
  ip = "${pkgs.iproute2}/bin/ip";
  # Listen port, not the VPN's ListenPort
  wgPort = 51820;
  # Peers' addresses
  wgSubnet = "10.100.0.0/24";
  # Marks wg0's packets
  wgFwMark = "0xca6c";
  # Marked packets bypass the VPN table
  wgRule = "fwmark ${wgFwMark} lookup main priority 100";
  # DDNS user
  ddnsUid = 400;
  # Its traffic bypasses the VPN table
  ddnsRule = "uidrange ${toString ddnsUid}-${toString ddnsUid} lookup main priority 100";
in
{
  imports = [ ./wireguard_kill_switch.nix ];

  # sudo install -Dm600 portus-private.key /etc/wireguard/wg0-private.key
  networking.wireguard.interfaces.wg0 = {
    ips = [ "10.100.0.1/24" ];
    listenPort = wgPort;
    fwMark = wgFwMark;
    privateKeyFile = "/etc/wireguard/wg0-private.key";
    # Before wg-quick's rules
    postSetup = ''
      ${ip} rule del ${wgRule} 2>/dev/null || true
      ${ip} rule add ${wgRule}
    '';
    postShutdown = "${ip} rule del ${wgRule} || true";

    # From portus's wg0.conf (/32s only)
    peers = [
      # {
      #   # Laptop
      #   publicKey = "<peer's wg pubkey>";
      #   allowedIPs = [ "10.100.0.2/32" ];
      # }
    ];
  };

  # Peers: LAN via the NIC, internet via the VPN
  networking.nat = {
    enable = true;
    externalInterface = nic;
    internalInterfaces = [ "wg0" ];
    extraCommands = ''
      iptables -w -t nat -A nixos-nat-post -s ${wgSubnet} -o ${vpn} -j MASQUERADE
    '';
  };

  networking.firewall = {
    allowedUDPPorts = [ wgPort ];
    trustedInterfaces = [ "wg0" ];
    # Strict drops handshakes
    checkReversePath = "loose";
  };

  # Kill switch holes
  killswitch.extraCommands = ''
    # Tunnel
    iptables -w -A killswitch-out -o wg0 -j ACCEPT

    # Server packets
    iptables -w -A killswitch-out -o ${nic} -m mark --mark ${wgFwMark} -j ACCEPT

    # DDNS: real IP only, never the VPN's
    iptables -w -A killswitch-out -o ${vpn} -m owner --uid-owner ${toString ddnsUid} -j REJECT
    iptables -w -A killswitch-out -o ${nic} -p tcp --dport 443 -m owner --uid-owner ${toString ddnsUid} -j ACCEPT
  '';

  # Dynamic DNS (script holds the token):
  users.users.gandi-ddns = {
    isSystemUser = true;
    uid = ddnsUid;
    group = "gandi-ddns";
  };
  users.groups.gandi-ddns.gid = ddnsUid;

  systemd.services.gandi-ddns = {
    description = "Update Gandi LiveDNS A record with current public IP";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    serviceConfig = {
      Type = "oneshot";
      User = "gandi-ddns";
      Group = "gandi-ddns";
      # Bypass the VPN table (re-added each run)
      ExecStartPre = [
        "+-${ip} rule del ${ddnsRule}"
        "+${ip} rule add ${ddnsRule}"
      ];
      ExecStart = "${pkgs.python3}/bin/python3 /data/dev/devops/gandi-ddns";
      StateDirectory = "gandi-ddns";
    };
  };
  systemd.timers.gandi-ddns = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "2min";
      OnUnitActiveSec = "10min";
    };
  };
}
