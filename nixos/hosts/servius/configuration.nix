# Servius
{
  pkgs,
  lib,
  config,
  movies,
  ...
}:

let
  # Physical NIC
  nic = "enp2s0";
  lanSubnet = "192.168.1.0/24";
  # External SSH port (router forwards TCP 2222, 22 stays for the LAN)
  sshPort = 2222;
  # DDNS user, its traffic bypasses the VPN table
  ddnsUid = 400;
in
{
  imports = [
    ./hardware-configuration.nix

    ../../modules/base.nix
    ../../modules/keyboard.nix
    ../../modules/syncthing.nix
    ../../modules/desktop_base.nix
    # ../../modules/desktop_sway.nix
    # ../../modules/desktop_niri.nix
    ../../modules/desktop_awesome.nix
    # ../../modules/desktop_office.nix
    # ../../modules/desktop_3dmodel.nix
    # ../../modules/gaming.nix
    # ../../modules/printing.nix
    # ../../modules/desktop_udev.nix
    ../../modules/security.nix
    # ../../modules/mounts/movies.nix
    # ../../modules/mounts/tvshows.nix
    # ../../modules/mounts/torrents.nix
    # ../../modules/mounts/doc.nix
    # ../../modules/development
    # ../../modules/development/go.nix
    # ../../modules/development/rust.nix
    # ../../modules/development/python.nix
    # ../../modules/development/nix.nix
    # ../../modules/development/lua.nix
    # ../../modules/development/bash.nix
    # ../../modules/development/markdown.nix
    # ../../modules/development/php.nix
    # ../../modules/development/containers.nix
    ../../modules/mdorg.nix
    ../../modules/movies.nix
    ../../modules/transmission.nix
    ../../modules/wireguard_kill_switch.nix

  ];

  # Per-host, not shared via base.nix.
  system.stateVersion = "26.05";

  environment.systemPackages = with pkgs; [
    imapfilter
  ];

  # Nvidia
  # https://wiki.nixos.org/wiki/NVIDIA
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = false;
    open = false; # Proprietary driver only for Kepler
    nvidiaSettings = true;
    # GTX 650 (Kepler): only supported by the 470 legacy driver
    package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
  };
  hardware.graphics.enable = true;
  nixpkgs.config.nvidia.acceptLicense = true;

  # Send native 4K to the LG TV (a 1080p signal gets upscaled with overscan),
  # but render the desktop at 1920x1080: the GPU scales it 2x.
  # ViewPortOut (in 4K pixels) compensates the TV overscan: margins L=40 R=38 T=22 B=20.
  # ForceFullCompositionPipeline: vsync every frame, no tearing (no compositor with awesome).
  # Can be tested with:
  # nvidia-settings --assign CurrentMetaMode="DPY-2: 3840x2160_30 { ViewPortIn=1920x1080, ViewPortOut=3762x2118+40+22, ForceFullCompositionPipeline=On }"
  services.xserver.screenSection = ''
    Option "MetaModes" "DPY-2: 3840x2160 { ViewPortIn=1920x1080, ViewPortOut=3762x2118+40+22, ForceFullCompositionPipeline=On }"
  '';
  services.xserver.displayManager.sessionCommands = lib.mkAfter ''
    # delmonitor first: setmonitor fails with BadValue if TV already exists
    # ${pkgs.xrandr}/bin/xrandr --delmonitor TV 2>/dev/null || true
    # ${pkgs.xrandr}/bin/xrandr --setmonitor TV 1920/1600x1080/900+0+0 HDMI-0

    # Never blank or power off the TV (overrides desktop_awesome.nix)
    ${pkgs.xset}/bin/xset s off
    ${pkgs.xset}/bin/xset -dpms
  '';

  # Autostart
  systemd.user.services.movies = {
    description = "Movies";
    wantedBy = [ "graphical-session.target" ];
    after = [ "graphical-session.target" ];
    # Systemd services only get a minimal PATH: tools the app spawns by name
    path = with pkgs; [
      mpv # playback
      ffmpeg # ffprobe (scrapper)
      sqlite # database backup/restore
      gzip # zcat (database backup/restore)
    ];
    serviceConfig = {
      ExecStart = "${movies.packages.${pkgs.stdenv.hostPlatform.system}.default}/bin/movies";
      Restart = "on-failure";
      RestartSec = 5;
    };
  };

  # Hide the mouse cursor when idle
  services.unclutter = {
    enable = true;
    timeout = 2;
  };

  # Never sleep
  systemd.sleep.settings.Sleep = {
    AllowSuspend = "no";
    AllowHibernation = "no";
    AllowHybridSleep = "no";
    AllowSuspendThenHibernate = "no";
  };

  # NFSv4 shares for the LAN (modules/mounts/ on the clients)
  services.nfs.server = {
    enable = true;
    exports = ''
      /data/movies          ${lanSubnet}(rw,no_subtree_check)
      /data/series          ${lanSubnet}(rw,no_subtree_check)
      /data/torrents        ${lanSubnet}(rw,no_subtree_check)
      /data/trogo/musique   ${lanSubnet}(rw,no_subtree_check)
      /data/trogo/histoires ${lanSubnet}(rw,no_subtree_check)
    '';
  };
  networking.firewall.allowedTCPPorts = [ 2049 ];

  # SSH from outside, openFirewall (default) opens both ports
  services.openssh = {
    ports = [
      22
      sshPort
    ];
    # Passwords from the LAN only, keys from outside (any port)
    extraConfig = ''
      Match Address *,!${lanSubnet}
        PasswordAuthentication no
        KbdInteractiveAuthentication no
    '';
  };

  # sshd's replies and DDNS bypass the VPN table
  systemd.services.vpn-bypass =
    let
      wg = "wg-quick-${config.killswitch.interface}.service";
      ip = "${pkgs.iproute2}/bin/ip";
      sshRule = "ipproto tcp sport ${toString sshPort} lookup main priority 100";
      ddnsRule = "uidrange ${toString ddnsUid}-${toString ddnsUid} lookup main priority 100";
    in
    {
      description = "Route sshd replies and DDNS outside the VPN";
      # After wg-quick's rules (unprioritized, they'd go before ours), re-added when it restarts
      after = [ wg ];
      partOf = [ wg ];
      wantedBy = [
        wg
        "multi-user.target"
      ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStartPre = [
          "-${ip} rule del ${sshRule}"
          "-${ip} rule del ${ddnsRule}"
        ];
        ExecStart = [
          "${ip} rule add ${sshRule}"
          "${ip} rule add ${ddnsRule}"
        ];
        ExecStop = [
          "-${ip} rule del ${sshRule}"
          "-${ip} rule del ${ddnsRule}"
        ];
      };
    };

  # Strict drops internet clients (their route back is the VPN)
  networking.firewall.checkReversePath = "loose";

  # Kill switch holes
  killswitch.extraCommands = ''
    # SSH replies (to connections in on sshPort, not any process using that source port)
    iptables -w -A killswitch-out -o ${nic} -p tcp -m conntrack --ctdir REPLY --ctorigdstport ${toString sshPort} -j ACCEPT

    # DDNS: real IP only, never the VPN's
    iptables -w -A killswitch-out -o ${config.killswitch.interface} -m owner --uid-owner ${toString ddnsUid} -j REJECT
    iptables -w -A killswitch-out -o ${nic} -p tcp --dport 443 -m owner --uid-owner ${toString ddnsUid} -j ACCEPT
  '';

  # Autologin
  services.displayManager.autoLogin = {
    enable = true;
    user = "silvus";
  };

  # Dynamic DNS
  users.users.gandi-ddns = {
    isSystemUser = true;
    uid = ddnsUid;
    group = "gandi-ddns";
  };
  users.groups.gandi-ddns.gid = ddnsUid;

  systemd.services.gandi-ddns = {
    description = "Update DNS A record with current public IP";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    serviceConfig = {
      Type = "oneshot";
      User = "gandi-ddns";
      Group = "gandi-ddns";
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

  # Backup
  systemd.services.backup-borg = {
    description = "Borg Backup";
    # Systemd services only get a minimal PATH
    path = with pkgs; [
      # bash
      python3
      borgbackup
      openssh
    ];
    serviceConfig = {
      Type = "oneshot";
      User = "silvus";
      ExecStart = "/data/doc/.bin/backup_borg";
    };
  };
  systemd.timers.backup-borg = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "02:30";
      Persistent = true;
    };
  };

  # Imapfilter
  systemd.services.imapfilter = {
    description = "IMAP Mail Sorting";
    # Systemd services only get a minimal PATH
    path = with pkgs; [
      python3
    ];
    serviceConfig = {
      Type = "oneshot";
      User = "silvus";
      ExecStart = "${pkgs.imapfilter}/bin/imapfilter -c /data/doc/ressources/mail/imapfilter.lua";
    };
  };
  systemd.timers.imapfilter = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "*-*-* *:00:00";
      Persistent = true;
    };
  };

}
