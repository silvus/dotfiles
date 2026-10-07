# Servius
{
  pkgs,
  lib,
  config,
  movies,
  ...
}:

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
    # ../../modules/security.nix
    # ../../modules/mnt_movies.nix
    # ../../modules/mnt_tvshows.nix
    # ../../modules/mnt_torrents.nix
    # ../../modules/mnt_doc.nix
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
    # ../../modules/mdorg.nix
    ../../modules/movies.nix
    ../../modules/transmission.nix

  ];

  # Per-host, not shared via base.nix.
  system.stateVersion = "26.05";

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

  # Never sleep
  systemd.sleep.settings.Sleep = {
    AllowSuspend = "no";
    AllowHibernation = "no";
    AllowHybridSleep = "no";
    AllowSuspendThenHibernate = "no";
  };

  environment.systemPackages = with pkgs; [
    imapfilter
  ];

  # Autologin
  services.displayManager.autoLogin = {
    enable = true;
    user = "silvus";
  };

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

  # Cron
  # services.cron.enable = true;
  # services.cron.systemCronJobs = [
  #   "30 2 * * * silvus /data/doc/.bin/backup_borg"
  #   "0 * * * * silvus imapfilter -c /data/doc/ressources/mail/imapfilter.lua -l /tmp/imapfilter.log"
  # ];

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
