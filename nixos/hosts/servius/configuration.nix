# Servius
{ pkgs, config, movies, ... }:

{
  imports = [
    ./hardware-configuration.nix

    ../../modules/base.nix
    ../../modules/keyboard.nix
    ../../modules/syncthing.nix
    ../../modules/desktop_base.nix
    # ../../modules/desktop_sway.nix
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
    # ../../modules/development.nix
    # ../../modules/development_php.nix
    # ../../modules/development_containers.nix
    # ../../modules/mdorg.nix
    ../../modules/movies.nix
    ../../modules/transmission.nix

  ];

  # Nvidia
  # https://wiki.nixos.org/wiki/NVIDIA
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = false;
    open = false; # Required for a 1060
    nvidiaSettings = true;
    # package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
    package = config.boot.kernelPackages.nvidiaPackages.production;
  };
  hardware.graphics.enable = true;
  nixpkgs.config.nvidia.acceptLicense = true;

  environment.systemPackages = with pkgs; [
    imapfilter
    borgbackup
  ];

  # Autostart
  systemd.user.services.movies = {
    description = "Movies";
    wantedBy = [ "graphical-session.target" ];
    after = [ "graphical-session.target" ];
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
  #   "0 */4 * * * silvus imapfilter -c /data/doc/ressources/mail/imapfilter.lua -l /tmp/imapfilter.log"
  # ];

  # Backup
  systemd.services.backup-borg = {
    description = "Borg Backup";
    serviceConfig = {
      Type = "oneshot";
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
      ExecStart = ''
        ${pkgs.imapfilter}/bin/imapfilter \
          -c /data/doc/ressources/mail/imapfilter.lua
      '';
    };
  };
  systemd.timers.imapfilter = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "*-*-* 00/4:00:00";
      Persistent = true;
    };
  };

}
