# Servius
{ ... }:

{
  imports = [
    ./hardware-configuration.nix

    ../../modules/base.nix
    ../../modules/keyboard.nix
    # Enable server modules
    # ../../modules/security.nix
    ../../modules/development.nix
  ];

  # Server-specific packages
  # environment.systemPackages = with pkgs; [
  #   # Monitoring
  #   prometheus-node-exporter

  #   # Backup tools
  #   borgbackup
  #   restic

  #   # Web server
  #   nginx
  #   certbot

  #   # System administration
  #   screen
  #   tmux
  # ];

  # Web server (nginx)
  # services.nginx = {
  #   enable = false; # Enable as needed per deployment
  #   recommendedGzipSettings = true;
  #   recommendedOptimisation = true;
  #   recommendedProxySettings = true;
  #   recommendedTlsSettings = true;
  # };

  # # Server firewall configuration
  # networking.firewall.allowedTCPPorts = [
  #   22    # SSH
  #   80    # HTTP
  #   443   # HTTPS
  #   9100  # Node exporter
  # ];

  # # Server optimizations
  # boot.kernel.sysctl = {
  #   # Network optimizations
  #   "net.core.rmem_max" = 16777216;
  #   "net.core.wmem_max" = 16777216;
  #   "net.ipv4.tcp_rmem" = "4096 87380 16777216";
  #   "net.ipv4.tcp_wmem" = "4096 87380 16777216";
  #   "net.ipv4.tcp_congestion_control" = "bbr";

  #   # Memory management
  #   "vm.swappiness" = 10;
  #   "vm.dirty_ratio" = 15;
  #   "vm.dirty_background_ratio" = 5;
  # };

  # # Logging configuration
  # services.journald.extraConfig = ''
  #   SystemMaxUse=2G
  #   SystemKeepFree=10G
  #   MaxRetentionSec=2month
  # '';

  # # Disable unnecessary desktop services
  # services.udisks2.enable = false;
  # services.power-profiles-daemon.enable = false;
  # services.blueman.enable = false;
  # hardware.bluetooth.enable = false;
  # services.xserver.enable = false;
  # services.pulseaudio.enable = false;
}
