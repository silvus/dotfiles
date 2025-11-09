{ config, pkgs, lib, ... }:

with lib;

{
  config = {
    # Basic firewall
    networking.firewall = {
      enable = true;
      allowedTCPPorts = [ 22 ];
      allowedUDPPorts = [ ];
      allowPing = true;
    };

    # Fail2ban
    services.fail2ban = {
      enable = true;
      maxretry = 5;
      bantime = "10m";
    };

    # SSH hardening
    services.openssh.settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
      X11Forwarding = false;
      MaxAuthTries = 3;
      ClientAliveInterval = 300;
      ClientAliveCountMax = 2;
    };

    # System security
    security = {
      sudo = {
        enable = true;
        wheelNeedsPassword = true;
        execWheelOnly = true;
      };
      lockKernelModules = true;
      protectKernelImage = true;
    };

    # Kernel hardening
    boot.kernel.sysctl = {
      # Network security
      "net.ipv4.conf.all.send_redirects" = 0;
      "net.ipv4.conf.default.send_redirects" = 0;
      "net.ipv4.conf.all.accept_redirects" = 0;
      "net.ipv4.conf.default.accept_redirects" = 0;
      "net.ipv4.conf.all.accept_source_route" = 0;
      "net.ipv4.icmp_ignore_bogus_error_responses" = 1;
      "net.ipv4.tcp_syncookies" = 1;

      # Memory protection
      "kernel.dmesg_restrict" = 1;
      "kernel.kptr_restrict" = 2;
      "kernel.yama.ptrace_scope" = 1;

      # File system protection
      "fs.protected_hardlinks" = 1;
      "fs.protected_symlinks" = 1;
    };

    # Automatic security updates disabled by default
    system.autoUpgrade.enable = false;
  };
}
