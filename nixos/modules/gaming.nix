{ config, pkgs, lib, ... }:

with lib;

{
  config = {
    # Gaming packages
    environment.systemPackages = with pkgs; [
      steam
      lutris
      gamemode
    ];

    # Steam configuration
    programs.steam = {
      enable = true;
      remotePlay.openFirewall = true;
      dedicatedServer.openFirewall = true;
      gamescopeSession.enable = true;
    };

    # GameMode for performance optimization
    programs.gamemode = {
      enable = true;
      settings = {
        general = {
          renice = 10;
          ioprio = 7;
          inhibit_screensaver = 1;
          desiredgov = "performance";
        };
        gpu = {
          apply_gpu_optimisations = "accept-responsibility";
          gpu_device = 0;
          amd_performance_level = "high";
        };
      };
    };

    # Gaming optimizations
    boot.kernel.sysctl = {
      # Memory management for gaming
      "vm.dirty_ratio" = lib.mkDefault 3;
      "vm.dirty_background_ratio" = lib.mkDefault 2;

      # Network optimizations for online gaming
      "net.core.rmem_default" = lib.mkDefault 262144;
      "net.core.rmem_max" = lib.mkDefault 16777216;
      "net.core.wmem_default" = lib.mkDefault 262144;
      "net.core.wmem_max" = lib.mkDefault 16777216;
    };

    # Gaming-specific firewall rules
    networking.firewall = {
      allowedTCPPorts = [
        27036 # Steam In-Home Streaming
        27037 # Steam In-Home Streaming
      ];
      allowedUDPPorts = [
        27031 # Steam In-Home Streaming
        27036 # Steam In-Home Streaming
      ];
      allowedTCPPortRanges = [
        { from = 27014; to = 27050; }
      ];
      allowedUDPPortRanges = [
        { from = 27000; to = 27100; }
      ];
    };

    # Enable 32-bit support
    hardware.graphics.enable32Bit = true;

    # Gaming directories
    systemd.tmpfiles.rules = [
      "d /home/silvus/Games 0755 silvus users -"
    ];

    # Gaming group
    users.groups.gaming = {};
    users.users.silvus.extraGroups = [ "gaming" ];
  };
}
