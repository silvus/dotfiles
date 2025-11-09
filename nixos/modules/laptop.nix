{ config, pkgs, lib, ... }:

with lib;

{
  config = {
    # TLP Power Management with good defaults
    services.tlp = {
      enable = true;
      settings = {
        # Battery thresholds
        START_CHARGE_THRESH_BAT0 = 20;
        STOP_CHARGE_THRESH_BAT0 = 80;

        # CPU scaling
        CPU_SCALING_GOVERNOR_ON_AC = "performance";
        CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

        # CPU performance
        CPU_ENERGY_PERF_POLICY_ON_AC = "performance";
        CPU_ENERGY_PERF_POLICY_ON_BAT = "power";

        # Platform profile
        PLATFORM_PROFILE_ON_AC = "performance";
        PLATFORM_PROFILE_ON_BAT = "low-power";

        # Runtime power management
        RUNTIME_PM_ON_AC = "on";
        RUNTIME_PM_ON_BAT = "auto";

        # USB autosuspend
        USB_AUTOSUSPEND = 1;

        # WiFi power saving
        WIFI_PWR_ON_AC = "off";
        WIFI_PWR_ON_BAT = "on";

        # Sound power saving
        SOUND_POWER_SAVE_ON_AC = 0;
        SOUND_POWER_SAVE_ON_BAT = 1;

        # Disk settings
        DISK_APM_LEVEL_ON_AC = "254 254";
        DISK_APM_LEVEL_ON_BAT = "128 128";

        # PCIe power management
        PCIE_ASPM_ON_AC = "default";
        PCIE_ASPM_ON_BAT = "powersupersave";
      };
    };

    # Power management services
    services.upower.enable = true;
    powerManagement.enable = true;
    services.thermald.enable = true;
    services.acpid.enable = true;

    # Touchpad configuration
    services.libinput = {
      enable = true;
      touchpad = {
        tapping = true;
        naturalScrolling = true;
        scrollMethod = "twofinger";
        disableWhileTyping = true;
        clickMethod = "clickfinger";
        accelProfile = "adaptive";
        accelSpeed = "0.3";
      };
    };

    # Backlight control
    programs.light.enable = true;

    # Laptop-specific packages
    environment.systemPackages = with pkgs; [
      powertop
      acpi
      lm_sensors
      upower
      brightnessctl
      light
      wirelesstools
      iw
      smartmontools
      hdparm
    ];

    # Auto-suspend settings
    services.logind = {
      lidSwitch = "suspend";
      lidSwitchExternalPower = "suspend";
      extraConfig = ''
        HandlePowerKey=suspend
        IdleAction=suspend
        IdleActionSec=20m
      '';
    };

    # Mobile broadband
    networking.networkmanager.plugins = with pkgs; [
      networkmanager-openvpn
    ];

    # CPU frequency scaling
    powerManagement.cpuFreqGovernor = "ondemand";

    # Laptop mode kernel settings
    boot.kernel.sysctl = {
      "vm.laptop_mode" = lib.mkDefault 5;
      "vm.dirty_writeback_centisecs" = lib.mkDefault 6000;
      "vm.dirty_expire_centisecs" = lib.mkDefault 6000;
      "vm.swappiness" = lib.mkDefault 10;
    };

    # Firmware updates
    services.fwupd.enable = true;

    # Bluetooth power management
    hardware.bluetooth.powerOnBoot = false;

    # Ensure user is in video group for backlight control
    users.users.silvus.extraGroups = [ "video" ];

    # Udev rules for power management
    services.udev.extraRules = ''
      ACTION=="add", SUBSYSTEM=="usb", TEST=="power/control", ATTR{power/control}="auto"
      ACTION=="add", SUBSYSTEM=="net", KERNEL=="wl*", RUN+="${pkgs.iw}/bin/iw dev $name set power_save on"
      ACTION=="add", SUBSYSTEM=="backlight", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness"
      ACTION=="add", SUBSYSTEM=="backlight", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
    '';
  };
}
