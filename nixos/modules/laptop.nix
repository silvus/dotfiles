{ pkgs, ... }:

{
  # Laptop-specific packages
  environment.systemPackages = with pkgs; [
    lxqt.lxqt-powermanagement # GUI power-management tray applet
    powertop # Power usage analysis and tuning
    acpi # ACPI status querying (battery, thermal)
    # lm_sensors                 # Hardware sensors monitoring
    # upower                     # Power device management backend
    brightnessctl # Backlight control utility
    # wirelesstools              # Legacy wireless tools (iwconfig, etc.)
    iw # Modern wireless management tool
    # smartmontools              # SMART monitoring for storage devices
    # hdparm                     # HDD/SSD parameter and performance tuning
  ];

  # Laptop lid
  services.logind.settings.Login.HandleLidSwitch = "suspend";
  services.logind.settings.Login.HandleLidSwitchExternalPower = "suspend";

  # Systemd user service for lxqt-powermanagement
  systemd.user.services.lxqt-powermanagement = {
    description = "LXQt Power Management";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.lxqt.lxqt-powermanagement}/bin/lxqt-powermanagement";
    };
  };

  # TLP Power Management
  services.tlp = {
    enable = true;
    settings = {
      # Battery thresholds
      # START_CHARGE_THRESH_BAT0 = 20;
      # STOP_CHARGE_THRESH_BAT0 = 80;

      # CPU scaling (left to intel_pstate/EPP below; only relevant with acpi-cpufreq)
      # CPU_SCALING_GOVERNOR_ON_AC = "performance";
      # CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

      # CPU energy/performance policy (EPP hint on intel_pstate)
      CPU_ENERGY_PERF_POLICY_ON_AC = "performance";
      CPU_ENERGY_PERF_POLICY_ON_BAT = "power";

      # Platform profile (firmware-level power/perf profile)
      PLATFORM_PROFILE_ON_AC = "performance";
      PLATFORM_PROFILE_ON_BAT = "low-power";

      # Runtime power management (PCI/USB device idling)
      RUNTIME_PM_ON_AC = "on";
      RUNTIME_PM_ON_BAT = "auto";

      # USB autosuspend
      USB_AUTOSUSPEND = 1;

      # WiFi power saving
      # WIFI_PWR_ON_AC = "off";
      # WIFI_PWR_ON_BAT = "on";

      # Sound power saving
      # SOUND_POWER_SAVE_ON_AC = 0;
      # SOUND_POWER_SAVE_ON_BAT = 1;

      # Disk settings
      # DISK_APM_LEVEL_ON_AC = "254 254";
      # DISK_APM_LEVEL_ON_BAT = "128 128";

      # PCIe power management — left at TLP default (skipping powersupersave
      # on battery to avoid wifi-stability issues on some chipsets)
      # PCIE_ASPM_ON_AC = "default";
      # PCIE_ASPM_ON_BAT = "powersupersave";
    };
  };

  # Power management services
  services.upower.enable = true; # Battery/power status backend (used by lxqt-powermanagement, notifications, etc.)
  services.thermald.enable = true; # Intel thermal daemon; proactively throttles to avoid hard thermal cutoffs
  services.acpid.enable = true; # ACPI event daemon (power button, lid, etc.)

  # Touchpad configuration
  # services.libinput = {
  #   enable = true;
  #   touchpad = {
  #     tapping = true;
  #     naturalScrolling = true;
  #     scrollMethod = "twofinger";
  #     disableWhileTyping = true;
  #     clickMethod = "clickfinger";
  #     accelProfile = "adaptive";
  #     accelSpeed = "0.3";
  #   };
  # };

  # Laptop mode kernel settings
  # boot.kernel.sysctl = {
  #   "vm.laptop_mode" = lib.mkDefault 5;
  #   "vm.dirty_writeback_centisecs" = lib.mkDefault 6000;
  #   "vm.dirty_expire_centisecs" = lib.mkDefault 6000;
  #   "vm.swappiness" = lib.mkDefault 10;
  # };

  # Firmware updates
  # services.fwupd.enable = true;

  # Bluetooth power management
  # hardware.bluetooth.powerOnBoot = false;
}
