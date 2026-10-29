# Virtus - Desktop
{
  pkgs,
  config,
  lib,
  ...
}:

{
  imports = [
    ./hardware-configuration.nix
    ../../modules/profiles/desktop.nix
  ];

  # Per-host, not shared via base.nix.
  system.stateVersion = "25.11";

  # Bootloader (dual boot with Debian)
  # https://wiki.nixos.org/wiki/Systemd/boot
  # boot.loader = {
  #   systemd-boot = {
  #     # The sort key used for Nix entries
  #     # sortKey = "a_01";

  #     # Dual boot with Debian
  #     # Use "d" key to change the default entry (the arrow)!
  #     extraEntries."debian.conf" = ''
  #       title Debian
  #       efi   /efi/debian/grubx64.efi
  #     '';
  #       # sort-key z_99_debian
  #   };
  # };

  # Nvidia
  # https://wiki.nixos.org/wiki/NVIDIA
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = false;
    open = false; # Required for a 1060
    nvidiaSettings = true;
    # package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
    # For Nixos 25.11
    # package = config.boot.kernelPackages.nvidiaPackages.production;
    # For Nixos 26.05
    package = config.boot.kernelPackages.nvidiaPackages.legacy_580;
  };
  hardware.graphics.enable = true;
  nixpkgs.config.nvidia.acceptLicense = true;

  environment.systemPackages = with pkgs; [
    keymapp # Voyager keyboard utility
    blink1-tool # Command line client for the blink(1) notification light
  ];

  # Screens
  services.xserver = {
    enable = true;

    # Without Nvidia
    # xrandrHeads = [
    #   {
    #     output = "HDMI-0";
    #     primary = false;
    #     monitorConfig = ''
    #       Option "PreferredMode" "1920x1080"
    #       Option "Position" "320 0"
    #     '';
    #   }
    #   {
    #     output = "DP-2";
    #     primary = true;
    #     monitorConfig = ''
    #       Option "PreferredMode" "2560x1080"
    #       Option "Position" "0 1080"
    #     '';
    #   }
    # ];
  };

  # Autorandr
  services.autorandr = {
    enable = true;
    # No profile matches with the TV plugged (tv is blocked from detection)
    defaultTarget = "stacked";

    profiles.stacked = {
      fingerprint = {
        "DP-2" =
          "00ffffffffffff003103002900000000351d0104a5441c783f64f5ad5049a322135054a10b00714f81c0814081809500a9c0b300d1c07f4800e0a0381f4030203500a91f2100001e000000fd003064545420010a202020202020000000fc0050414e4f20584c0a2020202020000000ff000a202020202020202020202020011a020321f14b010203040590111213141f23097f0783010000681a000001013064009e5a00e0a0381f4030203500a91f2100001a023a801871382d40582c9600a91f2100001ed37800e0a0381f4030203500a91f2100001a00000000000000000000000000000000000000000000000000000000000000000000000000000000bc";
        "HDMI-0" =
          "00ffffffffffff0006b3ae27a1c40000291c0103803c22782aba25a35650a0280e5054bfef00d1c0b30095008180814081c0714f0101023a801871382d40582c450056502100001e000000ff004a414c4d54463035303333370a000000fd00304b185412000a202020202020000000fc00415355532056503237380a2020012102032bf14f0102031112130414050e0f1d1e1f90230917078301000065030c002000681a000001012f4be66842806a703827400820980456502100001a011d007251d01e206e28550056502100001e011d00bc52d01e20b828554056502100001e8c0ad090204031200c40550056502100001800000000000000000000000028";
      };

      config = {
        "DP-2" = {
          enable = true;
          primary = true;
          mode = "2560x1080";
          rate = "60.00";
          position = "0x1080";
        };

        "HDMI-0" = {
          enable = true;
          mode = "1920x1080";
          rate = "60.00";
          position = "320x0";
        };

        # "DP-0".enable = false;
        # "DP-1".enable = false;
        # "DP-3".enable = false;
        # "DVI-D-0".enable = false;
        # "HDMI-1".enable = false;
      };
    };

    # Gaming on the TV (bin/tv_on), the TV replaces the top screen
    profiles.tv = {
      fingerprint = {
        "DP-2" =
          "00ffffffffffff003103002900000000351d0104a5441c783f64f5ad5049a322135054a10b00714f81c0814081809500a9c0b300d1c07f4800e0a0381f4030203500a91f2100001e000000fd003064545420010a202020202020000000fc0050414e4f20584c0a2020202020000000ff000a202020202020202020202020011a020321f14b010203040590111213141f23097f0783010000681a000001013064009e5a00e0a0381f4030203500a91f2100001a023a801871382d40582c9600a91f2100001ed37800e0a0381f4030203500a91f2100001a00000000000000000000000000000000000000000000000000000000000000000000000000000000bc";
        "HDMI-0" =
          "00ffffffffffff0006b3ae27a1c40000291c0103803c22782aba25a35650a0280e5054bfef00d1c0b30095008180814081c0714f0101023a801871382d40582c450056502100001e000000ff004a414c4d54463035303333370a000000fd00304b185412000a202020202020000000fc00415355532056503237380a2020012102032bf14f0102031112130414050e0f1d1e1f90230917078301000065030c002000681a000001012f4be66842806a703827400820980456502100001a011d007251d01e206e28550056502100001e011d00bc52d01e20b828554056502100001e8c0ad090204031200c40550056502100001800000000000000000000000028";
        # LG TV
        "HDMI-1" =
          "00ffffffffffff001e6d010001010101011f010380a05a780aee91a3544c99260f5054a1080031404540614071408180d1c00101010108e80030f2705a80b0588a0040846300001e565e00a0a0a029503020350040846300001e000000fd0018781e873c000a202020202020000000fc004c472054562053534352320a20014802034ff15a6160101f66650413051403021220212215015d5e5f6263643f40290957071507505507006e030c001000b83c2c00800102030468d85dc40178800b02e200cfe305c000e3060d01e20f33000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ba";
      };

      config = {
        "DP-2" = {
          enable = true;
          primary = true;
          mode = "2560x1080";
          rate = "60.00";
          position = "0x1080";
        };

        "HDMI-0".enable = false;

        # Native is 3840x2160, 1080p is lighter for the graphic card
        "HDMI-1" = {
          enable = true;
          mode = "1920x1080";
          rate = "60.00";
          position = "320x0";
        };
      };
    };
  };

  # The TV stays plugged: never auto-detect the tv profile (boot, hotplug),
  # only load it explicitly with bin/tv_on (autorandr --load tv)
  environment.etc."xdg/autorandr/tv/block".source = pkgs.writeShellScript "autorandr-tv-block" "exit 0";

  services.xserver.displayManager.sessionCommands = lib.mkAfter ''
    ${pkgs.autorandr}/bin/autorandr --change --default ${config.services.autorandr.defaultTarget}
  '';
}
