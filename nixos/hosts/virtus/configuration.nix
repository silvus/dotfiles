# Virtus
{ pkgs, config, ... }:

{
  imports = [
    ./hardware-configuration.nix

    ../../modules/base.nix
    ../../modules/keyboard.nix
    ../../modules/syncthing.nix
    ../../modules/desktop_base.nix
    # ../../modules/desktop_sway.nix
    ../../modules/desktop_awesome.nix
    ../../modules/gaming.nix
    ../../modules/printing.nix
    # ../../modules/security.nix
    ../../modules/mnt_movies.nix
    ../../modules/mnt_tvshows.nix
    ../../modules/mnt_torrents.nix
    # ../../modules/mnt_doc.nix
    ../../modules/development.nix
    ../../modules/development_containers.nix
  ];

  # Bootloader (dual boot with Debian)
  # https://wiki.nixos.org/wiki/Systemd/boot
  boot.loader = {
    systemd-boot = {
      # The sort key used for Nix entries
      # sortKey = "a_01";

      # Dual boot with Debian
      # Use "d" key to change the default entry (the arrow)!
      extraEntries."debian.conf" = ''
        title Debian
        efi   /efi/debian/grubx64.efi
      '';
        # sort-key z_99_debian
    };
  };

  # Nvidia
  # https://wiki.nixos.org/wiki/NVIDIA
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia = {
    modesetting.enable = true;
    powerManagement.enable = false;
    open = false; # Required for a 1060
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
  };
  hardware.graphics.enable = true;
  nixpkgs.config.nvidia.acceptLicense = true;

  environment.systemPackages = with pkgs; [
    keymapp
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
  };

  services.xserver.displayManager.sessionCommands = ''
    ${pkgs.autorandr}/bin/autorandr --change
  '';


}
