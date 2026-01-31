# Virtus - Development Desktop Configuration
{ ... }:

{
  imports = [
    ./hardware-configuration.nix

    ../../modules/base.nix
    ../../modules/keyboard.nix
    ../../modules/syncthing.nix
    # Enable desktop modules
    ../../modules/desktop_base.nix
    # ../../modules/desktop_sway.nix
    ../../modules/desktop_awesome.nix
    ../../modules/gaming.nix
    ../../modules/printing.nix
    # ../../modules/security.nix
    ../../modules/mnt_movies.nix
    ../../modules/mnt_tvshows.nix
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
  hardware.nvidia.open = true;  # Open-source kernel modules are preferred

  environment.systemPackages = with pkgs; [
    keymapp
  ];

  # Screens
  services.xserver = {
    enable = true;
    xrandrHeads = [
      {
        output = "HDMI-0";
        primary = false;
        monitorConfig = ''
          Option "PreferredMode" "1920x1080"
          Option "Position" "320 0"
        '';
      }
      {
        output = "DP-2";
        primary = true;
        monitorConfig = ''
          Option "PreferredMode" "2560x1080"
          Option "Position" "0 1080"
        '';
      }
    ];
  };
}
