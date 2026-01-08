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
    # ../../modules/security.nix
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
}
