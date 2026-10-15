# Noctus - Laptop (Thinkpad X260)
{ pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../modules/profiles/laptop.nix
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

  # Intel VA-API hardware video decode (fixes CPU-bound YouTube stutter,
  # especially now that TLP throttles the CPU on battery)
  hardware.graphics.extraPackages = with pkgs; [
    intel-media-driver # VA-API driver (iHD) for Broadwell+ Intel iGPUs
    intel-vaapi-driver # VA-API driver (i965) fallback for older Intel iGPUs
    libvdpau-va-gl # VDPAU-over-VA-API wrapper for apps that only speak VDPAU
  ];
  environment.sessionVariables.LIBVA_DRIVER_NAME = "iHD";

}
