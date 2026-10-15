# Gladius - Laptop (Thinkpad X280)
{ pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../../modules/profiles/laptop.nix
  ];

  # Per-host, not shared via base.nix.
  system.stateVersion = "26.05";

  # Intel VA-API hardware video decode (fixes CPU-bound YouTube stutter,
  # especially now that TLP throttles the CPU on battery)
  hardware.graphics.extraPackages = with pkgs; [
    intel-media-driver # VA-API driver (iHD) for Broadwell+ Intel iGPUs
    intel-vaapi-driver # VA-API driver (i965) fallback for older Intel iGPUs
    libvdpau-va-gl # VDPAU-over-VA-API wrapper for apps that only speak VDPAU
  ];
  environment.sessionVariables.LIBVA_DRIVER_NAME = "iHD";

}
