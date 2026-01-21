{ pkgs, ... }:

{
  # Printing
  # https://wiki.nixos.org/wiki/Printing
  services.printing = {
    enable = true;
    drivers = with pkgs; [
      brlaser # Drivers for some Brother printers
      brgenml1lpr # Generic drivers for more Brother printers
      gutenprint # Drivers for many different printers
      cups-filters # Auto-discovery of network printers
      cups-browsed # Auto-discovery of network printers
    ];
  };
  # Auto-discovery of network printers
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };
}
