# Hyperion (Ambilight)
{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    hyperion-ng
  ];

  # Serial LED controllers (Adalight, ...)
  # Only for usb controlled leds
  # users.users.silvus.extraGroups = [ "dialout" ];

  networking.firewall.allowedTCPPorts = [
    8090 # Web UI
  ];

  # User service: the screen grabber needs the X session
  systemd.user.services.hyperion = {
    description = "Hyperion Ambilight";
    wantedBy = [ "graphical-session.target" ];
    after = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.hyperion-ng}/bin/hyperiond --userdata %h/.hyperion";
      Restart = "on-failure";
      RestartSec = 5;
    };
  };
}
