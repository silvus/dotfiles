{ config, pkgs, ... }:

{

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.silvus = {
    isNormalUser = true;
    description = "Silvus";
    shell = pkgs.fish;
    extraGroups = [
      "tty"
      "audio"
      "video"
      "networkmanager"
      "floppy"
      "cdrom"
      "disk"
      # "systemd-journal-gateway"
      # "systemd-network"
      # "systemd-resolve"
      # "systemd-timesync"
      "wheel" # to be sudoer
    ];
  };


  # Enable automatic login for the user.
  # services.getty.autologinUser = "silvus";

}
