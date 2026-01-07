{ pkgs, ... }:

let
  keyboard_config = builtins.readFile ./../../approot/etc/keyd/default.conf;

in {
  # Add packages needed for date insertion
  environment.systemPackages = with pkgs; [
    keyd         # To get the binary in the PATH
  ];

  # Looks like there is a keyd group
  users.users.silvus.extraGroups = [
    "keyd"
  ];

  # Script to insert current date
  environment.etc."keyd/insert-date.sh" = {
    text = ''
      #!/usr/bin/env bash
      date_str=$(date +%Y-%m-%d)
      if command -v wtype >/dev/null 2>&1; then
        wtype "$date_str"
      elif command -v xdotool >/dev/null 2>&1; then
        xdotool type --delay 0 "$date_str"
      fi
    '';
    mode = "0755";
  };

  environment.etc."keyd/default.conf".text = keyboard_config;

  # Keyd keyboard remapping configuration
  # Converted from Kanata configuration + custom qwerty-fr XKB layout
  # Provides French accents on QWERTY layout with navigation layers
  # https://wiki.nixos.org/wiki/Keyd
  #
  # Config errors will appear in the log output and can be accessed in the usual way using your system's service manager (e.g sudo journalctl -eu keyd).

  # Include the dotfiles config for now and only keep the service.
  # The order of the entries is not preserved so keyd service fail...
  services.keyd = {
    enable = true;
  };
}
