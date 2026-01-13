{ pkgs, ... }:

let
  keyboard_config = builtins.readFile ./../../approot/etc/keyd/default.conf;

in {

  # Console keymap
  console = {
    # keyMap = "us";
    # Derive from X11 value
    useXkbConfig = true;
  };

  # Configure keymap in X11
  services.xserver = {
    enable = true;  # Make sure this is set

    xkb = {
      model = "pc105";
      layout = "custom,us";
      variant = "";
      # compose:rwin - Right Win as compose key
      # grp_led:scroll - Scroll lock LED indicates layout
      # lv3:ralt_switch - Right Alt for level 3
      # lv5:caps_switch - Caps Lock for level 5
      # nbsp:none - Disable non-breaking space
      # shift:both_capslock - Both shifts toggle Caps Lock
      options = "compose:rwin,grp_led:scroll,lv3:ralt_switch,lv5:caps_switch,nbsp:none,shift:both_capslock";
      # Define your custom layout
      extraLayouts.custom = {
        description = "Custom QWERTY-FR layout";
        languages = [ "eng" "fra" ];
        symbolsFile = ./../../approot/usr/share/X11/xkb/symbols/custom;
      };

    };

  };

  # Keyd keyboard remapping configuration
  # Converted from Kanata configuration + custom qwerty-fr XKB layout
  # Provides French accents on QWERTY layout with navigation layers
  # https://wiki.nixos.org/wiki/Keyd
  # Config errors will appear in the log output and can be accessed in the usual way using your system's service manager (e.g sudo journalctl -eu keyd).

  # Include the dotfiles config for now and only keep the service.
  # The order of the entries is not preserved so keyd service fail...
  environment.systemPackages = with pkgs; [
    keyd            # To get the binary in the PATH
    xorg.setxkbmap  # Needed for layout setup
  ];

  # Looks like there is a keyd group
  users.groups.keyd = {};
  users.users.silvus.extraGroups = [
    "keyd"
  ];

  environment.etc."keyd/default.conf".text = keyboard_config;

  # services.keyd = {
  #   enable = true;
  # };
  # "setgid: Operation not permitted" with the default service. Create the service manually:
  systemd.services.keyd = {
    description = "Keyd remapping daemon";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.keyd}/bin/keyd";
      Restart = "on-failure";

      # Set PATH explicitly to get keyd command to work
      Environment = "PATH=/run/current-system/sw/bin:${pkgs.keyd}/bin";
    };
  };
}
