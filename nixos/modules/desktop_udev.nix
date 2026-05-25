{ pkgs, ... }:

{
  services.udev.packages = [
    # Yubikey
    pkgs.yubikey-personalization

    (pkgs.writeTextFile {
      name = "voyager-udev-rules";
      text = ''KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="3297", ATTRS{idProduct}=="1977", MODE="0666"'';
      destination = "/etc/udev/rules.d/70-voyager.rules";
    })
    (pkgs.writeTextFile {
      name = "nuphyair60-udev-rules";
      text = ''SUBSYSTEM=="hidraw", ATTRS{idVendor}=="19f5", ATTRS{idProduct}=="3255", MODE="0666"'';
      destination = "/etc/udev/rules.d/70-nuphy-air-60.rules";
    })
    (pkgs.writeTextFile {
      name = "nyphyair75-udev-rules";
      text = ''SUBSYSTEM=="hidraw", ATTRS{idVendor}=="19f5", ATTRS{idProduct}=="3246", MODE="0666"'';
      destination = "/etc/udev/rules.d/70-nuphy-air-75.rules";
    })
    (pkgs.writeTextFile {
      name = "xteinkx4-udev-rules";
      text = ''SUBSYSTEM=="tty", ATTRS{idVendor}=="303a", ATTRS{idProduct}=="1001", MODE="0666"'';
      destination = "/etc/udev/rules.d/75-xteink-x4.rules";
    })
    # (pkgs.writeTextFile {
    #   name = "blink1-udev-rules";
    #   text = ''SUBSYSTEM=="hidraw", ATTRS{idVendor}=="27b8", ATTRS{idProduct}=="01ed", MODE="0666"'';
    #   destination = "/etc/udev/rules.d/51-blink1.rules";
    # })
    (pkgs.writeTextFile {
      name = "blink1-udev-rules";
      text = ''
        SUBSYSTEM=="usb", ATTR{idVendor}=="27b8", ATTR{idProduct}=="01ed", TAG+="uaccess", SYMLINK+="blink1_%n"
        SUBSYSTEM=="hidraw", ATTRS{idVendor}=="27b8", ATTRS{idProduct}=="01ed", TAG+="uaccess"
      '';
      destination = "/etc/udev/rules.d/51-blink1.rules";
    })

    (pkgs.writeTextFile {
      name = "pixel8a-udev-rules";
      text = ''SUBSYSTEM=="tty", ATTRS{idVendor}=="18d1", ATTRS{idProduct}=="4ee1", MODE="0666"'';
      destination = "/etc/udev/rules.d/76-pixel8a.rules";
    })
  ];

}
