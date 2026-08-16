# Sandash kiosk hardware: Raspberry Pi 3B + official 7" touchscreen (aarch64).
# Shared as-is by secundus

# Adapted from what a plain RPi3 needs out of
# https://github.com/NixOS/nixos-hardware/blob/master/raspberry-pi/3/ ,
# vendored directly instead of taking nixos-hardware as a flake input.
{
  pkgs,
  lib,
  modulesPath,
  sandash,
  ...
}:

let
  system = pkgs.stdenv.hostPlatform.system;
  sandashPkg = sandash.packages.${system}.default;

  kioskShell = pkgs.writeShellScriptBin "kiosk-shell" ''
    export SLINT_BACKEND=linuxkms
    while true; do
      ${lib.getExe sandashPkg}
      sleep 1
    done
  '';

  uboot = pkgs.ubootRaspberryPi3_64bit;

  configTxt = pkgs.writeText "config.txt" ''
    [all]
    kernel=u-boot.bin
    arm_64bit=1
    enable_uart=1
    camera_auto_detect=1
    display_auto_detect=1
    disable_overscan=1
    arm_boost=1
    dtparam=audio=on
    dtoverlay=vc4-kms-v3d
  '';

  # Stage the firmware-partition files the GPU firmware needs before Linux
  # starts: boot code, vendor device trees/overlays, U-Boot, and config.txt.
  # Used both at SD-image build time and to refresh a running system's
  # firmware partition on `nixos-rebuild switch`.
  installFirmware = target: ''
    mkdir -p "${target}/overlays"
    cp ${pkgs.raspberrypifw}/share/raspberrypi/boot/bootcode.bin "${target}/"
    cp ${pkgs.raspberrypifw}/share/raspberrypi/boot/fixup*.dat "${target}/"
    cp ${pkgs.raspberrypifw}/share/raspberrypi/boot/start*.elf "${target}/"
    cp ${pkgs.raspberrypifw}/share/raspberrypi/boot/*.dtb "${target}/"
    cp ${pkgs.raspberrypifw}/share/raspberrypi/boot/overlays/*.dtbo "${target}/overlays/"
    cp ${uboot}/u-boot.bin "${target}/u-boot.bin"
    cp ${configTxt} "${target}/config.txt"
  '';
in
{
  imports = [ "${modulesPath}/installer/sd-card/sd-image-aarch64.nix" ];

  # Vendor RPi kernel (vc4/KMS support for this hardware). nixpkgs flags this
  # series as deprecated in favour of nixos-hardware; revisit if it's ever
  # removed outright.
  boot.kernelPackages = pkgs.linuxKernel.packages.linux_rpi3;
  boot.initrd.availableKernelModules = [
    "usb-storage"
    "usbhid"
    "vc4"
  ];

  boot.loader.generic-extlinux-compatible.useGenerationDeviceTree = false;

  # base.nix defaults to systemd-boot/EFI; RPi3 has neither.
  boot.loader.systemd-boot.enable = lib.mkForce false;
  boot.loader.efi.canTouchEfiVariables = lib.mkForce false;

  # Quiet, blank-cursor boot straight through to the app. ttyS0 stays wired
  # up for debugging over the GPIO UART.
  boot.consoleLogLevel = 3;
  boot.kernelParams = [
    "console=ttyS0,115200n8"
    "console=tty0"
    "quiet"
    "vt.global_cursor_default=0"
  ];

  # Onboard wifi/bluetooth firmware, in case a kiosk isn't on wired ethernet.
  hardware.enableRedistributableFirmware = true;

  sdImage.populateFirmwareCommands = installFirmware "$NIX_BUILD_TOP/firmware";

  # The firmware partition isn't touched by `nixos-rebuild switch` by
  # default; refresh it in place so kernel/config.txt updates don't need a
  # re-flash.
  system.activationScripts.raspberry-pi-firmware = lib.stringAfter [ "specialfs" ] ''
    if mountpoint -q /boot/firmware; then
      ${installFirmware "/boot/firmware"}
    fi
  '';

  # sd-image-aarch64.nix mounts this "noauto", so it's never actually
  # mounted at boot -- silently disabling the activation script above.
  # Mount it normally (still "nofail") so firmware refreshes happen.
  fileSystems."/boot/firmware".options = lib.mkForce [ "nofail" ];

  # SD card wear reduction: always-on kiosk, nothing needs to survive a reboot.

  # journald still fsyncs every line to disk despite base.nix's size caps.
  services.journald.extraConfig = lib.mkForce ''
    Storage=volatile
    RuntimeMaxUse=50M
  '';

  # Avoids an atime write on every file read.
  fileSystems."/".options = [ "noatime" ];

  # Don't let a crash-looping kiosk app fill the card with core dumps.
  systemd.coredump.enable = false;

  # Rebuilds here are infrequent; weekly GC just wastes a scan most weeks.
  nix.gc.dates = lib.mkForce "monthly";

  # Dedicated low-priv user for the kiosk session. Group memberships are a
  # fallback for DRM/input device access; logind normally grants the active
  # seat0 session ACLs to these devices automatically.
  users.groups.kiosk = { };
  users.users.kiosk = {
    isSystemUser = true;
    group = "kiosk";
    extraGroups = [
      "video"
      "input"
      "render"
    ];
    home = "/var/lib/kiosk";
    createHome = true;
    shell = "${kioskShell}/bin/kiosk-shell";
  };
  environment.shells = [ "${kioskShell}/bin/kiosk-shell" ];

  services.getty.autologinUser = "kiosk";
}
