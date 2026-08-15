# Primus - sandash kiosk (Raspberry Pi 3B + official 7" touchscreen)
{ ... }:

{
  imports = [
    ../../modules/base.nix
    ../../modules/sandash.nix
    ./hardware.nix
  ];
}
