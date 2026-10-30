# SolidCraft - Parametric 3D CAD + MCP CLI
{ pkgs, solidcraft, ... }:

{
  environment.systemPackages = [
    solidcraft.packages.${pkgs.stdenv.hostPlatform.system}.default
  ];

  # SolidCraft MCP for the claudius VM (see .mcp.json)
  # The app's control channel only listens on loopback (run it with `solidcraft --control 27878`)
  # Keep its per-launch token in the gitignored custom/ dir, which claudius already mounts
  systemd.tmpfiles.rules = [
    "d /home/silvus/.dotfiles/custom/solidcraft 0700 silvus users -"
    "L+ /home/silvus/.local/share/solidcraft/control - - - - /home/silvus/.dotfiles/custom/solidcraft"
  ];
  # Expose the channel on the Incus bridge (the incusbr0 address), same port
  systemd.user.services.solidcraft-bridge = {
    description = "SolidCraft control channel bridge for claudius";
    wantedBy = [ "default.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.socat}/bin/socat TCP-LISTEN:27878,bind=10.124.88.1,reuseaddr,fork TCP:127.0.0.1:27878";
      # incusbr0 only exists once Incus has started (socket activation): retry until it does
      Restart = "always";
      RestartSec = 10;
    };
    unitConfig.StartLimitIntervalSec = 0;
  };
}
