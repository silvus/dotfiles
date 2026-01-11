{ ... }:

{
  fileSystems."/data/series" = {
    device = "192.168.1.20:/data/series"; # Servius
    fsType = "nfs4";                              # Use NFSv4 for Debian 12 compatibility.
    options = [
      "nofail"                 # Never block boot or report errors if unavailable.
      "x-systemd.automount"    # Create an on-demand automount unit.
      "x-systemd.idle-timeout=600"    # Unmount after 600 seconds idle.
      "x-systemd.mount-timeout=10s"   # Limit initial mount attempts to 10 seconds.
      "soft"                   # Fail quickly on timeout instead of hanging.
      "retrans=3"              # Retry packet transmission three times.
      "timeo=50"               # Set RPC timeout to 5 seconds (timeo is in tenths).
    ];
  };
}
