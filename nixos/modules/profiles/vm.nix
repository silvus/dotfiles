# VM profile
{ modulesPath, ... }:

{
  imports = [
    # Include the default incus configuration.
    "${modulesPath}/virtualisation/incus-virtual-machine.nix"
    ../base.nix
  ];

  # Networking
  networking = {
    dhcpcd.enable = false;
    useDHCP = false;
    useHostResolvConf = false;
  };

  systemd.network = {
    enable = true;
    networks."50-enp5s0" = {
      matchConfig.Name = "enp5s0";
      networkConfig = {
        DHCP = "ipv4";
        IPv6AcceptRA = true;
      };
      linkConfig.RequiredForOnline = "routable";
    };
  };
}
