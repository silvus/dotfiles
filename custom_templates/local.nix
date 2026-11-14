# Environment specific configuration
# Sourced from flake.nix
#
{ lib, hostname, ... }:

{
  # networking.domain = "exemple.net";
  #
}

# SolidCraft MCP CLI in the claudius VM
# optionalAttrs, not mkIf: other hosts don't declare this option at all.
# // lib.optionalAttrs (hostname == "claudius") {
  # claudius.solidcraft = true;
# }
#
