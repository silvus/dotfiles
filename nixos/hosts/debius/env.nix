# Debius - Debian Incus container
# Not a NixOS host: only the user environment installed into silvus's Nix
# profile inside the container.

{ pkgs, llm-agents }:

pkgs.buildEnv {
  name = "debius-env";
  paths = with pkgs; [
    llm-agents.packages.${stdenv.hostPlatform.system}.claude-code
    # llm-agents.packages.${stdenv.hostPlatform.system}.codex

    # Same nix as the other hosts (Debian's one is older), for `nix develop`
    nix
    ripgrep
    fd
    jq
  ];
}
