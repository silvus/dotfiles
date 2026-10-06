{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # Core development tools
    cmake
    pkg-config
    gcc
    clang

    # Formatter
    dprint
    # dprint-plugins.dprint-plugin-toml
    # dprint-plugins.dprint-plugin-json
    # dprint-plugins.g-plane-malva
    # dprint-plugins.dprint-plugin-ruff
    # dprint-plugins.g-plane-markup_fmt
    # dprint-plugins.g-plane-pretty_yaml
    dprint-plugins.dprint-plugin-markdown

    # JS
    # bun

    # Utilities
    lsof
    # iftop
    # nethogs
    dig

    # Static generator
    # zola

    # Android
    # NIXPKGS_ALLOW_UNFREE=1 nix-shell -p androidenv.androidPkgs.platform-tools
    # android-tools
  ];

  # Load flake dev shells on cd, with GC roots in <project>/.direnv/
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    settings.global = {
      # Disable the "direnv is taking a while to execute" warning
      warn_timeout = "0s";
      # Discreet grey logs
      log_format =
        # Escape characters
        let
          esc = builtins.fromJSON ''"\u001b"'';
        in
        "${esc}[90mdirenv: %s${esc}[0m";
    };
  };

}
