{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    fish
  ];

  # Add to PATH
  home.sessionPath = [
    "${config.home.homeDirectory}/bin"
    "${config.home.homeDirectory}/.local/bin"
    "${config.home.homeDirectory}/.dotfiles/bin"
    "/data/doc/.bin"
  ];

  programs.fish = {
    enable = true;

    # Disable greeting
    interactiveShellInit = ''
      set fish_greeting
    '';

    shellAliases = {
      "cd.." = "cd ..";
      ".." = "cd ..";
      "..." = "cd ../..";
      "...." = "cd ../../../";
      "....." = "cd ../../../../";
      "......" = "cd ../../../../../";

      "l" = "env LC_COLLATE=C ls -lhaFN --color=auto --group-directories-first";

      "e" = "$EDITOR";
      "b" = "$BROWSER";
      "diskusage" = "ncdu";
      "calculator" = "bc -l";
      # "bat" = "batcat";
      "fd" = "fdfind";
      "lz" = "lazygit";
      "y" = "yazi";

      "tree1" = "tree --dirsfirst -ChFLQ 1";
      "tree2" = "tree --dirsfirst -ChFLQ 2";
      "tree3" = "tree --dirsfirst -ChFLQ 3";
      "tree4" = "tree --dirsfirst -ChFLQ 4";
      "tree5" = "tree --dirsfirst -ChFLQ 5";
      "tree6" = "tree --dirsfirst -ChFLQ 6";

      "nvim-lazyvim" = "NVIM_APPNAME=lazyvim nvim";
      "lnvim" = "NVIM_APPNAME=lazyvim nvim";
    };

    shellAbbrs = {
      "n-cg" = "nix-collect-garbage";
      "n-rs" = "sudo nixos-rebuild switch --flake ~/.dotfiles/nixos";
    };

    functions = {
      "mkcd" = ''
        function mkcd --description 'Create a folder and go into it'
            mkdir -p "$argv"
            cd "$argv"
        end
      '';
    };
  };

}

