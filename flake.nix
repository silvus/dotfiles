{
  description = "Silvus's NixOS Configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    mdorg.url = "git+ssh://silvus@arcus:/data/git/mdorg";
    movies.url = "git+ssh://silvus@arcus:/data/git/movies";

    llm-agents.url = "github:numtide/llm-agents.nix";
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-unstable,
      home-manager,
      mdorg,
      movies,
      llm-agents,
    }:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };

      # 2026-05-06 fix for Lutris (fail on tests, openldap problem)
      # overlay-unstable = final: prev: {
      #   unstable = import nixpkgs-unstable {
      #     inherit system;
      #     config.allowUnfree = true;
      #   };
      # };
      overlay-unstable = final: prev: {
        unstable = import nixpkgs-unstable {
          inherit system;

          config = {
            allowUnfree = true;
          };

          overlays = [
            (ufinal: uprev: {
              openldap = uprev.openldap.overrideAttrs (_: {
                doCheck = false;
              });
            })
          ];
        };
      };

      mkHost =
        hostname:
        nixpkgs.lib.nixosSystem {
          inherit system;

          specialArgs = {
            inherit hostname mdorg movies llm-agents;
          };
          modules = [
            {
              nixpkgs.overlays = [ overlay-unstable ];
            }

            ./nixos/hosts/${hostname}/configuration.nix

            # Import local custom module
            (if builtins.pathExists ./custom/local.nix then ./custom/local.nix else { })

            {
              networking.hostName = nixpkgs.lib.mkDefault hostname;
            }
          ];
        };

    in
    {

      # NixOs
      nixosConfigurations = {
        nixos-vm = mkHost "nixos-vm";
        claudius = mkHost "claudius";
        noctus = mkHost "noctus";
        virtus = mkHost "virtus";
        servius = mkHost "servius";
        arcus = mkHost "arcus";
      };

      # Home manager on Debian
      homeConfigurations.silvus = home-manager.lib.homeManagerConfiguration {
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
        modules = [
          ./nixos/hosts/debian/home.nix
        ];
      };

      # Run this with `nix develop`
      devShells.${system}.default = nixpkgs.legacyPackages.${system}.mkShell {
        buildInputs = with nixpkgs.legacyPackages.${system}; [
          nixos-rebuild
          git
          vim
          nixfmt
          nil
        ];

        # Runs automatically when you enter the shell
        # shellHook = ''
        #   echo "Available commands:"
        #   echo "  nixos-rebuild switch --flake .#\$(hostname) --sudo"
        #   echo "  nixos-rebuild test --flake .#\$(hostname) --sudo"
        #   echo "  nixfmt *.nix **/*.nix"
        # '';
      };

      # Run this with `nix run github:silvus/dotfiles` (hostname must be defined)
      apps.${system}.default = {
        type = "app";
        program = toString (
          pkgs.writeShellScript "switch-system" ''
            set -euo pipefail

            exec ${pkgs.nixos-rebuild}/bin/nixos-rebuild switch \
              --flake ${self}#$(hostname) \
              --sudo
          ''
        );
      };
    };

}
