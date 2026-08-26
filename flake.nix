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
      # Arch for this flake's own outputs (pkgs, devShells, apps, home-manager)
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };

      overlay-unstable = final: prev: {
        unstable = import nixpkgs-unstable {
          inherit system;
          config.allowUnfree = true;
        };
      };

      # hostSystem is the target arch for this specific host (e.g. "aarch64-linux"
      # for a Raspberry Pi), independent of the flake's own `system` above.
      mkHost =
        hostname: hostSystem:
        nixpkgs.lib.nixosSystem {
          system = hostSystem;

          specialArgs = {
            inherit
              hostname
              mdorg
              movies
              llm-agents
              ;
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
      # Second argument to mkHost is that host's arch (hostSystem), set per-host
      # so hosts on a different arch (e.g. a Raspberry Pi) can be added here.
      nixosConfigurations = {
        nixos-vm = mkHost "nixos-vm" "x86_64-linux";
        claudius = mkHost "claudius" "x86_64-linux";
        noctus = mkHost "noctus" "x86_64-linux";
        virtus = mkHost "virtus" "x86_64-linux";
        servius = mkHost "servius" "x86_64-linux";
        arcus = mkHost "arcus" "x86_64-linux";
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
