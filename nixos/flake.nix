{
  description = "Silvus's NixOS Configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager }:
  let
    system = "x86_64-linux";
    lib = nixpkgs.lib;

    mkHost = hostname: lib.nixosSystem {
      inherit system;
      specialArgs = { inherit hostname; };
      modules = [
        ./modules/base.nix
        ./hosts/${hostname}/configuration.nix
        {
          networking.hostName = lib.mkDefault hostname;
        }
      ];
    };

  in
  {
    nixosConfigurations = {
      nixos-vm = mkHost "nixos-vm";
      noctus = mkHost "noctus";
      somnus = mkHost "somnus";
      virtus = mkHost "virtus";
      servius = mkHost "servius";
    };

    homeConfigurations.silvus = home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      modules = [
        {
          home = {
            username = "silvus";
            homeDirectory = "/home/silvus";
            stateVersion = "25.05";
          };
          programs.home-manager.enable = true;
          imports = [ ./modules/development.nix ];
        }
      ];
    };

    # TODO: why? How to improve the first installation with this?
    devShells.${system}.default = nixpkgs.legacyPackages.${system}.mkShell {
      buildInputs = with nixpkgs.legacyPackages.${system}; [
        nixos-rebuild
        git
        vim
        nixfmt-rfc-style
        nil
      ];

      shellHook = ''
        echo "NixOS Configuration Development Environment"
        echo ""
        echo "Available commands:"
        echo "  nixos-rebuild switch --flake .#\$(hostname) --use-remote-sudo"
        echo "  nixos-rebuild test --flake .#\$(hostname) --use-remote-sudo"
        echo "  nixfmt *.nix **/*.nix"
        echo ""
        echo "Current hostname: \$(hostname)"
        echo "Available hosts: nixos-vm, noctus, somnus, virtus, servius"
        echo ""
        echo "Installation commands:"
        echo "  useradd -m -G wheel -s /run/current-system/sw/bin/bash silvus"
        echo "  passwd silvus"
        echo "  su - silvus"
        echo "  nix --extra-experimental-features nix-command --extra-experimental-features flakes run nixpkgs#git -- clone https://github.com/silvus/dotfiles.git ~/.dotfiles"
        echo "  sudo nixos-rebuild switch --flake path:/home/silvus/.dotfiles/nixos#\$(hostname) --use-remote-sudo"
      '';
    };
  };
}
