{
  description = "Silvus's NixOS Configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
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
        ./nixos/hosts/${hostname}/configuration.nix
        {
          networking.hostName = lib.mkDefault hostname;
        }
      ];
    };

  in
  {
    # NixOs
    nixosConfigurations = {
      nixos-vm = mkHost "nixos-vm";
      noctus = mkHost "noctus";
      somnus = mkHost "somnus";
      virtus = mkHost "virtus";
      servius = mkHost "servius";
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
        nixfmt-rfc-style
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
  };
}
