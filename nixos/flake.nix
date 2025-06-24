{
  description = "Custom NixOS flake configuration";

  # Declare flake inputs (dependencies)
  inputs = {
    # Use the nixpkgs repository from Github, nixos-unstable branch
    # nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    # Use the nixpkgs repository, stable branch
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    # Add Home Manager
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    # Make sure Home Manager uses the same nixpkgs as the system
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  # Declare flake outputs
  outputs =
    {
      self,
      nixpkgs,
      home-manager,
    }:

    let
      # Define the target system type
      system = "x86_64-linux";
      # Import nixpkgs for the chosen system
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };

    in
    {

      # Config based on hostname
      nixosConfigurations.noctus = nixpkgs.lib.nixosSystem {
        inherit system;

        # List of NixOS modules to include
        modules = [
          # Main system config
          ./hosts/noctus/configuration.nix

          # Shared base config
          ./system/nixos.nix
          ./system/nixos_desktop.nix

          # Include the Home Manager NixOS module
          home-manager.nixosModules.home-manager

          # Home Manager specific configuration
          {
            # Use the same pkgs instance for Home Manager as the system
            home-manager.useGlobalPkgs = true;
            # Install user packages via Home Manager
            home-manager.useUserPackages = true;
            # On activation move existing files by appending the given file extension rather than exiting with an error.
            home-manager.backupFileExtension = "hm_bk";
            # Home manager config for a user
            # home-manager.users.silvus = import ./home.nix;
            home-manager.users.silvus = {
              imports = [
                ./packages/theme.nix
                ./packages/dev.nix

                ./packages/sway.nix
                ./packages/swaync.nix
                ./packages/waybar.nix
              ];

              # Fix 'command not found' database broken
              # programs.nix-index =
              # {
              #  enable = true;
              # enableFishIntegration = true;
              # };

              home.stateVersion = "25.05"; # Please read the comment before changing.
            };

          }
        ];
      };

      nixosConfigurations.nixos-vm = nixpkgs.lib.nixosSystem {
        inherit system;

        # List of NixOS modules to include
        modules = [
          # Main system config
          ./hosts/nixos-vm/configuration.nix

          # Shared base config
          ./system/nixos.nix
          ./system/nixos_desktop.nix

          # Include the Home Manager NixOS module
          home-manager.nixosModules.home-manager

          # Home Manager specific configuration
          {
            # Use the same pkgs instance for Home Manager as the system
            home-manager.useGlobalPkgs = true;
            # Install user packages via Home Manager
            home-manager.useUserPackages = true;
            # On activation move existing files by appending the given file extension rather than exiting with an error.
            home-manager.backupFileExtension = "hm_bk";
            # Home manager config for a user
            # home-manager.users.silvus = import ./home.nix;
            home-manager.users.silvus = {
              imports = [
                ./packages/theme.nix
                ./packages/dev.nix

                ./packages/sway.nix
                ./packages/swaync.nix
                ./packages/waybar.nix
              ];
              home.stateVersion = "25.05"; # Please read the comment before changing.
            };
          }
        ];
      };

      # Home Manager standalone (non-NixOS)
      homeConfigurations.silvus = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          # ./packages/waybar.nix
          {
            home.username = "silvus";
            home.homeDirectory = "/home/silvus";
            home.stateVersion = "25.05"; # Please read the comment before changing.
            # Let Home Manager install and manage itself.
            programs.home-manager.enable = true;

            imports = [
              ./system/debian.nix
              ./packages/dev.nix
            ];
          }
        ];
      };
    };

}

