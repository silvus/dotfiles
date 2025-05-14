# {
#   description = "A very basic flake";

#   inputs = {
#     nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
#   };

#   outputs = { self, nixpkgs }: {

#     packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;

#     packages.x86_64-linux.default = self.packages.x86_64-linux.hello;

#   };
# }
{
  description = "Custom NixOS flake configuration";

  # Declare flake inputs (dependencies)
  inputs = {
    # Use the nixpkgs repository from Github, nixos-unstable branch
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
    # Add Home Manager
    home-manager.url = "github:nix-community/home-manager";
    # Make sure Home Manager uses the same nixpkgs as the system
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  # Declare flake outputs
  outputs = { self, nixpkgs, home-manager }:

    let 
      # Define the target system type
      system = "x86_64-linux";
      # Import nixpkgs for the chosen system
      pkgs = import nixpkgs { inherit system; };

    in
    {
    
      # Config based on hostname
      nixosConfigurations.noctus = nixpkgs.lib.nixosSystem {
        inherit system;
      
        # List of NixOS modules to include
        modules = [
          
          # Shared base config
          ./base.nix

          # Main system config
          ./hosts/noctus/configuration.nix

          # Include the Home Manager NixOS module
          home-manager.nixosModules.home-manager

          # Home Manager specific configuration
          {
            # Use the same pkgs instance for Home Manager as the system
            home-manager.useGlobalPkgs = true;
            # Install user packages via Home Manager
            home-manager.useUserPackages = true;
            # Home manager config for a user
            # home-manager.users.silvus = import ./home.nix;
            home-manager.users.silvus = {
              imports = [
                ./packages/fish.nix
                ./packages/theme.nix
                ./packages/git.nix
                ./packages/lazygit.nix
                ./packages/vscodium.nix
                ./packages/sway.nix
                #./packages/hyprland.nix
                #./packages/waybar.nix
              ];

                # Fix 'command not found' database broken
                # programs.nix-index =
            # {
            #  enable = true;
              # enableFishIntegration = true;
            # };

              home.stateVersion = "24.11"; # Please read the comment before changing. 
            };

          }
        ];
      };

      nixosConfigurations.nixos-vm = nixpkgs.lib.nixosSystem {
        inherit system;
      
        # List of NixOS modules to include
        modules = [

          # Shared base config
          ./base.nix

          # Main system config
          ./hosts/nixos-vm/configuration.nix

          # Include the Home Manager NixOS module
          home-manager.nixosModules.home-manager

          # Home Manager specific configuration
          {
            # Use the same pkgs instance for Home Manager as the system
            home-manager.useGlobalPkgs = true;
            # Install user packages via Home Manager
            home-manager.useUserPackages = true;
            # Home manager config for a user
            # home-manager.users.silvus = import ./home.nix;
            home-manager.users.silvus = {
              imports = [
                ./packages/fish.nix
                # ./packages/theme.nix
                # ./packages/git.nix
                # ./packages/lazygit.nix
                # ./packages/vscodium.nix
                # ./packages/sway.nix
                #./packages/hyprland.nix
                #./packages/waybar.nix
              ];
              home.stateVersion = "24.11"; # Please read the comment before changing. 
            };

          }
        ];
      };
    };

}
