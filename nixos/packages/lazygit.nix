{ config, pkgs, ... }:

{

	home.packages = with pkgs; [
    lazygit
	];

  programs.lazygit = {
    enable = true;
    settings = {
      gui.showRandomTip = false;
      update.method = "never";
    };
  };
  
}
