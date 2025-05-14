{ config, pkgs, ... }:

{
	
	home.sessionVariables = {
		XCURSOR_THEME = "Adwaita";
		XCURSOR_SIZE = "24";
	};
	home.pointerCursor = {
		gtk.enable = true;
		# x11.enable = true;
		package = pkgs.bibata-cursors;
		name = "Bibata-Modern-Classic";
		size = 16;
	};

	gtk =  {
		enable = true;
		theme = {
			name = "Everforest-Dark-B-LB";
			package = pkgs.everforest-gtk-theme;
		};
		iconTheme = {
			name = "Everforest-Dark";
			package = pkgs.everforest-gtk-theme;
		};
		cursorTheme = {
			name = "Adwaita";
			size = 24;
		};
	};
  
}
