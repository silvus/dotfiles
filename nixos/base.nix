{ config, pkgs, ... }:

{
	# Enable networking
	networking.networkmanager.enable = true;

	# Set your time zone.
	time.timeZone = "Europe/Paris";

	# Select internationalisation properties.
	i18n.defaultLocale = "en_US.UTF-8";

	i18n.extraLocaleSettings = {
		LC_ADDRESS = "fr_FR.UTF-8";
		LC_IDENTIFICATION = "fr_FR.UTF-8";
		LC_MEASUREMENT = "fr_FR.UTF-8";
		LC_MONETARY = "fr_FR.UTF-8";
		LC_NAME = "fr_FR.UTF-8";
		LC_NUMERIC = "fr_FR.UTF-8";
		LC_PAPER = "fr_FR.UTF-8";
		LC_TELEPHONE = "fr_FR.UTF-8";
		LC_TIME = "fr_FR.UTF-8";
	};

	# Configure keymap in X11
	services.xserver.xkb = {
		layout = "us";
		variant = "";
	};

	# Define a user account. Don't forget to set a password with ‘passwd’.
	users.users.silvus = {
		isNormalUser = true;
		description = "Silvus";
		extraGroups = [ "networkmanager" "wheel" ];
		packages = with pkgs; [];
		shell = pkgs.fish;
	};

	 # Allow unfree packages
	nixpkgs.config.allowUnfree = true;

	# List packages installed in system profile. To search, run:
	# $ nix search wget
	environment.systemPackages = with pkgs; [
		vim
		wget
		curl
		fish
		git
		python3
		tmux
		dfc
		ncdu
		which
		most
		htop
		btop
		fzf
		bat
		fd
		gawk
		mosh
		unzip
		unrar

		firefox
		mpv
		moc
		playerctl
		rxvt-unicode
		wezterm

		dconf
		# grim
		# mako
		# slurp
		# sway

		# wallust

	];

	programs.vim.enable = true;
	programs.vim.defaultEditor = true;
	# programs.hyprland.enable = true;
	programs.fish.enable = true;
	environment.shells = with pkgs; [ fish ];
	environment.variables.EDITOR = "vim";
	environment.variables.GTK_THEME = "Everforest-Dark-B-LB";
	environment.variables.GTK_ICON_THEME = "Everforest-Dark";

	# https://nixos.wiki/wiki/Fonts
	fonts.packages = with pkgs; [
		nerd-fonts.hack
		nerd-fonts.dejavu-sans-mono
		nerd-fonts.inconsolata
		nerd-fonts.liberation
		nerd-fonts.ubuntu-mono
		nerd-fonts.jetbrains-mono
		noto-fonts
		noto-fonts-emoji
		font-awesome
	];

	# Enable the OpenSSH daemon.
	services.openssh.enable = true;

	# Open ports in the firewall.
	# networking.firewall.allowedTCPPorts = [ ... ];
	# networking.firewall.allowedUDPPorts = [ ... ];
	# Or disable the firewall altogether.
	# networking.firewall.enable = false;

	# This value determines the NixOS release from which the default
	# settings for stateful data, like file locations and database versions
	# on your system were taken. It‘s perfectly fine and recommended to leave
	# this value at the release version of the first install of this system.
	# Before changing this value read the documentation for this option
	# (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
	system.stateVersion = "24.11"; # Did you read the comment?
}
