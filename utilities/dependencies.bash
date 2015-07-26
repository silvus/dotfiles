#!/usr/bin/env bash

sudo apt-get update

_install_server() {
	sudo apt-get install \
		curl `# Play with HTTP` \
		wget `# Downloader` \
		rsync `# Synchronisation` \
		htop `# Better top command` \
		dfc `# Disk space` \
		iftop `# Traffic htop-like` \
		build-essential `# Compiling things` \
		cmake `# YouCompleteMe dependency` \
		python-dev `# Python headers` \
		ack-grep `# Search` \
		vim `# Text editor` \
		tmux `# Split terminal` \
		urlview `# Extract urls` \
		ranger highlight caca-utils `# File manager and dependencies` \
		newsbeuter `# Rss reader` \
		lynx `# Web browser`
}

_install_desktop() {
	# Mpv PPA
	sudo add-apt-repository ppa:mc3man/mpv-tests

	sudo apt-get install \
		mpv `# [PPA] video player` \
		xclip `# Add to clipboard` \
		scrot `# Make screenshot` \
		pass `# Passwords manager` \
		feh `# Image viewer` \
		moc `# Music player` \
		i3 `# WM` \
		conky `# i3 bar` \
		libnotify-bin  `# Notifications`
}

OPTION=$(whiptail --title "Dotfiles" --menu "Choose your configuration" 10 50 3 \
"1" "Server" \
"2" "Desktop" \
"3" "Server & Desktop" 3>&1 1>&2 2>&3)

exitstatus="$?"
if [[ "$exitstatus" == 0 ]]; then
    case "$OPTION" in
        1)
            _install_server
            ;;
        2)
            _install_desktop
            ;;
        3)
            _install_server
			_install_desktop
            ;;
    esac
else
    echo "Abort"
    exit 1
fi

exit 0
