#!/usr/bin/env bash

# ------------------------------------------------------------------------- #
# Set Guake terminal colors                                                 #
# Inspired by https://github.com/Anthony25/gnome-terminal-colors-solarized  #
# ------------------------------------------------------------------------- #

_color_configure() {
	local COLORS="#272728282222:#ffff29292929:#0000cdcd0000:#cdcdcdcd0000:#1e1e9090ffff:#cdcd0000cdcd:#0000cdcdcdcd:#fefefffffefe:#757571715e5e:#ffff29292929:#a6a6e2e22e2e:#ffffffff0000:#6666d9d9efef:#ffff0000ffff:#0000ffffffff:#fefefffffefe"
	local FOREGROUND="#F6F6F5F5EEEE"
	local BOLD="#000000000000"
	local BACKGROUND="#00141a"

	gconftool-2 -s -t string /apps/guake/style/background/color $BACKGROUND
	gconftool-2 -s -t string /apps/guake/style/font/palette $COLORS
	gconftool-2 -s -t string /apps/guake/style/font/color $FOREGROUND
	gconftool-2 -s -t string /apps/guake/style/font/bold $BOLD
}

_color_test() {
	echo "Colors are configured :"
	for i in {1..6}; do
	   tput setaf "$i"
	   echo "This is your beautiful color $i !"
	done
	tput sgr0
}

_color_configure
_color_test
