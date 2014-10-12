#!/usr/bin/env bash

# ------------------------------------------------------------------------- #
# Configure Guake terminal                                                  #
# ------------------------------------------------------------------------- #
_guake_configure() {
	gconftool-2 --set --type bool /apps/guake/general/open_tab_cwd true
    gconftool-2 --set --type bool /apps/guake/general/use_default_font false
    gconftool-2 --set --type bool /apps/guake/general/scroll_keystroke true
    gconftool-2 --set --type int /apps/guake/general/history_size 2048
    gconftool-2 --set --type bool /apps/guake/general/use_scrollbar false
    gconftool-2 --set --type bool /apps/guake/general/window_tabbar true
    gconftool-2 --set --type bool /apps/guake/general/window_losefocus false
    gconftool-2 --set --type bool /apps/guake/general/prompt_on_quit true
    gconftool-2 --set --type bool /apps/guake/general/use_popup false
    gconftool-2 --set --type bool /apps/guake/general/use_trayicon false
    gconftool-2 --set --type int /apps/guake/general/window_height 98

    # Keybinding
    gconftool-2 --set --type string /apps/guake/keybindings/global/show_hide "twosuperior"
}

# ------------------------------------------------------------------------- #
# Set Guake terminal colors                                                 #
# Inspired by https://github.com/Anthony25/gnome-terminal-colors-solarized  #
# ------------------------------------------------------------------------- #

_color_configure() {
	local COLORS="#272728282222:#ffff29292929:#0000cdcd0000:#cdcdcdcd0000:#1e1e9090ffff:#cdcd0000cdcd:#0000cdcdcdcd:#fefefffffefe:#757571715e5e:#ffff29292929:#a6a6e2e22e2e:#ffffffff0000:#6666d9d9efef:#ffff0000ffff:#0000ffffffff:#fefefffffefe"
	local FOREGROUND="#F6F6F5F5EEEE"
	local BOLD="#000000000000"
	local BACKGROUND="#00141a"

	gconftool-2 --set --type string /apps/guake/style/background/color $BACKGROUND
	gconftool-2 --set --type string /apps/guake/style/font/palette $COLORS
	gconftool-2 --set --type string /apps/guake/style/font/color $FOREGROUND
	gconftool-2 --set --type string /apps/guake/style/font/bold $BOLD
}

_color_test() {
	echo "Colors are configured :"
	for i in {1..6}; do
	   tput setaf "$i"
	   echo "This is your beautiful color $i !"
	done
	tput sgr0
}

_guake_configure
_color_configure
_color_test
