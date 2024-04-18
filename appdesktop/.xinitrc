#!/usr/bin/env bash

# Update Xresources
# ------------------------------------------------------
xrdb -merge -I$HOME ~/.Xresources

# Keyboard config
# ------------------------------------------------------
# if [[ -f "$HOME/.dotfiles/bin/keyboardmap" ]]; then
# 	source "$HOME/.dotfiles/bin/keyboardmap"
# fi
if [[ -f "$HOME/.dotfiles/bin/keyboardmapsimple" ]]; then
	source "$HOME/.dotfiles/bin/keyboardmapsimple"
fi

# Sleep time
# ------------------------------------------------------
xset s 3600 3600
xset dpms 3600 3600 3600


# Include specific screen (xrandr) configuration script if needed
# ------------------------------------------------------
if [[ -f "$HOME/.dotfiles_custom/autostart_xinit" ]]; then
	source "$HOME/.dotfiles_custom/autostart_xinit"
fi

# Launch default WM, if not already do in autostart_xinit
# ------------------------------------------------------
exec /usr/bin/awesome
