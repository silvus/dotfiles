#!/usr/bin/env bash

# Update Xresources
# ------------------------------------------------------
xrdb -merge -I$HOME ~/.Xresources
# Include specific Xresources if needed
if [[ -f "$HOME/.dotfiles/custom/Xresources" ]]; then
	xrdb -merge -I$HOME/.dotfiles/custom/Xresources
fi

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
if [[ -f "$HOME/.dotfiles/custom/autostart_xinit" ]]; then
	source "$HOME/.dotfiles/custom/autostart_xinit"
fi

# Invoke global X session script
# This script will launch defaut desktop, so call it last
# ------------------------------------------------------
if [[ -f "/etc/X11/xinit/xinitrc" ]]; then
	/etc/X11/xinit/xinitrc
fi

# Launch default WM, if not already do in global xinitrc
# ------------------------------------------------------
dbus-run-session awesome
