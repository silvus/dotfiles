#!/usr/bin/env bash

# Update Xresources
# ------------------------------------------------------
xrdb -merge -I$HOME ~/.Xresources

# Keyboard config
# ------------------------------------------------------
# TODO: handle tty layout (/etc/default/keyboard)
setxkbmap -model pc105 -layout fr,gb -variant oss,intl

# https://gist.github.com/jatcwang/ae3b7019f219b8cdc6798329108c9aee

# Menu is compose key
setxkbmap -option 'compose:menu'

# Enable AltGr on Alt Right
setxkbmap -option 'lv3:ralt_switch'

# Non-breakable space character with AltGrSpace
setxkbmap -option 'nbsp:level3'

# Use keyboard LED to show alternative layout
setxkbmap -option 'grp_led:scroll'

# Shift+Caps to switch to another keyboard layout
setxkbmap -option 'grp:shift_caps_toggle'

# Euro sign on 'AltGr+e'
setxkbmap -option 'eurosign:e'

# Swap Capslock for Backspace
# Capslock is now shift left and right
# https://askubuntu.com/a/82734
setxkbmap -option 'caps:backspace'
setxkbmap -option 'shift:both_capslock'


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
