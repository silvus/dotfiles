#!/usr/bin/env bash

# Update Xresources
# ------------------------------------------------------
xrdb -merge -I$HOME ~/.Xresources

# Keyboard config
# ------------------------------------------------------
setxkbmap -model pc105 -layout fr,gb -variant oss,intl -option "grp:shift_caps_toggle,grp_led:scroll,nbsp:level4,lv3:ralt_switch,compose:menu,eurosign:e"
# TODO: -variant intl or altgr-intl ?
# TODO: handle tty layout (/etc/default/keyboard)

# https://gist.github.com/jatcwang/ae3b7019f219b8cdc6798329108c9aee
# grp:shift_caps_toggle : Shift+Caps Lock to switch to another layout
# grp_led:scroll : Use keyboard LED to show alternative layout
# nbsp:level3 : Usual space on AltGr + Space
# nbsp:level4 : Usual space on Shift + AltGr Space
# lv3:ralt_switch : Alt_R to AltGr
# compose:menu : Menu is compose key
# eurosign:e : AltGr+e to €

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
