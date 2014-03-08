#!/bin/bash

SUBLIMETEXT_CONF_DIR="$HOME/.config/sublime-text-3/Packages/User"

SUBLIMETEXT_CONF_KEYMAP="Default (Linux).sublime-keymap"
SUBLIMETEXT_CONF_SETTINGS="Preferences.sublime-settings"

# Backup config files
# -------------------------------------------------------------------
if [ -f "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_KEYMAP" ]; then
	# echo "backup current Sublime Text Keymap in $DOTFILES_DIR/backup"
	mv "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_KEYMAP" "$DOTFILES_DIR/backup/$SUBLIMETEXT_CONF_KEYMAP.bak"
fi
if [ -f "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_SETTINGS" ]; then
	# echo "backup current Sublime Text Settings in $DOTFILES_DIR/backup"
	mv "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_SETTINGS" "$DOTFILES_DIR/backup/$SUBLIMETEXT_CONF_SETTINGS.bak"
fi

# Make symlinks
# -------------------------------------------------------------------
ln -s "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_KEYMAP" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_KEYMAP"
ln -s "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_SETTINGS" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_SETTINGS"
