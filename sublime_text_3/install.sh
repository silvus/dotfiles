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

# TODO : Install or update plugins
# -------------------------------------------------------------------
# Alignement
# Git Gutter (https://github.com/jisaacks/GitGutter)
# JS Format
# Language French (https://github.com/superbob/SublimeTextLanguageFrench)
# Laravel Blade Highlighter (https://github.com/Medalink/laravel-blade)
# LESS
# Package-Control (https://sublime.wbond.net/installation#st3)
# Sidebar Enhancements (https://github.com/titoBouzout/SideBarEnhancements)
# DocBlockr (https://github.com/spadgos/sublime-jsdocs)
# PHP Companion (https://sublime.wbond.net/packages/PHP%20Companion)
# PHP Completions Kit (https://sublime.wbond.net/packages/PHP%20Completions%20Kit)

# sublimelint (https://github.com/lunixbochs/sublimelint) [Actif]
# ou
# SublimeLinter (https://github.com/SublimeLinter/SublimeLinter3)
# + SublimeLinter-[...] (https://github.com/SublimeLinter?page=2)


# sublime-laravelgenerator (https://github.com/gnarula/sublime-laravelgenerator)
# Laravel IDE Helper Generator (https://github.com/barryvdh/laravel-ide-helper)

# Laravel 4 Artisan https://sublime.wbond.net/packages/Laravel%204%20Artisan
# Laravel 4 Facades https://sublime.wbond.net/packages/Laravel%204%20Facades

# Phpcs (https://sublime.wbond.net/packages/Phpcs)
