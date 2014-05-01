#!/bin/bash

# Variables
#----------------------------
DOTFILES_DIR="$HOME/.dotfiles" # dotfiles directory
DOTFILES_BASH="$DOTFILES_DIR/bash"
DOTFILES_VIM="$DOTFILES_DIR/vim"
DOTFILES_SUBLIMETEXT="$DOTFILES_DIR/sublime_text_3"

SUBLIMETEXT_PACKAGECONTROLE_URL="https://sublime.wbond.net/Package%20Control.sublime-package"
SUBLIMETEXT_CONF_DIR="$HOME/.config/sublime-text-3/Packages/User"
SUBLIMETEXT_CONF_KEYMAP="Default (Linux).sublime-keymap"
SUBLIMETEXT_CONF_SETTINGS="Preferences.sublime-settings"
SUBLIMETEXT_CONF_MARKDOWN="Markdown.sublime-settings"
SUBLIMETEXT_CONF_PHP="PHP.sublime-settings"
SUBLIMETEXT_CONF_PYTHON="Python.sublime-settings"
SUBLIMETEXT_CONF_PACKAGECONTROL="Package Control.sublime-settings"

# Function for backup a file and make a symlink
#----------------------------
make_link() {
	local file_name="$1"
	local dot_file_path="$2"
	local file_path="$3"

	#  If file_path is not already a symlink or doesn't exist
	if [ ! -L "$file_path" ]; then

		# File already exist, make backup
		if [ -f "$file_path" ]; then
			echo "backup current $file_name in $DOTFILES_DIR/backup/$file_name.bak"
			mv "$file_path" "$DOTFILES_DIR/backup/$file_name.bak"
		fi

		# Make symlink
		ln -sv "$dot_file_path" "$file_path"
	else
		echo "$file_name is already install"
	fi

}

# Bashrc
#----------------------------
make_link "bash_aliases" "$DOTFILES_BASH/bash_aliases" "$HOME/.bash_aliases"

# Vim
#----------------------------
# Create folders if necessary
if [ ! -d "$HOME/.vim/backup" ]; then
	mkdir -p "$HOME/.vim/backup"
fi
if [ ! -d "$HOME/.vim/swap" ]; then
	mkdir -p "$HOME/.vim/swap"
fi
if [ ! -d "$HOME/.vim/colors" ]; then
	mkdir -p "$HOME/.vim/colors"
fi

make_link "molokai.vim" "$DOTFILES_VIM/colors/molokai.vim" "$HOME/.vim/colors/molokai.vim"
make_link "vimrc" "$DOTFILES_VIM/vimrc" "$HOME/.vimrc"

# Sublime Text 3
#----------------------------
if [ -d "/opt/sublime_text" ]; then
	echo "Installing config for Sublime Text 3"

	# Package Control
	if [ ! -f "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_PACKAGECONTROL" ]; then
		echo "Download Package Control"
		wget -P "$SUBLIMETEXT_CONF_DIR" "$SUBLIMETEXT_PACKAGECONTROLE_URL"
	fi

	make_link "$SUBLIMETEXT_CONF_KEYMAP" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_KEYMAP" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_KEYMAP"
	make_link "$SUBLIMETEXT_CONF_SETTINGS" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_SETTINGS" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_SETTINGS"
	make_link "$SUBLIMETEXT_CONF_MARKDOWN" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_MARKDOWN" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_MARKDOWN"
	make_link "$SUBLIMETEXT_CONF_PHP" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_PHP" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_PHP"
	make_link "$SUBLIMETEXT_CONF_PYTHON" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_PYTHON" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_PYTHON"
	make_link "$SUBLIMETEXT_CONF_PACKAGECONTROL" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_PACKAGECONTROL" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_PACKAGECONTROL"

fi

# End
#----------------------------
echo "All done !"
