#!/bin/bash

# Variables
# --------------------------------------------------------
DOTFILES_DIR="$HOME/.dotfiles" # dotfiles directory
DOTFILES_BASH="$DOTFILES_DIR/bash"
DOTFILES_VIM="$DOTFILES_DIR/vim"
DOTFILES_TMUX="$DOTFILES_DIR/tmux"
DOTFILES_LYNX="$DOTFILES_DIR/lynx"
DOTFILES_NEWSBEUTER="$DOTFILES_DIR/newsbeuter"
DOTFILES_SUBLIMETEXT="$DOTFILES_DIR/sublime_text_3"

SUBLIMETEXT_CONF_DIR="$HOME/.config/sublime-text-3/Packages/User"
SUBLIMETEXT_CONF_KEYMAP="Default (Linux).sublime-keymap"
SUBLIMETEXT_CONF_SETTINGS="Preferences.sublime-settings"
SUBLIMETEXT_CONF_SIDEBAR="Side Bar.sublime-settings"
SUBLIMETEXT_CONF_MARKDOWN="Markdown.sublime-settings"
SUBLIMETEXT_CONF_PHP="PHP.sublime-settings"
SUBLIMETEXT_CONF_PYTHON="Python.sublime-settings"

# Function for backup a file and make a symlink
# --------------------------------------------------------
make_link() {
	local file_name="$1"
	local dot_file_path="$2"
	local file_path="$3"

	# Colors
	local green=$(tput setaf 2)
	local yellow=$(tput setaf 3)
	local reset=$(tput sgr0)

	#  If file_path is not already a symlink or doesn't exist
	if [ ! -L "$file_path" ]; then

		# File already exist, make backup
		if [ -f "$file_path" ]; then
			echo "${yellow}Backup current $file_name in $DOTFILES_DIR/backup/$file_name.bak${reset}"
			mv "$file_path" "$DOTFILES_DIR/backup/$file_name.bak"
		fi

		# Make symlink
		ln -sv "$dot_file_path" "$file_path"
	else
		echo "${green}$file_name is already install${reset}"
	fi
}

# Bashrc
# --------------------------------------------------------
echo "--- Bash ---"
make_link "bash_aliases" "$DOTFILES_BASH/bash_aliases" "$HOME/.bash_aliases"

# Vim
# --------------------------------------------------------
echo "--- Vim ---"

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
make_link "vimrc_secure" "$DOTFILES_VIM/vimrc_secure" "$HOME/.vim/.vimrc_secure"

# Tmux
# --------------------------------------------------------
echo "--- Tmux ---"

make_link "vimrc" "$DOTFILES_TMUX/tmux.conf" "$HOME/.tmux.conf"

# Lynx
# --------------------------------------------------------
echo "--- Lynx ---"

# Create folders if necessary
if [ ! -d "$HOME/.lynx" ]; then
	mkdir -p "$HOME/.lynx"
fi

make_link "lynxrc" "$DOTFILES_LYNX/lynxrc" "$HOME/.lynx/.lynxrc"

# Newsbeuter
# --------------------------------------------------------
echo "--- Newsbeuter ---"

# Create folders if necessary
if [ ! -d "$HOME/.newsbeuter/" ]; then
	mkdir -p "$HOME/.newsbeuter"
fi

make_link "newsbeuter/config" "$DOTFILES_NEWSBEUTER/config" "$HOME/.newsbeuter/config"

# Sublime Text 3
# --------------------------------------------------------
if [ -d "/opt/sublime_text" ]; then
	echo "--- Sublime Text 3 ---"

	make_link "$SUBLIMETEXT_CONF_KEYMAP" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_KEYMAP" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_KEYMAP"
	make_link "$SUBLIMETEXT_CONF_SETTINGS" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_SETTINGS" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_SETTINGS"
	make_link "$SUBLIMETEXT_CONF_SIDEBAR" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_SIDEBAR" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_SIDEBAR"
	make_link "$SUBLIMETEXT_CONF_MARKDOWN" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_MARKDOWN" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_MARKDOWN"
	make_link "$SUBLIMETEXT_CONF_PHP" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_PHP" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_PHP"
	make_link "$SUBLIMETEXT_CONF_PYTHON" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_PYTHON" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_PYTHON"
fi

# End
# --------------------------------------------------------
echo "--- Done ! ---"
