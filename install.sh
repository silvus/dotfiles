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

# Colors
green=$(tput setaf 2)
yellow=$(tput setaf 3)
blue=$(tput setaf 4)
reset=$(tput sgr0)

# Function for backup a file and make a symlink
# --------------------------------------------------------
make_link() {
	local file_name="$1"
	local dot_file_path="$2"
	local file_path="$3"

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
		echo "${green}$file_name is already installed${reset}"
	fi
}

# Clone if doesn't exist, else update
# --------------------------------------------------------
clone_or_update() {
	local git_url="$1"
	local install_path="$2"
	local depot_name="$3"

	if [ -d "$install_path" ]; then
		# Update
		echo "${green}Update $depot_name${reset}"
		( cd "$install_path" && git pull )
	else
		# Clone
		echo "${yellow}Install $depot_name"
		git clone "$git_url" "$install_path${reset}"
	fi
}

# Create directory if necessary
# --------------------------------------------------------
dir_check() {
	local dir_path="$1"
	if [ ! -d "$dir_path" ]; then
		mkdir -p "$dir_path"
	fi
}

# Bashrc
# --------------------------------------------------------
echo "${blue}--- Bash ---${reset}"
make_link "bash_aliases" "$DOTFILES_BASH/bash_aliases" "$HOME/.bash_aliases"

# Vim
# --------------------------------------------------------
echo "${blue}--- Vim ---${reset}"

dir_check "$HOME/.vim/backup"
dir_check "$HOME/.vim/swap"
dir_check "$HOME/.vim/colors"
dir_check "$HOME/.vim/autoload"
dir_check "$HOME/.vim/bundle"

make_link "molokai.vim" "$DOTFILES_VIM/colors/molokai.vim" "$HOME/.vim/colors/molokai.vim"
make_link "vimrc" "$DOTFILES_VIM/vimrc" "$HOME/.vimrc"
make_link "vimrc_secure" "$DOTFILES_VIM/vimrc_secure" "$HOME/.vim/.vimrc_secure"

# Vim plugins
# --------------------------------------------------------
echo "${blue}--- Vim plugins ---${reset}"

echo "Pathogen"
curl -LSso "$HOME/.vim/autoload/pathogen.vim" "https://raw.github.com/tpope/vim-pathogen/master/autoload/pathogen.vim"

# Nerdtree
clone_or_update "https://github.com/scrooloose/nerdtree.git" "$HOME/.vim/bundle/nerdtree" "Nerdtree"
# Syntastic
clone_or_update "https://github.com/scrooloose/syntastic.git" "$HOME/.vim/bundle/syntastic" "Syntastic"
# Supertab
clone_or_update "https://github.com/ervandew/supertab.git" "$HOME/.vim/bundle/supertab" "Supertab"

# Tmux
# --------------------------------------------------------
echo "${blue}--- Tmux ---${reset}"
make_link "vimrc" "$DOTFILES_TMUX/tmux.conf" "$HOME/.tmux.conf"

# Lynx
# --------------------------------------------------------
echo "${blue}--- Lynx ---${reset}"
dir_check "$HOME/.lynx"
make_link "lynxrc" "$DOTFILES_LYNX/lynxrc" "$HOME/.lynx/.lynxrc"

# Newsbeuter
# --------------------------------------------------------
echo "${blue}--- Newsbeuter ---${reset}"
dir_check "$HOME/.newsbeuter"
make_link "newsbeuter/config" "$DOTFILES_NEWSBEUTER/config" "$HOME/.newsbeuter/config"
make_link "newsbeuter/browse" "$DOTFILES_NEWSBEUTER/browse" "$HOME/.newsbeuter/browse"

# Sublime Text 3
# --------------------------------------------------------
if [ -d "/opt/sublime_text" ]; then
	echo "${blue}--- Sublime Text 3 ---${reset}"
	make_link "$SUBLIMETEXT_CONF_KEYMAP" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_KEYMAP" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_KEYMAP"
	make_link "$SUBLIMETEXT_CONF_SETTINGS" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_SETTINGS" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_SETTINGS"
	make_link "$SUBLIMETEXT_CONF_SIDEBAR" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_SIDEBAR" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_SIDEBAR"
	make_link "$SUBLIMETEXT_CONF_MARKDOWN" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_MARKDOWN" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_MARKDOWN"
	make_link "$SUBLIMETEXT_CONF_PHP" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_PHP" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_PHP"
	make_link "$SUBLIMETEXT_CONF_PYTHON" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_PYTHON" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_PYTHON"
fi

# End
# --------------------------------------------------------
echo "${blue}--- Done ! ---${reset}"
