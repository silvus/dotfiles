#!/usr/bin/env bash

# Variables
# --------------------------------------------------------
DOTFILES_DIR="$(readlink -e $(dirname $0))/.." # dotfiles directory
BACKUP_DIR="$DOTFILES_DIR/backup"
DOTFILES_BASH="$DOTFILES_DIR/bash"
DOTFILES_VIM="$DOTFILES_DIR/vim"
DOTFILES_TMUX="$DOTFILES_DIR/tmux"
DOTFILES_LYNX="$DOTFILES_DIR/lynx"
DOTFILES_NEWSBEUTER="$DOTFILES_DIR/newsbeuter"
DOTFILES_RANGER="$DOTFILES_DIR/ranger"
DOTFILES_SUBLIMETEXT="$DOTFILES_DIR/sublime_text_3"
DOTFILES_VIRTUALENV="$DOTFILES_DIR/virtualenvs"

SUBLIMETEXT_CONF_DIR="$HOME/.config/sublime-text-3/Packages/User"
SUBLIMETEXT_CONF_KEYMAP="Default (Linux).sublime-keymap"
SUBLIMETEXT_CONF_SETTINGS="Preferences.sublime-settings"
SUBLIMETEXT_CONF_SIDEBAR="Side Bar.sublime-settings"
SUBLIMETEXT_CONF_MARKDOWN="Markdown.sublime-settings"
SUBLIMETEXT_CONF_PHP="PHP.sublime-settings"
SUBLIMETEXT_CONF_PYTHON="Python.sublime-settings"
SUBLIMETEXT_CONF_YAML="YAML.sublime-settings"

# Colors
_TXTCOLOR_BLUE=$(tput setaf 4)
_TXTCOLOR_RESET=$(tput sgr0)

# Source functions
source "$DOTFILES_DIR/install/_functions.bash"

# Bashrc
# --------------------------------------------------------
echo "${_TXTCOLOR_BLUE}--- Bash ---${_TXTCOLOR_RESET}"
make_symlink "bash_aliases" "$DOTFILES_BASH/bash_aliases" "$HOME/.bash_aliases"

# Git
# --------------------------------------------------------
make_symlink "gitignore_global" "$DOTFILES_DIR/git/gitignore_global" "$HOME/.gitignore_global"

# Vim
# --------------------------------------------------------
echo "${_TXTCOLOR_BLUE}--- Vim ---${_TXTCOLOR_RESET}"
dir_check "$HOME/.vim/backup"
dir_check "$HOME/.vim/swap"
dir_check "$HOME/.vim/colors"
dir_check "$HOME/.vim/autoload"
dir_check "$HOME/.vim/bundle"
make_symlink "jellybeans.vim" "$DOTFILES_VIM/colors/jellybeans.vim" "$HOME/.vim/colors/jellybeans.vim"
make_symlink "vimrc" "$DOTFILES_VIM/vimrc" "$HOME/.vimrc"
make_symlink "vimrc_secure" "$DOTFILES_VIM/vimrc_secure" "$HOME/.vim/.vimrc_secure"

# Vim plugins
# --------------------------------------------------------
echo "${_TXTCOLOR_BLUE}--- Vim plugins ---${_TXTCOLOR_RESET}"

echo "Pathogen"
curl -LSso "$HOME/.vim/autoload/pathogen.vim" "https://raw.github.com/tpope/vim-pathogen/master/autoload/pathogen.vim"

# Ctrl P
clone_or_update "https://github.com/kien/ctrlp.vim.git" "$HOME/.vim/bundle/ctrlp" "Ctrl P"
# Vim Airline
clone_or_update "https://github.com/bling/vim-airline.git" "$HOME/.vim/bundle/vim-airline" "Vim Airline"
# Syntastic
clone_or_update "https://github.com/scrooloose/syntastic.git" "$HOME/.vim/bundle/syntastic" "Syntastic"
# Vim Commentary
clone_or_update "https://github.com/tpope/vim-commentary.git" "$HOME/.vim/bundle/vim-commentary" "Vim Commentary"
# Supertab
clone_or_update "https://github.com/ervandew/supertab.git" "$HOME/.vim/bundle/supertab" "Supertab"

# Tmux
# --------------------------------------------------------
echo "${_TXTCOLOR_BLUE}--- Tmux ---${_TXTCOLOR_RESET}"
make_symlink "tmux" "$DOTFILES_TMUX/tmux.conf" "$HOME/.tmux.conf"

# Sshrc
# --------------------------------------------------------
echo "${_TXTCOLOR_BLUE}--- SSHRC ---${_TXTCOLOR_RESET}"
curl -sS "https://raw.githubusercontent.com/Russell91/sshrc/master/sshrc" -o "$DOTFILES_DIR/bin/sshrc"
chmod 775 "$DOTFILES_DIR/bin/sshrc"
make_symlink "sshrc" "$DOTFILES_DIR/sshrc/sshrc" "$HOME/.sshrc"

# Lynx
# --------------------------------------------------------
if [[ -x "/usr/bin/lynx" ]]; then
    echo "${_TXTCOLOR_BLUE}--- Lynx ---${_TXTCOLOR_RESET}"
    dir_check "$HOME/.lynx"
    make_symlink "lynxrc" "$DOTFILES_LYNX/lynxrc" "$HOME/.lynx/.lynxrc"
    make_symlink "lynx.lss" "$DOTFILES_LYNX/lynx.lss" "$HOME/.lynx/lynx.lss"
    make_symlink "lynx_bookmarks.html" "$DOTFILES_LYNX/lynx_bookmarks.html" "$HOME/.lynx/lynx_bookmarks.html"
fi

# Ranger
# --------------------------------------------------------
if [[ -x "/usr/bin/ranger" ]]; then
    echo "${_TXTCOLOR_BLUE}--- Ranger ---${_TXTCOLOR_RESET}"
    dir_check "$HOME/.config/ranger"
    make_symlink "rc.conf" "$DOTFILES_RANGER/rc.conf" "$HOME/.config/ranger/rc.conf"
    make_symlink "rifle.conf" "$DOTFILES_RANGER/rifle.conf" "$HOME/.config/ranger/rifle.conf"
fi

# Newsbeuter
# --------------------------------------------------------
if [[ -x "/usr/bin/newsbeuter" ]]; then
	echo "${_TXTCOLOR_BLUE}--- Newsbeuter ---${_TXTCOLOR_RESET}"
	dir_check "$HOME/.newsbeuter"
	make_symlink "newsbeuter_config" "$DOTFILES_NEWSBEUTER/config" "$HOME/.newsbeuter/config"
	make_symlink "newsbeuter_browse" "$DOTFILES_NEWSBEUTER/browse" "$HOME/.newsbeuter/browse"
	make_symlink "newsbeuter_urls" "$DOTFILES_NEWSBEUTER/urls" "$HOME/.newsbeuter/urls"
fi

# Virtualenvwrapper
# --------------------------------------------------------
if [[ -d "/data/dev/.virtualenvs" ]]; then
    echo "${_TXTCOLOR_BLUE}--- Virtualenv Hooks ---${_TXTCOLOR_RESET}"
    make_symlink "postactivate" "$DOTFILES_VIRTUALENV/postactivate" "/data/dev/.virtualenvs/postactivate"
fi

# Sublime Text 3
# --------------------------------------------------------
if [[ -d "/opt/sublime_text" ]]; then
	echo "${_TXTCOLOR_BLUE}--- Sublime Text 3 ---${_TXTCOLOR_RESET}"
	make_symlink "$SUBLIMETEXT_CONF_KEYMAP" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_KEYMAP" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_KEYMAP"
	make_symlink "$SUBLIMETEXT_CONF_SETTINGS" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_SETTINGS" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_SETTINGS"
	make_symlink "$SUBLIMETEXT_CONF_SIDEBAR" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_SIDEBAR" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_SIDEBAR"
	make_symlink "$SUBLIMETEXT_CONF_MARKDOWN" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_MARKDOWN" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_MARKDOWN"
	make_symlink "$SUBLIMETEXT_CONF_PHP" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_PHP" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_PHP"
	make_symlink "$SUBLIMETEXT_CONF_PYTHON" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_PYTHON" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_PYTHON"
	make_symlink "$SUBLIMETEXT_CONF_YAML" "$DOTFILES_SUBLIMETEXT/$SUBLIMETEXT_CONF_YAML" "$SUBLIMETEXT_CONF_DIR/$SUBLIMETEXT_CONF_YAML"
fi

# End
# --------------------------------------------------------
echo "${_TXTCOLOR_BLUE}--- Done ! ---${_TXTCOLOR_RESET}"

