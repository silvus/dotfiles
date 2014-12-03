#!/usr/bin/env bash

# Variables
# --------------------------------------------------------
DOTFILES_DIR="$(readlink -e $(dirname $0))/.." # dotfiles directory
BASH_COMPLETION_DIR="/etc/bash_completion.d"
BACKUP_DIR="$DOTFILES_DIR/backup"
SUBLIMETEXT_CONF_DIR="$HOME/.config/sublime-text-3/Packages/User"
I3_CONFIG_FILE="$HOME/.i3/config"

# Colors
_TXTCOLOR_BLUE=$(tput setaf 4)
_TXTCOLOR_RESET=$(tput sgr0)

# Source functions
# --------------------------------------------------------
source "$DOTFILES_DIR/install/_functions.bash"

# Bash
# --------------------------------------------------------
echo "${_TXTCOLOR_BLUE}--- Bash ---${_TXTCOLOR_RESET}"
make_symlink "bash_aliases" "$DOTFILES_DIR/bash/bash_aliases" "$HOME/.bash_aliases"
make_symlink "completion_sshrc" "$DOTFILES_DIR/bash/completion/sshrc" "$BASH_COMPLETION_DIR/sshrc" "sudo"
make_symlink "completion_dev" "$DOTFILES_DIR/bash/completion/dev" "$BASH_COMPLETION_DIR/dev" "sudo"

# Git
# --------------------------------------------------------
if [[ -x "$(which git 2>/dev/null)" ]]; then
	echo "${_TXTCOLOR_BLUE}--- Git ---${_TXTCOLOR_RESET}"
	make_symlink "gitignore_global" "$DOTFILES_DIR/git/gitignore_global" "$HOME/.gitignore_global"
fi

# Vim
# --------------------------------------------------------
if [[ -x "$(which vim 2>/dev/null)" ]]; then
	echo "${_TXTCOLOR_BLUE}--- Vim ---${_TXTCOLOR_RESET}"
	dir_check "$HOME/.vim/backup"
	dir_check "$HOME/.vim/swap"
	dir_check "$HOME/.vim/colors"
	dir_check "$HOME/.vim/autoload"
	dir_check "$HOME/.vim/bundle"
	make_symlink "jellybeans.vim" "$DOTFILES_DIR/vim/colors/jellybeans.vim" "$HOME/.vim/colors/jellybeans.vim"
	make_symlink "vimrc" "$DOTFILES_DIR/vim/vimrc" "$HOME/.vimrc"
	make_symlink "vimrc_secure" "$DOTFILES_DIR/vim/vimrc_secure" "$HOME/.vim/.vimrc_secure"

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
fi

# Tmux
# --------------------------------------------------------
if [[ -x "$(which tmux 2>/dev/null)" ]]; then
	echo "${_TXTCOLOR_BLUE}--- Tmux ---${_TXTCOLOR_RESET}"
	make_symlink "tmux" "$DOTFILES_DIR/tmux/tmux.conf" "$HOME/.tmux.conf"
fi

# PHP
# --------------------------------------------------------
if [[ -x "$(which php 2>/dev/null)" ]]; then
	# Composer
	echo "${_TXTCOLOR_BLUE}--- Composer ---${_TXTCOLOR_RESET}"
	curl -sS "https://getcomposer.org/composer.phar" -o "$DOTFILES_DIR/bin/composer"
	chmod 775 "$DOTFILES_DIR/bin/composer"
	make_symlink "completion_composer" "$DOTFILES_DIR/bash/completion/composer" "$BASH_COMPLETION_DIR/composer" "sudo"
fi

# Go
# --------------------------------------------------------
if [[ -x "$(which go 2>/dev/null)" ]]; then
	if [[ -f "/usr/local/go/misc/bash/go" ]]; then
		# Completion
		echo "${_TXTCOLOR_BLUE}--- Go ---${_TXTCOLOR_RESET}"
		make_symlink "completion_go" "/usr/local/go/misc/bash/go" "$BASH_COMPLETION_DIR/go" "sudo"
	fi
fi

# SSHRC
# --------------------------------------------------------
echo "${_TXTCOLOR_BLUE}--- SSHRC ---${_TXTCOLOR_RESET}"
curl -sS "https://raw.githubusercontent.com/Russell91/sshrc/master/sshrc" -o "$DOTFILES_DIR/bin/sshrc"
chmod 775 "$DOTFILES_DIR/bin/sshrc"
make_symlink "sshrc" "$DOTFILES_DIR/sshrc/sshrc" "$HOME/.sshrc"

# Lynx
# --------------------------------------------------------
if [[ -x "$(which lynx 2>/dev/null)" ]]; then
    echo "${_TXTCOLOR_BLUE}--- Lynx ---${_TXTCOLOR_RESET}"
    dir_check "$HOME/.lynx"
    make_symlink "lynxrc" "$DOTFILES_DIR/lynx/lynxrc" "$HOME/.lynx/.lynxrc"
    make_symlink "lynx.lss" "$DOTFILES_DIR/lynx/lynx.lss" "$HOME/.lynx/lynx.lss"
    make_symlink "lynx_bookmarks.html" "$DOTFILES_DIR/lynx/lynx_bookmarks.html" "$HOME/.lynx/lynx_bookmarks.html"
fi

# Ranger
# --------------------------------------------------------
if [[ -x "$(which ranger 2>/dev/null)" ]]; then
    echo "${_TXTCOLOR_BLUE}--- Ranger ---${_TXTCOLOR_RESET}"
    dir_check "$HOME/.config/ranger"
    make_symlink "rc.conf" "$DOTFILES_DIR/ranger/rc.conf" "$HOME/.config/ranger/rc.conf"
    make_symlink "rifle.conf" "$DOTFILES_DIR/ranger/rifle.conf" "$HOME/.config/ranger/rifle.conf"
fi

# MOC
# --------------------------------------------------------
if [[ -x "$(which mocp 2>/dev/null)" ]]; then
    echo "${_TXTCOLOR_BLUE}--- MOC ---${_TXTCOLOR_RESET}"
    dir_check "$HOME/.moc/themes"
    make_symlink "moc_config" "$DOTFILES_DIR/moc/config" "$HOME/.moc/config"
    chmod 644 "$HOME/.moc/config" # Need to be not writable by other
    make_symlink "moc_theme" "$DOTFILES_DIR/moc/dot_theme" "$HOME/.moc/themes/dot_theme"
fi

# i3
# --------------------------------------------------------
if [[ -x "$(which i3 2>/dev/null)" ]]; then
    echo "${_TXTCOLOR_BLUE}--- i3 ---${_TXTCOLOR_RESET}"
    dir_check "$HOME/.i3"

    make_symlink "conky_launcher" "$DOTFILES_DIR/i3/conky_launcher" "$HOME/.i3/conky_launcher"
    chmod 775 "$HOME/.i3/conky_launcher"
    make_symlink "conky_statusbar" "$DOTFILES_DIR/i3/conky_statusbar" "$HOME/.i3/conky_statusbar"

	SCREEN_COUNT=$(xrandr -q | grep ' connected' | wc -l)
	echo "Number of screens connected : $SCREEN_COUNT"

	# Generated i3 config
	echo "${_TXTCOLOR_GREEN}Generated i3 config${_TXTCOLOR_RESET}"

	echo -e "# DO NOT EDIT THIS FILE BY HAND.\n# YOUR CHANGES WILL BE OVERWRITTEN !\n" > "$I3_CONFIG_FILE"
	cat "$DOTFILES_DIR/i3/config_main" >> "$I3_CONFIG_FILE"

	if [[ "$SCREEN_COUNT" -gt 1 ]]; then
		# Dual screen
    	make_symlink "i3_status" "$DOTFILES_DIR/i3/i3status_dual.conf" "$HOME/.i3/i3status.conf"
    	cat "$DOTFILES_DIR/i3/config_bar_dual" >> "$I3_CONFIG_FILE"
	else
		# Simple screen
    	make_symlink "i3_status" "$DOTFILES_DIR/i3/i3status.conf" "$HOME/.i3/i3status.conf"
    	cat "$DOTFILES_DIR/i3/config_bar" >> "$I3_CONFIG_FILE"
	fi

	cat "$DOTFILES_DIR/i3/config_apps" >> "$I3_CONFIG_FILE"
fi

# Conky
# --------------------------------------------------------
if [[ -x "$(which conky 2>/dev/null)" ]]; then
	echo "${_TXTCOLOR_BLUE}--- Conky ---${_TXTCOLOR_RESET}"
	make_symlink "conkyrc" "$DOTFILES_DIR/conky/conkyrc" "$HOME/.conkyrc"
fi

# Newsbeuter
# --------------------------------------------------------
if [[ -x "$(which newsbeuter 2>/dev/null)" ]]; then
	echo "${_TXTCOLOR_BLUE}--- Newsbeuter ---${_TXTCOLOR_RESET}"
	dir_check "$HOME/.newsbeuter"
	make_symlink "newsbeuter_config" "$DOTFILES_DIR/newsbeuter/config" "$HOME/.newsbeuter/config"
	make_symlink "newsbeuter_browse" "$DOTFILES_DIR/newsbeuter/browse" "$HOME/.newsbeuter/browse"
	make_symlink "newsbeuter_urls" "$DOTFILES_DIR/newsbeuter/urls" "$HOME/.newsbeuter/urls"
fi

# Virtualenvwrapper
# --------------------------------------------------------
if [[ -d "/data/dev/.virtualenvs" ]]; then
    echo "${_TXTCOLOR_BLUE}--- Virtualenv Hooks ---${_TXTCOLOR_RESET}"
    make_symlink "postactivate" "$DOTFILES_DIR/virtualenvs/postactivate" "/data/dev/.virtualenvs/postactivate"
fi

# Sublime Text 3
# --------------------------------------------------------
if [[ -d "/opt/sublime_text" ]]; then
	echo "${_TXTCOLOR_BLUE}--- Sublime Text 3 ---${_TXTCOLOR_RESET}"
	make_symlink "Sublime-keymap" "$DOTFILES_DIR/sublime_text_3/Default (Linux).sublime-keymap" "$SUBLIMETEXT_CONF_DIR/Default (Linux).sublime-keymap"
	make_symlink "Preferences" "$DOTFILES_DIR/sublime_text_3/Preferences.sublime-settings" "$SUBLIMETEXT_CONF_DIR/Preferences.sublime-settings"
	make_symlink "Side Bar" "$DOTFILES_DIR/sublime_text_3/Side Bar.sublime-settings" "$SUBLIMETEXT_CONF_DIR/Side Bar.sublime-settings"
	make_symlink "Markdown" "$DOTFILES_DIR/sublime_text_3/Markdown.sublime-settings" "$SUBLIMETEXT_CONF_DIR/Markdown.sublime-settings"
	make_symlink "PHP" "$DOTFILES_DIR/sublime_text_3/PHP.sublime-settings" "$SUBLIMETEXT_CONF_DIR/PHP.sublime-settings"
	make_symlink "Python" "$DOTFILES_DIR/sublime_text_3/Python.sublime-settings" "$SUBLIMETEXT_CONF_DIR/Python.sublime-settings"
	make_symlink "YAML" "$DOTFILES_DIR/sublime_text_3/YAML.sublime-settings" "$SUBLIMETEXT_CONF_DIR/YAML.sublime-settings"
	make_symlink "LaTeXTools" "$DOTFILES_DIR/sublime_text_3/LaTeXTools.sublime-settings" "$SUBLIMETEXT_CONF_DIR/LaTeXTools.sublime-settings"
fi

# End
# --------------------------------------------------------
echo "${_TXTCOLOR_BLUE}--- End ---${_TXTCOLOR_RESET}"
