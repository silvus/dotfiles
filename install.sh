#!/bin/bash

# Variables
#----------------------------
DOTFILES_DIR="$HOME/.dotfiles" # dotfiles directory

# Backup and cleanup
#----------------------------
if [ ! -d "$DOTFILES_DIR/backup" ]; then
	mkdir -p "$DOTFILES_DIR/backup"
fi

if [ -f "$HOME/.bash_aliases" ]; then
	echo "backup current bash_aliases in $DOTFILES_DIR/backup/.bash_aliases.bak"
	mv "$HOME/.bash_aliases" "$DOTFILES_DIR/backup/.bash_aliases.bak"
fi
if [ -f "$HOME/.vimrc" ]; then
	echo "backup current vimrc in $DOTFILES_DIR/backup/.vimrc.bak"
	mv "$HOME/.vimrc" "$DOTFILES_DIR/backup/.vimrc.bak"
fi
if [ -f "$HOME/.vim/colors/molokai.vim" ]; then
	echo "backup current molokai.vim in $DOTFILES_DIR/backup/molokai.vim.bak"
	mv "$HOME/.vim/colors/molokai.vim" "$DOTFILES_DIR/backup/molokai.vim.bak"
fi

# Bashrc
#----------------------------
echo "Creating symlink to bashrc"
ln -s "$DOTFILES_DIR/bash/bash_aliases" "$HOME/.bash_aliases"

# Vim
#----------------------------
if [ ! -d "$HOME/.vim/backup" ]; then
	mkdir -p "$HOME/.vim/backup"
fi
if [ ! -d "$HOME/.vim/swap" ]; then
	mkdir -p "$HOME/.vim/swap"
fi

echo "Creating symlink to vim molokai color"
if [ ! -d "$HOME/.vim/colors" ]; then
	mkdir -p "$HOME/.vim/colors"
fi
ln -s "$DOTFILES_DIR/vim/colors/molokai.vim" "$HOME/.vim/colors/molokai.vim"

echo "Creating symlink to vimrc"
ln -s "$DOTFILES_DIR/vim/vimrc" "$HOME/.vimrc"

# Sublime Text 3
#----------------------------
if [ -d "/opt/sublime_text" ]; then
	DOTFILES_SUBLIMETEXT="$DOTFILES_DIR/sublime_text_3"
	echo "Installing config for Sublime Text 3"
	source "$DOTFILES_SUBLIMETEXT/install.sh"
fi

# End
#----------------------------
echo "All done !"
