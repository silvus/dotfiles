#!/bin/bash

# Variables
#----------------------------
DOTFILES_DIR="$HOME/.dotfiles" # dotfiles directory

# Backup and cleanup
#----------------------------
if [ ! -d $DOTFILES_DIR/backup ]; then
	mkdir -p $DOTFILES_DIR/backup
fi

if [ -f ~/.bash_aliases ]; then
	echo "backup current bash_aliases in $DOTFILES_DIR/backup/.bash_aliases.bak"
	mv ~/.bash_aliases $DOTFILES_DIR/backup/.bash_aliases.bak
fi
if [ -f ~/.vimrc ]; then
	echo "backup current vimrc in $DOTFILES_DIR/backup/.vimrc.bak"
	mv ~/.vimrc $DOTFILES_DIR/backup/.vimrc.bak
fi
if [ -f ~/.vim/colors/molokai.vim ]; then
	echo "backup current molokai.vim in $DOTFILES_DIR/backup/molokai.vim.bak"
	mv ~/.vim/colors/molokai.vim $DOTFILES_DIR/backup/molokai.vim.bak
fi

# Bashrc
#----------------------------
echo "Creating symlink to bashrc"
ln -s $DOTFILES_DIR/bash/bash_aliases ~/.bash_aliases

# Vim
#----------------------------
if [ ! -d ~/.vim/backup ]; then
	mkdir -p ~/.vim/backup
fi
if [ ! -d ~/.vim/swap ]; then
	mkdir -p ~/.vim/swap
fi

echo "Creating symlink to vim molokai color"
if [ ! -d ~/.vim/colors ]; then
	mkdir -p ~/.vim/colors
fi
ln -s $DOTFILES_DIR/vim/colors/molokai.vim ~/.vim/colors/molokai.vim

echo "Creating symlink to vimrc"
ln -s $DOTFILES_DIR/vim/vimrc ~/.vimrc

# End
#----------------------------
echo "All done !"
