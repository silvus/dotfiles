#!/bin/bash

# Variables
#----------------------------
dir=~/.dotfiles # dotfiles directory

# Backup and cleanup
#----------------------------
if [ -f ~/.bash_aliases ]; then
	echo "backup current bash_aliases in $dir/backup/.bash_aliases.bak"
	mv ~/.bash_aliases $dir/backup/.bash_aliases.bak
fi
if [ -f ~/.vimrc ]; then
	echo "backup current vimrc in $dir/backup/.vimrc.bak"
	mv ~/.vimrc $dir/backup/.vimrc.bak
fi
if [ -f ~/.vim/colors/molokai.vim ]; then
	echo "backup current molokai.vim in $dir/backup/molokai.vim.bak"
	mv ~/.vim/colors/molokai.vim $dir/backup/molokai.vim.bak
fi

# Bashrc
#----------------------------
echo "Creating symlink to bashrc"
ln -s $dir/bash/bash_aliases ~/.bash_aliases

# Vim
#----------------------------
echo "Creating symlink to vim molokai color"
ln -s $dir/vim/colors/molokai.vim ~/.vim/colors/molokai.vim

echo "Creating symlink to vimrc"
ln -s $dir/vim/vimrc ~/.vimrc

# End
#----------------------------
echo "All done !"
