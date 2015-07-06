" Base conf
if filereadable(expand("~/.vim/conf/vimrc_base"))
    source ~/.vim/conf/vimrc_base
endif

" Plugins
if filereadable(expand("~/.vim/conf/vimrc_plugins"))
    source ~/.vim/conf/vimrc_plugins
endif
