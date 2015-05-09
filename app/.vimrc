" Base conf
if filereadable(expand("~/.vim/conf/vim_base"))
    source ~/.vim/conf/vim_base
endif

" Plugins
if filereadable(expand("~/.vim/conf/vim_plugins"))
    source ~/.vim/conf/vim_plugins
endif

" Keymaps
if filereadable(expand("~/.vim/conf/vim_keymaps"))
    source ~/.vim/conf/vim_keymaps
endif

" Themes
if filereadable(expand("~/.vim/conf/vim_theme"))
    source ~/.vim/conf/vim_theme
endif
