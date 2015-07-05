" vimrc.ide conf
if filereadable(expand("~/.vim/vimrc_ide"))
    source ~/.vim/vimrc_ide
endif

" Toggle menu/tool bar
" -----------------------------
function! ToggleGUICruft()
	if &guioptions=='i'
		exec('set guioptions=imTr')
	else
		exec('set guioptions=i')
	endif
endfunction

map <F11> <Esc>:call ToggleGUICruft()<cr>

" by default, hide gui menus
set guioptions=i
