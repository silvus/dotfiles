" Vim-Plug
" ------------------------------------------------------------------------------------
" curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
call plug#begin('~/.local/share/nvim/plugged')

Plug 'mhinz/vim-startify'
" Plug 'vim-signify'
" Plug 'farmergreg/vim-lastplace'
" Plug 'terryma/vim-multiple-cursors'
" Plug 'w0rp/ale'
Plug 'itchyny/lightline.vim'

" Fzf
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Nerdtree (On demand)
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }"

" Tagbar (On demand)
Plug 'majutsushi/tagbar', { 'on': 'TagbarToggle' }"

" Deoplete
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

" Theme
Plug 'joshdick/onedark.vim'

call plug#end()


" Plugin config
" ------------------------------------------------------------------------------------
" Autocomplete
" pip3 install --user neovim
let g:deoplete#enable_at_startup = 1
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif

" Disable the candidates in Comment/String syntaxes.
" call deoplete#custom#source('_', 'disabled_syntaxes', ['Comment', 'String'])


" Style
" ------------------------------------------------------------------------------------
set number
set relativenumber number
set cursorline
syntax on

" Theme (mute error if absent)
silent! colorscheme onedark

" 24-bit colour enabled
set termguicolors

" Weird symbols ([2 q) when changing modes
" https://github.com/neovim/neovim/wiki/FAQ#nvim-shows-weird-symbols-2-q-when-changing-modes
set guicursor=
" Workaround some broken plugins which set guicursor indiscriminately.
:autocmd OptionSet guicursor noautocmd set guicursor=


" Makes Vim work in a way that Insert mode is the default mode. Useful if you want to use Vim as a modeless editor.
" Use CTRL-O to execute one Normal mode command. When this is a mapping, it is executed as if 'insertmode' was off. Normal mode remains active until the mapping is finished.
" Use CTRL-L to execute a number of Normal mode commands, then use Esc to get back to Insert mode. Note that CTRL-L moves the cursor left, like does when 'insertmode' isn't set.
set insertmode
" set noinsertmode
" Or use tombh/novim-mode to keep vim-startify ?

" Scroll
" ------------------------------------------------------------------------------------
set sidescroll=1          " Scroll one character at a time to reveal more text
set scrolloff=10          " Places a line between the current line and the screen edge
set sidescrolloff=10      " Places a couple columns between the current column and the screen edge


" Keymaps
" ------------------------------------------------------------------------------------
" change the leader key from "\" to ";" ("," is also popular)
let mapleader="!"


" Ctrl + ! to escape insert mode
inoremap <silent> <C-!> <C-l>


" Delete in insert mode
inoremap <silent> <C-d> <C-o>dd
