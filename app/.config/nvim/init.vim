" :source $MYVIMRC

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

" Default to non modal
Plug 'tombh/novim-mode'

" Theme
Plug 'joshdick/onedark.vim'

call plug#end()


" Deoplete
" ------------------------------------------------------------------------------------
" pip3 install --user neovim
let g:deoplete#enable_at_startup = 1
autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif

" Disable the candidates in Comment/String syntaxes.
" call deoplete#custom#source('_', 'disabled_syntaxes', ['Comment', 'String'])

" Fzf
" ------------------------------------------------------------------------------------
" Mapping selecting mappings
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

" Find files
nmap <c-p> :FZF<cr>
imap <c-p> <Esc>:FZF<cr>
" Switch buffers"
nmap <c-e> :Buffers<CR>
imap <c-e> <Esc>:Buffers<CR>
" v:oldfiles and open buffers
nmap <c-h> :History<CR>
imap <c-h> <Esc>:History<CR>
" Executes commands
nmap <a-x> :Commands<CR>
imap <z-x> <Esc>:Commands<CR>
" File types
nmap <a-f> :Filetypes<CR>
imap <a-f> <Esc>:Filetypes<CR>
" Lines in loaded buffers
nmap <a-p> :Lines<CR>
imap <a-p> <Esc>:Lines<CR>

aug fzf_setup
	au!
	au TermOpen term://*FZF tnoremap <silent> <buffer><nowait> <esc> <c-c>
aug END

autocmd! FileType fzf
autocmd  FileType fzf set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler


" NerdTree
" ------------------------------------------------------------------------------------
" Toogle NerdTree
nnoremap <silent> <F2> :NERDTreeToggle<CR>
inoremap <silent> <F2> <C-o>:NERDTreeToggle<CR>

" Show hidden files
let NERDTreeShowHidden=1

" Tagbar
" ------------------------------------------------------------------------------------
nnoremap <silent> <F3> :TagbarToggle<CR>
inoremap <silent> <F3> <C-o>:TagbarToggle<CR>

" Novim
" ------------------------------------------------------------------------------------
let g:novim_mode_use_shortcuts = 0

" Startify
" ------------------------------------------------------------------------------------
let g:startify_custom_header = [
\ '  _______             ____   ____.___          ',
\ '  \      \   ____  ___\   \ /   /|   | _____   ',
\ '  /   |   \_/ __ \/  _ \   Y   / |   |/     \  ',
\ ' /    |    \  ___(  <_> )     /  |   |  Y Y  \ ',
\ ' \____|__  /\___  >____/ \___/   |___|__|_|  / ',
\ '         \/     \/                         \/  ',
\ '',
\ ]


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

set clipboard+=unnamedplus

" if has("gui_running")
	" Guifont DejaVu Sans Mono:h13
" fi

" Weird symbols ([2 q) when changing modes
" https://github.com/neovim/neovim/wiki/FAQ#nvim-shows-weird-symbols-2-q-when-changing-modes
set guicursor=
" Workaround some broken plugins which set guicursor indiscriminately.
:autocmd OptionSet guicursor noautocmd set guicursor=


" Makes Vim work in a way that Insert mode is the default mode. Useful if you want to use Vim as a modeless editor.
" Use CTRL-O to execute one Normal mode command. When this is a mapping, it is executed as if 'insertmode' was off. Normal mode remains active until the mapping is finished.
" Use CTRL-L to execute a number of Normal mode commands, then use Esc to get back to Insert mode. Note that CTRL-L moves the cursor left, like does when 'insertmode' isn't set.
" set insertmode
" set noinsertmode
" Or use tombh/novim-mode to keep vim-startify ?

" Scroll
" ------------------------------------------------------------------------------------
set mouse=a
set sidescroll=1          " Scroll one character at a time to reveal more text
set scrolloff=10          " Places a line between the current line and the screen edge
set sidescrolloff=10      " Places a couple columns between the current column and the screen edge

" Search
" ------------------------------------------------------------------------------------
set ignorecase            " Make searches case-insensitive.


" Keymaps
" ------------------------------------------------------------------------------------
" change the leader key from "\" to "!"
let mapleader="!"

" Escape insert mode
inoremap <C-!> <C-l>
inoremap <C-!> <C-l>
inoremap <C-:> <C-l>:
inoremap <ESC> <C-l>
" inoremap <ESC><ESC> <C-l>
" inoremap <ESC><ESC><ESC> <C-l>
" inoremap <ESC><ESC><ESC><ESC> <C-l>

" Delete line
inoremap <C-d> <C-o>dd
noremap <C-d> dd
vnoremap <C-d> dd

" Reload VIMRC
nnoremap <F4> :source $MYVIMRC<CR>

" Execute current file
nnoremap <F5> :!%:p<CR>

" Insert current date
nnoremap <F6> "=strftime("%Y-%m-%d")<CR>P
inoremap <F6> <C-R>=strftime("%Y-%m-%d")<CR>
nnoremap <F7> "=strftime("%T")<CR>P
inoremap <F7> <C-R>=strftime("%T")<CR>

" Redraws the screen and removes any search highlighting.
nnoremap <F8> :nohl<CR><C-l>

" Indentation on paste
set pastetoggle=<F9>

" Toggle line numbers
nmap <F10> :set invnumber<bar>:set invrelativenumber<CR>

" Spellchecking
nnoremap <F12> :setlocal spell! spell?<CR>

" Buffers navigation
inoremap <C-e> <C-l>:ls<CR>:buffer<Space>
nnoremap <C-e> :ls<CR>:buffer<Space>

" Save with Ctrl + S
nmap <C-s> :w<CR>
imap <C-s> <C-o>:w<CR>
vmap <C-s> <C-o>:w<CR>

" CTRL-X is care Cut
vnoremap <C-x> "+x

" CTRL-C is Copy
vnoremap <C-c> "+y

" CTRL-V is Paste
map <C-v> "+gP
map <S-Insert> "+gP
cmap <C-v> <C-r>+
cmap <S-Insert> <C-r>+

" CTRL-Z is Undo; not in cmdline though
noremap <C-z> u
inoremap <C-z> <C-o>u

" CTRL-Y is Redo (although not repeat); not in cmdline though
noremap <C-y> <C-r>
inoremap <C-y> <C-o><C-r>

" Keep V-line behavior
xnoremap <Up> k
xnoremap <Down> j
xnoremap <Left> h
xnoremap <Right> l

" backspace in Visual mode deletes selection
vnoremap <BS> d

" Break inserts into smaller undo-chunks
inoremap . .<C-g>u
inoremap ? ?<C-g>u
inoremap ! !<C-g>u
inoremap , ,<C-g>u
inoremap <CR> <CR><C-g>u

