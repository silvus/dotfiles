" :source $MYVIMRC

" Vim-Plug
" ------------------------------------------------------------------------------------
" curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

let g:with_plugins = 0   " Used track if plugins are load

" Automatic installation of Vim-Plug
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
	silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	autocmd VimEnter * PlugInstall --sync | q | source $MYVIMRC
endif

" if exists("plug#begin") " Doesn't work, plugins are loaded after
if !empty(glob("~/.local/share/nvim/site/autoload/plug.vim"))
	let g:with_plugins = 1 " Keep info that plugins are loaded

	call plug#begin('~/.local/share/nvim/plugged')

	Plug 'mhinz/vim-startify'
	" Plug 'vim-signify'
	" Plug 'terryma/vim-multiple-cursors'
	" Plug 'w0rp/ale'
	Plug 'itchyny/lightline.vim'
	Plug 'farmergreg/vim-lastplace'

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

	" Markdown
	Plug 'godlygeek/tabular' | Plug 'plasticboy/vim-markdown'

	" Language packs
	Plug 'sheerun/vim-polyglot'

	" Theme
	Plug 'joshdick/onedark.vim'

	call plug#end()

	" Automatically install missing plugins on startup
	" ------------------------------------------------------------------------------------
	autocmd VimEnter *
		\  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
			\|   PlugInstall --sync | q
		\| endif

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
	" Allows scrolling through wrapped lines one visual line at a time
	" But set md as txt
	let g:novim_mode_use_better_wrap_navigation = 0

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
endif

" Status line
" ------------------------------------------------------------------------------------
set laststatus=2          " Last window always has a statusline

set statusline+=\ %n\                               " Buffer number
set statusline+=%1*\ %<%f\ %*                       " File name
set statusline+=\ %y\                               " File type
" set statusline+=\ %{&fileencoding?&fileencoding:&encoding} " Encoding -
" TODO: only if not utf-8. Need a an autoload command (https://stackoverflow.com/questions/25178484/how-to-change-color-in-statusline-if-current-file-has-no-utf8-fileencoding) ?
set statusline+=\ %h%m%r%w\                         " Modified? Readonly?
set statusline+=%=                                  " Switch to the right side
set statusline+=\ %c\                               " Col number
" set statusline+=\ %P\                             " Percent through file
set statusline+=%1*
set statusline+=\ %l/%L\ %*
" Current line / Total lines

hi StatusLine ctermbg=DarkBlue guibg=DarkBlue ctermfg=black guifg=black cterm=NONE term=NONE gui=NONE
" hi User1 ctermbg=black ctermfg=white

" Style
" ------------------------------------------------------------------------------------
set number
set relativenumber number
syntax enable             " Enable syntax highlighting
"if (g:with_plugins)
	" Cursor line hightlight if not default theme
	"set cursorline
"endif

" 24-bit colour enabled
"Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
"If you're using tmux version 2.2 or later, you can remove the outermost $TMUX check and use tmux's 24-bit color support
"(see < http://sunaku.github.io/tmux-24bit-color.html#usage > for more information.)
if (empty($TMUX))
	if (has("nvim"))
		"For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
		let $NVIM_TUI_ENABLE_TRUE_COLOR=1
	endif
	"For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
	"Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
	" < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
	if (has("termguicolors"))
		set termguicolors
	endif
endif

" Statubar Theme
" Default lightline theme hightlight all bar on insert mode
" let g:lightline = { 'colorscheme': 'onedark', }
if (g:with_plugins)
	" hide -- INSERT --
	set noshowmode
endif

" Theme
" set background=dark
" let g:onedark_termcolors=16
let g:onedark_terminal_italics = 1
" let g:onedark_termcolors=256

if (has("autocmd"))
	" Doesn't work here https://github.com/equalsraf/neovim-qt/issues/219
	" if (has("gui_running"))
		" Set a different background in gui
	"	augroup colorextend
	"		autocmd!
	"		let s:background = { "gui": "#282C34", "cterm": "232", "cterm16": "0" }
	"		autocmd ColorScheme * call onedark#set_highlight("Normal", { "bg": s:background }) "No `fg` setting
	"	augroup END

	" Don't set a background color when running in a terminal, just use the terminal's background color
	" `gui` is the hex color code used in GUI mode/nvim true-color mode
	" `cterm` is the color code used in 256-color mode
	" `cterm16` is the color code used in 16-color mode
	augroup colorset
		autocmd!
		let s:white = { "gui": "#282C34", "cterm": "145", "cterm16" : "7" }
		autocmd ColorScheme * call onedark#set_highlight("Normal", { "fg": s:white }) " `bg` will not be styled since there is no `bg` setting
	augroup END
endif

silent! colorscheme onedark


" Misc
" ------------------------------------------------------------------------------------
set wildmenu              " Show list instead of just completing
set backspace=2           " Backspace in insert mode works like other editors

" Understand *.md as markdown
autocmd BufNewFile,BufReadPost *.md,*.mdown,*.markdown,*.org,*.mediawiki set filetype=markdown

" Clipboard
set clipboard+=unnamedplus

" Weird symbols ([2 q) when changing modes
" https://github.com/neovim/neovim/wiki/FAQ#nvim-shows-weird-symbols-2-q-when-changing-modes
set guicursor=
" Workaround some broken plugins which set guicursor indiscriminately.
autocmd OptionSet guicursor noautocmd set guicursor=

" Makes Vim work in a way that Insert mode is the default mode. Useful if you want to use Vim as a modeless editor.
" Use CTRL-O to execute one Normal mode command. When this is a mapping, it is executed as if 'insertmode' was off. Normal mode remains active until the mapping is finished.
" Use CTRL-L to execute a number of Normal mode commands, then use Esc to get back to Insert mode. Note that CTRL-L moves the cursor left, like does when 'insertmode' isn't set.
" set insertmode
" set noinsertmode
" Or use tombh/novim-mode to keep vim-startify ?
" Set normal mode at start (Doesn't work with tombh/novim-mode
" autocmd BufRead,BufNewFile * execute "normal \<ESC>"


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

" Ctrl + arrow: move quickly
nnoremap <silent> <C-Up> {
inoremap <silent> <C-Up> <Esc>{i
nnoremap <silent> <C-Down> }
inoremap <silent> <C-Down> <Esc>}i

" backspace in Visual mode deletes selection
vnoremap <BS> d

" Break inserts into smaller undo-chunks
inoremap . .<C-g>u
inoremap ? ?<C-g>u
inoremap ! !<C-g>u
inoremap , ,<C-g>u
inoremap <CR> <CR><C-g>u

" Toggle folds
nnoremap <space> za

" Better window navigation
nnoremap <leader><Up> <C-w>k
nnoremap <leader><Down> <C-w>j
nnoremap <leader><Left> <C-w>h
nnoremap <leader><Right> <C-w>l

