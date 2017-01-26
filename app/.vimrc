" Base conf
set nocompatible          " Get rid of Vi compatibility mode


" Misc
" ------------------------------------------------------------------------------------
set wildmenu              " Show list instead of just completing
set backspace=2           " Backspace in insert mode works like other editors
set encoding=utf-8        " Encoding
set cm=blowfish           " Cryptmethod
set hidden                " Switch between buffers without saving
set splitright            " New vsplit windows to the right of the current
set splitbelow            " New split windows to the bottom of the current
set spelllang=en,fr

" Use whole words when opening URLs. This avoids cutting off parameters (after '?') and anchors (after '#')
let g:netrw_gx="<cWORD>"

" File explorer
let g:netrw_banner=0
let g:netrw_liststyle=3
" let g:netrw_special_syntax=1
" let g:netrw_browse_split=4

" Wrap
set nowrap                " Don't wrap text
set showbreak=↪           " Line wraps character
set display=lastline      " Show as much as possible of a wrapped last line, not just "@"

" Understand *.md and jrnl (temps files) as markdown
autocmd BufNewFile,BufReadPost *.md,jrnl*.txt set filetype=markdown wrap linebreak nolist textwidth=0 wrapmargin=0

" Jump to the last position when reopening a file
autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" Timeout for key sequences
" Fix for vim-airline pause when leaving insert mode but without breaking
" leader key
if ! has('gui_running')
	set ttimeoutlen=10
	augroup FastEscape
		autocmd!
		au InsertEnter * set timeoutlen=50
		au InsertLeave * set timeoutlen=1000
	augroup END
endif


" Spaces - Tabs - Indent - Fold
" ------------------------------------------------------------------------------------
filetype on
filetype plugin on
filetype indent on        " Load filetype-specific indent files

set list                  " Show special characters
set listchars=tab:\ \ ,trail:•,extends:#,nbsp:.

set tabstop=4             " Number of visual spaces per TAB
set softtabstop=4         " Number of spaces in tab when editing
set shiftwidth=4          " When indenting with '>', use x spaces width
" set expandtab           " Tabs are spaces

" Tabs (default)
autocmd FileType * setlocal tabstop=4 shiftwidth=4 softtabstop=4 noexpandtab

" Spaces
autocmd FileType python,xml setlocal tabstop=4 shiftwidth=4 softtabstop=4 expandtab

" Specific
autocmd FileType make setlocal tabstop=8 shiftwidth=8 softtabstop=8 noexpandtab
autocmd FileType yaml setlocal tabstop=2 shiftwidth=2 softtabstop=2 expandtab

" Fold
set foldmethod=indent     " Fold based on indent
set foldlevel=1
set foldlevelstart=1
set foldnestmax=1         " Deepest fold level
" set foldcolumn=1        " See the folds
set nofoldenable          " Don't fold by default


" Scroll
" ------------------------------------------------------------------------------------
set sidescroll=1          " Scroll one character at a time to reveal more text
set scrolloff=10          " Places a line between the current line and the screen edge
set sidescrolloff=10      " Places a couple columns between the current column and the screen edge


" Search
" ------------------------------------------------------------------------------------
set incsearch             " Find as you type search
set hlsearch              " Highlight search terms
set ignorecase            " Make searches case-insensitive.
" set gdefault              " The substitute flag g is on


" Backups and undo
" ------------------------------------------------------------------------------------
" Swap files out of the project root
set backup
set backupdir=~/.vim/backup/
set directory=~/.vim/swap/
" Persistent undo history
set undofile
set undodir=~/.vim/undo/

function! EnsureDirExists (dir)
	if !isdirectory(a:dir)
		call mkdir(a:dir,'p')
	endif
endfunction

let myVimDir = expand("$HOME/.vim")
let myBackupDir = myVimDir . '/backup'
let mySwapDir = myVimDir . '/swap'
call EnsureDirExists(myVimDir)
call EnsureDirExists(myBackupDir)
call EnsureDirExists(mySwapDir)


" Clipboard
" ------------------------------------------------------------------------------------
" In line copy and paste to system clipboard
" vmap <C-c> y:call system("xclip -i -selection clipboard", getreg("\""))<CR>:call system("xclip -i", getreg("\""))<CR>
" nmap <C-v> :call setreg("\"",system("xclip -o -selection clipboard"))<CR>p

" use + buffer (the system clipboard) if +xterm_clipboard
if has('unnamedplus')
	set clipboard=unnamed,unnamedplus
endif


" Tmux Fix : http://sunaku.github.io/vim-256color-bce.html
" ------------------------------------------------------------------------------------
if &term =~ '256color'
	set t_ut=
endif

if &term =~ '^screen'
	" tmux will send xterm-style keys when its xterm-keys option is on
	execute "set <xUp>=\e[1;*A"
	execute "set <xDown>=\e[1;*B"
	execute "set <xRight>=\e[1;*C"
	execute "set <xLeft>=\e[1;*D"
endif


" Keymaps
" ------------------------------------------------------------------------------------
let mapleader="!"

" Edit vimrc
nnoremap <leader>ev :vsplit $MYVIMRC<CR>
" Reload .vimrc
nnoremap <leader>sv :source $MYVIMRC<CR>
nnoremap <leader>r :source $MYVIMRC<CR>

" Navigate in autocomplete (before wrapped line remap)
inoremap <expr> <Down> ((pumvisible())?("\<C-n>"):("\<Down>"))
inoremap <expr> <Up> ((pumvisible())?("\<C-p>"):("\<Up>"))

" Move through wrapped lines
nnoremap <silent> <Up> gk
inoremap <silent> <Up> <C-o>gk
nnoremap <silent> <Down> gj
inoremap <silent> <Down> <C-o>gj
nnoremap <silent> <home> g<home>
inoremap <silent> <home> <C-o>g<home>
nnoremap <silent> <End> g<End>
inoremap <silent> <End> <C-o>g<End>

" Splits
nnoremap <C-w>h :new<CR>
" nnoremap <C-w>v :vnew<CR>

" Ctrl + arrow: move quickly
nnoremap <silent> <C-Up> {
inoremap <silent> <C-Up> <Esc>{i
nnoremap <silent> <C-Down> }
inoremap <silent> <C-Down> <Esc>}i

" Delete in insert mode
inoremap <silent> <C-d> <Esc>ddi

" Paste in insert mode
inoremap <silent> <C-p> <Esc>pi

" Paste from yank register
noremap <leader>p "0p
noremap <leader>P "0P

" Shift + arrow: select text
nmap <S-Up> v<Up>
nmap <S-Down> v<Down>
nmap <S-Left> v<Left>
nmap <S-Right> v<Right>
vmap <S-Up> <Up>
vmap <S-Down> <Down>
vmap <S-Left> <Left>
vmap <S-Right> <Right>
imap <S-Up> <Esc>v<Up>
imap <S-Down> <Esc>v<Down>
imap <S-Left> <Esc>v<Left>
imap <S-Right> <Esc>v<Right>

" Simulate shift-arrows (select block in windows) with control-arrows
inoremap <Esc>[A <C-O>vk
vnoremap <Esc>[A k
inoremap <Esc>[B <C-O>vj
vnoremap <Esc>[B j
inoremap <Esc>[C <C-O>vl
vnoremap <Esc>[C l
inoremap <Esc>[D <C-O>vh
vnoremap <Esc>[D h
noremap <ESC>[1;2D <C-S-Left>
noremap! <ESC>[1;2D <C-S-Left>
noremap <ESC>[1;2C <C-S-Right>
noremap! <ESC>[1;2C <C-S-Right>

" Cycling through buffers
nnoremap <C-n> :bnext<CR>
nnoremap <C-b> :bprevious<CR>
" nnoremap <C-q> :bd<CR>

" Save with Ctrl + s
nmap <C-s> :w<CR>
imap <C-s> <Esc>:w<CR>a
vmap <C-s> <Esc>:w<CR>v

" Toggle folds
nnoremap <space> za

" Toggle line numbers
nmap <silent> <F10> :set invnumber<bar>:set invrelativenumber<CR>

" Redraws the screen and removes any search highlighting.
nnoremap <silent> <C-l> :nohl<CR><C-l>

" Execute current file
nnoremap <F5> :!%:p<CR>

" Insert current date
nnoremap <F6> "=strftime("%Y-%m-%d %T")<CR>P
inoremap <F6> <C-R>=strftime("%Y-%m-%d %T")<CR>
nnoremap <F7> "=strftime("%Y-%m-%d")<CR>P
inoremap <F7> <C-R>=strftime("%Y-%m-%d")<CR>

" Make Markdown header
nnoremap <F8> VypVr=

" Indentation on paste
set pastetoggle=<F9>
" nnoremap p ]p
" nnoremap P p

" Spellchecking
nnoremap <F12> :setlocal spell! spell?<CR>

" Open shell
nnoremap <leader>s :shell<CR>

" Open file explorer
nnoremap <leader>e :Explore<CR>
map <F4> :Explore<CR>

" Change paste motion cp{motion}
nmap <silent> cp :set opfunc=ChangePaste<CR>g@
function! ChangePaste(type, ...)
	silent exe "normal! `[v`]\"_c"
	silent exe "normal! p"
endfunction

" GoTo
nnoremap <leader>g :YcmCompleter GoTo<CR>

" Autocomplete with C-Space
if has("gui_running")
	" C-Space seems to work under gVim on both Linux and win32
	inoremap <C-Space> <C-n>
else " no gui
	if has("unix")
		inoremap <Nul> <C-n>
	endif
endif


" Keymaps - Vim more like Emacs
" ------------------------------------------------------------------------------------
" if X clipboard is available
if has("x11")
	" source $VIMRUNTIME/mswin.vim
	" set 'selection', 'selectmode', 'mousemodel' and 'keymodel' like other editors
	behave mswin

	" CTRL-X and SHIFT-Del are Cut
	vnoremap <C-x> "+x
	vnoremap <S-Del> "+x

	" CTRL-C and CTRL-Insert are Copy
	vnoremap <C-c> "+y
	vnoremap <C-Insert> "+y

	" CTRL-V and SHIFT-Insert are Paste
	map <C-v> "+gP
	map <S-Insert> "+gP

	cmap <C-v> <C-r>+
	cmap <S-Insert> <C-r>+

	" Pasting blockwise and linewise selections is not possible in Insert and
	" Visual mode without the +virtualedit feature.  They are pasted as if they
	" were characterwise instead.
	" Uses the paste.vim autoload script.
	" Use CTRL-G u to have CTRL-Z only undo the paste.
	exe 'inoremap <script> <C-v> <C-g>u' . paste#paste_cmd['i']
	exe 'vnoremap <script> <C-v> ' . paste#paste_cmd['v']

	imap <S-Insert> <C-v>
	vmap <S-Insert> <C-v>

	" Use CTRL-Q to do what CTRL-V used to do
	noremap <C-q> <C-v>

	" CTRL-Y is Redo (although not repeat); not in cmdline though
	noremap <C-y> <C-r>
	inoremap <C-y> <C-o><C-r>

	" Keep V-line behavior
	xnoremap <Up> k
	xnoremap <Down> j
	xnoremap <Left> h
	xnoremap <Right> l
endif

" backspace in Visual mode deletes selection
vnoremap <BS> d

" CTRL-Z is Undo; not in cmdline though
noremap <C-z> u
inoremap <C-z> <C-o>u


" Commands
" ------------------------------------------------------------------------------------
" Trim trailing spaces
function! StripTrailingWhitespaces()
	" Preparation: save last search, and cursor position.
	let _s=@/
	let l = line(".")
	let c = col(".")
	" Do the business:
	%s/\s\+$//e
	" Clean up: restore previous search history, and cursor position
	let @/=_s
	call cursor(l, c)
endfunction
command! StripTrailingWhitespaces call StripTrailingWhitespaces()

" Yapf
command! Yapf :0,$!yapf
autocmd FileType python nnoremap <leader>y :0,$!yapf<Cr>

" :W - To write with root rights
command! W :execute ':silent w !sudo tee % > /dev/null' | :edit!

" Insert lorem ipsum
command! Lorem :read !lorem --words 50 --cols 2000<CR>
nnoremap <leader>lo :read !lorem --words 50 --cols 2000<CR>


" Theme
" ------------------------------------------------------------------------------------
set t_Co=256              " Enable 256-color mode.
syntax enable             " Enable syntax highlighting (previously syntax on).
colorscheme jellybeans    " Set colorscheme
" set background=dark

set ruler                 " Always show info along bottom.
set rulerformat=''

" Vertical separator
set fillchars+=vert:│

set number                " Show line numbers
set relativenumber        " Show relative line numbers
set showmode              " Current mode in status line
set showcmd               " Display the number of (characters|lines) in visual mode, also cur command
set showmatch             " Show matching brackets when text indicator is over them
set cursorline            " Highlights the current line

" Highlights the current line on insert
" autocmd InsertEnter * set cul
" autocmd InsertLeave * set nocul

" Highlights the current line on insert only on current split
augroup CursorLine
	au!
	au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
	au WinLeave * setlocal nocursorline
augroup END


" Status line
" ------------------------------------------------------------------------------------
set laststatus=2          " Last window always has a statusline

set statusline=
set statusline+=\ %<%f\                             " File name
set statusline+=\ %y\                               " File type
set statusline+=\ %h%m%r%w\                         " Modified? Readonly?
set statusline+=%=                                  " Switch to the right side
set statusline+=\ %c\                               " Colnr
set statusline+=\ %l/%L\                            " Current line / Total lines
set statusline+=\ %P\                               " Percent through file


" Plugins
" ------------------------------------------------------------------------------------
if filereadable(expand("~/.vim/vimrc_plugins")) && filereadable(expand("~/.vim/autoload/plug.vim"))
	" g:load_plugins = 0 is use to disable plugins loading
	if !exists("g:load_plugins") || (exists("g:load_plugins") && g:load_plugins)
		source ~/.vim/vimrc_plugins
	endif
endif
