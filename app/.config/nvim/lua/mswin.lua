-- MS Win (https://github.com/neovim/neovim/blob/096211a87b1649e9a7408ce159072a6236195eea/runtime/mswin.vim)

vim.opt.selection = 'exclusive'
-- vim.opt.selection = 'inclusive'
vim.opt.selectmode = 'key'    -- when using shifted special keys
-- vim.opt.mousemodel = 'popup' -- Right mouse button pops up a menu
vim.opt.keymodel = 'startsel' -- select using shifted special keys
-- vim.opt.keymodel = 'startsel,stopsel' -- select using shifted special keys and stop on all keys

-- backspace and cursor keys wrap to previous/next line
vim.opt.backspace = { 'indent', 'eol', 'start' }
vim.opt.whichwrap = 'b,s,<,>,[,]'

-- backspace in Visual mode deletes selection
vim.keymap.set('v', '<BS>', 'd')

-- CTRL-X and SHIFT-Del are Cut
vim.keymap.set('v', '<C-X>', 'x')
vim.keymap.set('v', '<S-Del>', 'x')

-- CTRL-C and CTRL-Insert are Copy
vim.keymap.set('v', '<C-c>', '"+y')
vim.keymap.set('v', '<C-C>', '"+y')
vim.keymap.set('v', '<C-Insert>', '"+y')

-- CTRL-V and SHIFT-Insert are Paste
-- vim.keymap.set('n', '<C-v>', 'P<CR>')
-- vim.keymap.set('i', '<C-v>', '<ESC>P<CR>i')
-- vim.keymap.set('v', '<C-v>', '<C-o>P<CR>v')
vim.keymap.set('n', '<C-v>', 'p')
vim.keymap.set({ 'i', 'v' }, '<C-v>', '<C-o>p')

-- CTRL-A is Select all
-- vim.keymap.set('n' , '<C-A>', 'gggH<C-O>G')
-- vim.keymap.set('i', '<C-A>', '<C-O>gg<C-O>gH<C-O>G')
-- vim.keymap.set(cnoremap, '<C-A>', '<C-C>gggH<C-O>G')
-- vim.keymap.set(onoremap, '<C-A>', '<C-C>gggH<C-O>G')
-- vim.keymap.set(snoremap, '<C-A>', '<C-C>gggH<C-O>G')
-- vim.keymap.set(xnoremap, '<C-A>', '<C-C>ggVG')

-- CTRL-Tab is Next window
-- noremap <C-Tab> <C-W>w
-- inoremap <C-Tab> <C-O><C-W>w
-- cnoremap <C-Tab> <C-C><C-W>w
-- onoremap <C-Tab> <C-C><C-W>w

-- CTRL-Z is Undo
vim.keymap.set('n', '<C-Z>', 'u')
vim.keymap.set('i', '<C-Z>', '<C-O>u')
-- CTRL-Y is Redo (although not repeat); not in cmdline though
vim.keymap.set('n', '<C-Y>', '<C-R>')
vim.keymap.set('i', '<C-Y>', '<C-O><C-R>')

