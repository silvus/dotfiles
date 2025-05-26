-- Leader
vim.keymap.set("n", "<Space>", "<Nop>", { silent = true, remap = false })
vim.g.mapleader = ' '
-- vim.g.maplocalleader = ' '

-- Netw
vim.keymap.set('n', "<leader>pv", vim.cmd.Ex, { desc = 'Netw' })

-- Delete line
vim.keymap.set('n', '<C-d>', 'dd', { silent = true, desc = 'Delete line' })
vim.keymap.set({'i', 'v'}, '<C-d>', '<C-o>dd', { silent = true, desc = 'Delete line' })

-- Save
vim.keymap.set('n', '<C-s>', ':update<cr>', { silent = true, desc = 'Save' })
vim.keymap.set({'i', 'v'}, '<C-s>', '<C-o>:update<cr>', { silent = true, desc = 'Save' })

-- Close
vim.keymap.set('n', '<C-q>', ':q<cr>', { silent = true, desc = 'Close' })

-- Indent
vim.keymap.set({'i', 'v'}, '<S-Tab>', '<C-o><<', { silent = true, desc = 'Unindent' })

-- Open a vertical split to the right with a new buffer
-- vim.keymap.set("n", "<leader>s", ":vsplit<cr>", { noremap = true, silent = true }) -- vertical split

-- Window navigation
vim.keymap.set("n", "<C-Right>", "<C-w>l", { noremap = true, silent = true }) -- move to right window
vim.keymap.set("n", "<C-Left>", "<C-w>h", { noremap = true, silent = true }) -- move to left window
vim.keymap.set("n", "<C-Down>", "<C-w>j", { noremap = true, silent = true }) -- move to lower window
vim.keymap.set("n", "<C-Up>", "<C-w>k", { noremap = true, silent = true }) -- move to upper window
vim.keymap.set("i", "<C-Right>", "<C-o><C-w>l", { noremap = true, silent = true }) -- move to right window and stay in insert mode
vim.keymap.set("i", "<C-Left>", "<C-o><C-w>h", { noremap = true, silent = true }) -- move to left window and stay in insert mode
vim.keymap.set("i", "<C-Down>", "<C-o><C-w>j", { noremap = true, silent = true }) -- move to right window and stay in insert mode
vim.keymap.set("i", "<C-Up>", "<C-o><C-w>k", { noremap = true, silent = true }) -- move to left window and stay in insert mode

-- Window resizing
vim.keymap.set("n", "<C-S-Right>", "<C-w>>", { noremap = true, silent = true }) -- increase window width
vim.keymap.set("n", "<C-S-Left>", "<C-w><", { noremap = true, silent = true }) -- decrease window width
vim.keymap.set("n", "<C-S-Down>", "<C-w>+", { noremap = true, silent = true }) -- increase window height
vim.keymap.set("n", "<C-S-Up>", "<C-w>-", { noremap = true, silent = true }) -- decrease window height
vim.keymap.set("i", "<C-S-Right>", "<C-o><C-w>>", { noremap = true, silent = true }) -- increase window width
vim.keymap.set("i", "<C-S-Left>", "<C-o><C-w><", { noremap = true, silent = true }) -- decrease window width
