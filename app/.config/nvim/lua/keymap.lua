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
