-- https://neovim.io/doc/user/options.html

-- Fat cursor
-- vim.opt.guicursor = ""
vim.opt.guicursor = {
	'n-v-c:block-Cursor/lCursor-blinkwait100-blinkon500-blinkoff500',
	'i-ci:ver25-Cursor/lCursor-blinkwait100-blinkon500-blinkoff500',
	"r-cr-o:hor20-Cursor/lCursor",
}

-- Line numbers
vim.opt.nu = true
vim.opt.relativenumber = true

-- Indent
vim.opt.tabstop = 4     -- width used to display an actual tabulation character
vim.opt.softtabstop = 4 -- how wide an indentation is supposed to span
vim.opt.shiftwidth = 4  -- width used for shifting commands
vim.opt.expandtab = false -- Tabs are spaces?
vim.opt.smartindent = false

-- Wrap
vim.opt.wrap = false

-- Backup
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
vim.opt.undofile = true

-- Search
vim.opt.hlsearch = false  -- Highlight search terms
vim.opt.incsearch = true  -- Find as you type search
vim.opt.ignorecase = true -- Case-insensitive searching UNLESS \C or capital in search
vim.opt.smartcase = true

-- Sync clipboard between OS and Neovim.
-- Remove this option if you want your OS clipboard to remain independent.
-- vim.opt.clipboard = 'unnamed' -- PRIMARY selections (copy-on-select, pasted with the middle mouse)
vim.opt.clipboard = 'unnamedplus' -- CLIPBOARD selections (^C)

-- Style
vim.opt.termguicolors = true
-- vim.opt.colorcolumn = "80"
-- Conceal is not great on markdown checkbox level 2 and more (Because of Treesitter)
vim.wo.conceallevel = 2
-- Show whitespaces and trailing 
vim.opt.list = true
vim.opt.listchars = {
	tab = '→ ',
	trail = "·",
	lead = "·",
	nbsp = '␣',
	-- space = "·",
	--eol = '↲',
	-- precedes = "<",
	-- extends = ">",
	-- leadmultispace = "┊ ",
}

-- Set showbreak
vim.opt.showbreak = '↳ '

-- Scroll
vim.opt.scrolloff = 10     -- Places a line between the current line and the screen edge
vim.opt.sidescrolloff = 10 -- Places a couple columns between the current column and the screen edge
vim.opt.signcolumn = "yes"
-- vim.opt.isfname:append("@-@")

-- Update
vim.opt.updatetime = 50

-- Netrw
vim.g.netrw_banner = 0    -- Disables the Netrw banner. Press 'I' to toggle.
vim.g.netrw_liststyle = 3 -- tree style listing

-- Open Help in a vertical split
vim.api.nvim_create_autocmd('BufWinEnter', {
	-- pattern = '*',
	group = vim.api.nvim_create_augroup("HelpVerticalSplit", { clear = true }),
	callback = function(event)
		if vim.bo[event.buf].filetype == 'help' then
			vim.cmd("wincmd L")
		end
	end,
})
