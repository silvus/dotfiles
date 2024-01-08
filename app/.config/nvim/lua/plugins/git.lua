return {
	{
		'lewis6991/gitsigns.nvim',
		event = "VeryLazy",
		config = function()
			require('gitsigns').setup()
		end,
	},
	{
		'tpope/vim-fugitive',
		-- lazy = true,
		event = "VeryLazy",
		config = function()
			vim.keymap.set("n", "<leader>gs", vim.cmd.Git)
		end,
	},
}
