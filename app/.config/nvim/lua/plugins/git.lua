return {
	{
		'lewis6991/gitsigns.nvim',
		lazy = true,
	},
	{
		'tpope/vim-fugitive',
		lazy = true,
		config = function()
			vim.keymap.set("n", "<leader>gs", vim.cmd.Git)
		end,
	},
}
