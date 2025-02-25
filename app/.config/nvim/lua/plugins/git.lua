return {
	{
		'lewis6991/gitsigns.nvim',
		event = "VeryLazy",
		config = function()
			require('gitsigns').setup()
		end,
	},
	{
		'mbbill/undotree',
		config = function()
			vim.keymap.set("n", "<leader>u", vim.cmd.UndotreeToggle)
		end,
	},
	-- {
	-- 	'tpope/vim-fugitive',
	-- 	-- lazy = true,
	-- 	event = "VeryLazy",
	-- 	config = function()
	-- 		vim.keymap.set("n", "<leader>gs", vim.cmd.Git)
	-- 	end,
	-- },
	{
		"kdheepak/lazygit.nvim",
		lazy = true,
		cmd = {
			"LazyGit",
			"LazyGitConfig",
			"LazyGitCurrentFile",
			"LazyGitFilter",
			"LazyGitFilterCurrentFile",
		},
		-- optional for floating window border decoration
		dependencies = {
			"nvim-lua/plenary.nvim",
		},
		-- setting the keybinding for LazyGit with 'keys' is recommended in
		-- order to load the plugin when the command is run for the first time
		keys = {
			{ "<leader>lz", "<cmd>LazyGit<cr>", desc = "LazyGit" }
		},
		config = function()
			-- scaling factor for floating window
			vim.g.lazygit_floating_window_scaling_factor = 1
		end,
	},
}

