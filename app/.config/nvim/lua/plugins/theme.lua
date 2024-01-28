return {
	{
		"navarasu/onedark.nvim",
		lazy = false, -- make sure we load this during startup if it is your main colorscheme
		priority = 1000, -- make sure to load this before all the other start plugins
		config = function()
			require('onedark').setup({
				style = 'darker',
				transparent = true,
			})
			require('onedark').load()

			vim.cmd.colorscheme('onedark')
		end,
	},
	-- {
	-- 	'EdenEast/nightfox.nvim',
	-- 	lazy = false,
	-- 	priority = 1000,
	-- 	opts = {},
	-- 	config = function()
	-- 		vim.cmd.colorscheme('carbonfox')
	-- 	end,
	-- },
	-- {
	-- 	"folke/tokyonight.nvim",
	-- 	lazy = false,
	-- 	priority = 1000,
	-- 	config = function()
	-- 		vim.cmd.colorscheme('tokyonight-night')
	-- 	end,
	-- },
	{
		'nvim-tree/nvim-web-devicons',
		lazy = true,
		config = function()
			require('nvim-web-devicons').setup()
		end,
	},
	{
		'nvim-lualine/lualine.nvim',
		-- and https://github.com/ryanoasis/nerd-fonts
		dependencies = {
			'nvim-tree/nvim-web-devicons',
		},
		event = "VeryLazy",
		config = function()
			require('lualine').setup({
				options = {
					theme = 'codedark',
				},
				sections = {
					lualine_c = {
						{
							'filename',
							newfile_status = true,	-- Display new file status (new file means no write after created)
							path = 1,				-- 0: Just the filename
													-- 1: Relative path
													-- 2: Absolute path
													-- 3: Absolute path, with tilde as the home directory
													-- 4: Filename and parent dir, with tilde as the home directory
						}
					}
				},
			})
		end
	},
	{
		"nvim-tree/nvim-tree.lua",
		version = "*",
		lazy = false,
		dependencies = {
			"nvim-tree/nvim-web-devicons",
		},
		config = function()
			require("nvim-tree").setup({})
		end,
	},
	{
		'rcarriga/nvim-notify',
		config = function()
			require("notify").setup({
				background_colour = "#000000",
			})
			vim.notify = require("notify")
		end,
	},
	{
		"folke/which-key.nvim",
		event = "VeryLazy",
		init = function()
			vim.o.timeout = true
			vim.o.timeoutlen = 300
		end,
		opts = {},
	},
}
