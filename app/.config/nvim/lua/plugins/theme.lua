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

			vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
			vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })

			vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
			vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
		end,
	},
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
		dependencies = 'nvim-tree/nvim-web-devicons',
		event = "VeryLazy",
		config = function()
			require('nvim-web-devicons').setup()

			-- table.insert(opts.icons_enabled, false)
			-- table.insert(opts.theme, 'codedark')

			require('lualine').setup({
				options = {
					theme = 'codedark',
					-- icons_enabled = false,
				},
				sections = {
					lualine_c = {
						{
							'filename',
							newfile_status = true,  -- Display new file status (new file means no write after created)
							path = 1,               -- 0: Just the filename
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
		"lukas-reineke/headlines.nvim",
		dependencies = "nvim-treesitter/nvim-treesitter",
		event = "VeryLazy",
		config = function() -- or `opts = {}`
			require('headlines').setup({
				-- markdown = {
				-- 	fat_headline_lower_string = "_",
				-- },
				-- org = {
				-- 	fat_headline_lower_string = "_",
				-- },
			})
		end,
	},
}
