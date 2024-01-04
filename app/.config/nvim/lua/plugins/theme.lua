return {
	{
		"navarasu/onedark.nvim",
		lazy = false, -- make sure we load this during startup if it is your main colorscheme
		priority = 1000, -- make sure to load this before all the other start plugins
		config = function()
		  require('onedark').setup {
			style = 'darker',
			transparent = true,
		}
		require('onedark').load()

		vim.cmd.colorscheme('onedark')

		vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
		vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })

		  -- 		vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
	-- 		vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
		end,
	  },
	-- {
	-- 	"navarasu/onedark.nvim",
	-- 	lazy = false, -- make sure we load this during startup if it is your main colorscheme
	-- 	priority = 1000, -- make sure to load this before all the other start plugins
	-- 	config = function()
	-- 		-- 	-- load the colorscheme here
	-- 		-- 	vim.cmd([[colorscheme onedark]])
	-- 		-- require('onedark').setup {
	-- 		-- 	style = 'darker', -- Default theme style. Choose between 'dark', 'darker', 'cool', 'deep', 'warm', 'warmer' and 'light'
	-- 		-- 	transparent = true,  -- Show/hide background
	-- 		-- }
	-- 		-- require('onedark').load()

	-- 		-- vim.cmd.colorscheme('onedark')
	-- 		vim.cmd([[colorscheme onedark]])

	-- 		vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
	-- 		vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })

	-- 	end,
	-- },
	{
		'nvim-lualine/lualine.nvim',
		dependencies = 'nvim-tree/nvim-web-devicons',
		event = "VeryLazy",
		config = function(_, opts)
			-- table.insert(opts.icons_enabled, false)
			-- table.insert(opts.theme, 'codedark')

			require('lualine').setup {
				options = {
					theme = 'codedark',
					-- icons_enabled = false,
				},
			  }

		end
	},
	{
		"lukas-reineke/headlines.nvim",
		dependencies = "nvim-treesitter/nvim-treesitter",
		config = function() -- or `opts = {}`
			require('headlines').setup {
				-- markdown = {
				-- 	fat_headline_lower_string = "_",
				-- },
				-- org = {
				-- 	fat_headline_lower_string = "_",
				-- },
			}
		end,
		
	},
}
