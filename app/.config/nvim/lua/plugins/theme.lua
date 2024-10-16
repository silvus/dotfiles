return {
	{
		"navarasu/onedark.nvim",
		lazy = false, -- make sure we load this during startup if it is your main colorscheme
		priority = 1000, -- make sure to load this before all the other start plugins
		config = function()
			require('onedark').setup({
				style = 'darker',
				transparent = true,
				highlights = {
					["Normal"] = {fg = '#cdd0d4'},
					["@markup.list"] = {fg = '#286920'},
					["@markup.list.checked"] = {fg = '#779e4d', fmt = 'italic'},
					["@markup.list.unchecked"] = {fg = '#286920'},
				  },
			})
			require('onedark').load()

			vim.cmd.colorscheme('onedark')

			-- Markdown colors (see :highlight)
			vim.api.nvim_set_hl(0, 'markdownH1', { fg = '#02B33E' })
			vim.api.nvim_set_hl(0, 'markdownH2', { fg = '#1D96D1' })
			vim.api.nvim_set_hl(0, 'markdownH3', { fg = '#203880' })
			vim.api.nvim_set_hl(0, 'markdownH4', { fg = '#6543fa' })
			vim.api.nvim_set_hl(0, 'markdownH5', { fg = '#07420f' })
			vim.api.nvim_set_hl(0, 'markdownH6', { fg = '#707a62' })
			vim.api.nvim_set_hl(0, 'markdownH1Delimiter', { fg = '#02B33E', bold = true })
			vim.api.nvim_set_hl(0, 'markdownH2Delimiter', { fg = '#1D96D1', bold = true })
			vim.api.nvim_set_hl(0, 'markdownH3Delimiter', { fg = '#203880', bold = true })
			vim.api.nvim_set_hl(0, 'markdownH4Delimiter', { fg = '#6543fa', bold = true })
			vim.api.nvim_set_hl(0, 'markdownH5Delimiter', { fg = '#07420f', bold = true })
			vim.api.nvim_set_hl(0, 'markdownH6Delimiter', { fg = '#FFAAAA', bold = true })
			
			vim.api.nvim_set_hl(0, '@markup.heading.1.markdown', { link = 'markdownH1' })
			vim.api.nvim_set_hl(0, '@markup.heading.2.markdown', { link = 'markdownH2' })
			vim.api.nvim_set_hl(0, '@markup.heading.3.markdown', { link = 'markdownH3' })
			vim.api.nvim_set_hl(0, '@markup.heading.4.markdown', { link = 'markdownH4' })
			vim.api.nvim_set_hl(0, '@markup.heading.5.markdown', { link = 'markdownH5' })
			vim.api.nvim_set_hl(0, '@markup.heading.6.markdown', { link = 'markdownH6' })
			vim.api.nvim_set_hl(0, '@markup.heading.1.marker.markdown', { link = 'markdownH1Delimiter' })
			vim.api.nvim_set_hl(0, '@markup.heading.2.marker.markdown', { link = 'markdownH2Delimiter' })
			vim.api.nvim_set_hl(0, '@markup.heading.3.marker.markdown', { link = 'markdownH3Delimiter' })
			vim.api.nvim_set_hl(0, '@markup.heading.4.marker.markdown', { link = 'markdownH4Delimiter' })
			vim.api.nvim_set_hl(0, '@markup.heading.5.marker.markdown', { link = 'markdownH5Delimiter' })
			vim.api.nvim_set_hl(0, '@markup.heading.6.marker.markdown', { link = 'markdownH6Delimiter' })

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
					-- theme = 'wombat',

					-- Disable Powerline separators
					section_separators = '',
					component_separators = '',
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
			require("nvim-tree").setup({
				sync_root_with_cwd = true,
				update_focused_file = {
					enable = true,
					update_root = {
						enable = false,
						ignore_list = {},
					},
					exclude = false,
				},
			})

			vim.keymap.set({'n', 'i', 'v'}, '<C-b>', ":NvimTreeToggle<cr>", {silent = true, noremap = true})
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
