return {
	{
		'nvim-telescope/telescope.nvim', branch = '0.1.x',
		dependencies = {
			'nvim-lua/plenary.nvim',
		},
		config = function()
			require("telescope").setup {
				defaults = {
					mappings = {
						i = {
							["<Esc>"] = require('telescope.actions').close
						}
					},
					-- The below pattern is lua regex and not wildcard
					file_ignore_patterns = {"node_modules","%.out"},
				},
				extensions = {
					file_browser = {
						-- hidden = true,
					},
					heading = {
						treesitter = true,
					}
				},
			}

			local builtin = require('telescope.builtin')
			vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
			vim.keymap.set('n', '<leader>fi', builtin.live_grep, {})
			vim.keymap.set('n', '<leader>fg', builtin.git_files, {})
			vim.keymap.set({'n', 'i', 'v'}, '<C-p>', builtin.find_files, {})
			vim.keymap.set({'n', 'i', 'v'}, '<C-e>', builtin.oldfiles, {})
			vim.keymap.set('n', '<leader><space>', builtin.buffers, {})
			vim.keymap.set('n', '<leader>:', builtin.commands, {})
			vim.keymap.set('n', '<leader>vk', builtin.keymaps, {})
			vim.keymap.set('n', '<leader>vh', builtin.help_tags, {})
		end,
	},
}
