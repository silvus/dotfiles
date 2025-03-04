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
				pickers = {
					find_files = {
						hidden = true
					}
				},
				extensions = {
					-- file_browser = {
					-- 	hidden = true,
					-- },
					heading = {
						treesitter = true,
					}
				},
			}

			local builtin = require('telescope.builtin')
			vim.keymap.set('n', '<leader>ff', builtin.find_files, { desc = 'find_files' })
			vim.keymap.set('n', '<leader>fs', builtin.live_grep, { desc = 'live_grep' })
			vim.keymap.set('n', '<leader>fg', builtin.git_files, { desc = 'git_files' })
			vim.keymap.set({'n', 'i', 'v'}, '<C-p>', builtin.find_files, { desc = 'find_files' })
			vim.keymap.set({'n', 'i', 'v'}, '<C-e>', builtin.oldfiles, { desc = 'oldfiles' })
			vim.keymap.set('n', '<leader><space>', builtin.buffers, { desc = 'buffers' })
			vim.keymap.set('n', '<leader>f:', builtin.commands, { desc = 'commands' })
			vim.keymap.set('n', '<leader>fk', builtin.keymaps, { desc = 'keymaps' })
			vim.keymap.set('n', '<leader>vh', builtin.help_tags, { desc = 'help_tags' })
		end,
	},
}
