return {
	'nvim-telescope/telescope.nvim', branch = '0.1.x',
	dependencies = { 'nvim-lua/plenary.nvim' },
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
					-- hidden = true
				},
			},
		}

		local builtin = require('telescope.builtin')
		vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
		vim.keymap.set({'n', 'i', 'v'}, '<C-p>', builtin.git_files, {})
		vim.keymap.set({'n', 'i', 'v'}, '<C-e>', builtin.oldfiles, {})
		vim.keymap.set('n', '<leader>fsp', function()
			builtin.grep_string({ search = vim.fn.input("Grep > ") })
		end)
		vim.keymap.set('n', '<leader><space>', builtin.buffers, {})
		vim.keymap.set('n', '<leader>vh', builtin.help_tags, {})
	end,
}
