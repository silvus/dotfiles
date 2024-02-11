return {
	{
		'jakewvincent/mkdnflow.nvim',
		config = function()
			require('mkdnflow').setup({
				modules = {
					cmp = true,
				},
				create_dirs = true,
				perspective = {
					priority = 'current',
					fallback = 'first',
					root_tell = false,
					nvim_wd_heel = false,
					update = false
				},
				links = {
					style = 'markdown',
					name_is_source = true,
					conceal = true,
				},
				wrap = true,
				to_do = {
					symbols = {' ', 'X', '-'},
				},
				mappings = {
					MkdnEnter = {{'i', 'n', 'v'}, '<CR>'} -- This monolithic command has the aforementioned
						-- insert-mode-specific behavior and also will trigger row jumping in tables. Outside
						-- of lists and tables, it behaves as <CR> normally does.
					-- MkdnNewListItem = {'i', '<CR>'} -- Use this command instead if you only want <CR> in
						-- insert mode to add a new list item (and behave as usual outside of lists).
				}
			})
		end
	},
	{
		"iamcco/markdown-preview.nvim",
		cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
		ft = { "markdown" },
		build = function()
			vim.fn["mkdp#util#install"]()
		end,
	}
}
