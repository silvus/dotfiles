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
