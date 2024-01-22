return {
	{
		'jakewvincent/mkdnflow.nvim',
		config = function()
			require('mkdnflow').setup({
				modules = {
					cmp = true,
				},
				create_dirs = false,
				perspective = {
					priority = 'current',
					fallback = 'first',
					root_tell = false,
					nvim_wd_heel = false,
					update = false
				},
				wrap = true,
				to_do = {
					symbols = {' ', 'X', '-'},
				},
			})
		end
	}
}
