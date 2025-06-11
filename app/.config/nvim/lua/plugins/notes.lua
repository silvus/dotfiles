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
					symbols = { ' ', 'x', '-' },
				},
				mappings = {
					MkdnEnter = { { 'i', 'n', 'v' }, '<CR>' } -- This monolithic command has the aforementioned
					-- insert-mode-specific behavior and also will trigger row jumping in tables. Outside
					-- of lists and tables, it behaves as <CR> normally does.
					-- MkdnNewListItem = {'i', '<CR>'} -- Use this command instead if you only want <CR> in
					-- insert mode to add a new list item (and behave as usual outside of lists).
				}
			})
		end,
	},

	-- {
	-- 	"3rd/image.nvim",
	-- 	build = false, -- so that it doesn't build the rock https://github.com/3rd/image.nvim/issues/91#issuecomment-2453430239
	-- 	opts = {
	-- 		processor = "magick_cli",
	-- 	}
	-- },
	-- -- sudo apt install imagemagick libmagickwand-dev
	-- {
	-- 	"3rd/diagram.nvim",
	-- 	dependencies = {
	-- 		"3rd/image.nvim",
	-- 	},
	-- 	opts = {
	-- 		renderer_options = {
	-- 			mermaid = {
	-- 				background = "black", -- nil | "transparent" | "white" | "#hex"
	-- 				theme = "forest", -- nil | "default" | "dark" | "forest" | "neutral"
	-- 				scale = 3, -- nil | 1 (default) | 2  | 3 | ...
	-- 				-- width = nil, -- nil | 800 | 400 | ...
	-- 				-- height = nil, -- nil | 600 | 300 | ...
	-- 			},
	-- 			-- plantuml = {
	-- 			-- 	charset = nil,
	-- 			-- },
	-- 			-- d2 = {
	-- 			-- 	theme_id = nil,
	-- 			-- 	dark_theme_id = nil,
	-- 			-- 	scale = nil,
	-- 			-- 	layout = nil,
	-- 			-- 	sketch = nil,
	-- 			-- },
	-- 			-- gnuplot = {
	-- 			-- 	size = nil, -- nil | "800,600" | ...
	-- 			-- 	font = nil, -- nil | "Arial,12" | ...
	-- 			-- 	theme = nil, -- nil | "light" | "dark" | custom theme string
	-- 			-- },
	-- 		}
	-- 	},
	-- },
}

