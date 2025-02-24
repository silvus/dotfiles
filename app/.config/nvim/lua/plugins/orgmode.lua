return {
	'nvim-orgmode/orgmode',
	dependencies = {
		{ 'nvim-treesitter/nvim-treesitter', lazy = true },
		{ 'hrsh7th/nvim-cmp' },
	},
	-- VeryLazy event for things that can load later and are not important for the initial UI
	-- event = 'VeryLazy',
	-- lazy-load on filetype
	ft = "org",
	config = function()
		local orgmode = require('orgmode')

		local org = orgmode.setup({
			org_agenda_files = {
				'/data/doc/**/*',
				'/data/work/**/*',
			},
			org_default_notes_file = '/data/doc/orgmode/todo.org',
			org_startup_indented = true, -- Use virtual indent
			org_todo_keywords = {
				'TODO(t)',
				'NEXT(n)',
				'WAIT(w@/!)',
				'|',
				'DONE(d!)',
				'CANCELED(c@)',
				'DELEGATED(g@)',
				'INACTIVE(i@)',
			},
			-- org_todo_keyword_faces = {
			-- 	WAIT = ':foreground blue :weight bold',
			-- 	DELEGATED = ':background #FFFFFF :slant italic :underline on',
			-- 	INACTIVE = ':background #000000 :foreground red', -- overrides builtin color for `TODO` keyword
			-- },
			win_split_mode = '80vsplit', -- auto, horizontal, vertical, { 'float', 0.3 }, tabnew
			org_startup_folded = 'content', -- Only show the first two levels
			org_log_into_drawer = 'LOGBOOK',
			org_log_done = 'time',
			org_agenda_span = 'day', -- day/week/month/year/number of days
			-- org_agenda_skip_scheduled_if_done = false,
			-- org_agenda_skip_deadline_if_done = false,
			org_time_stamp_rounding_minutes = 15,

			mappings = {
				global = {
					org_agenda = { 'gA', '<prefix>a' },
					org_capture = { 'gC', '<prefix>c' },
				},
				org = {
					org_todo = { 'cit', '<prefix>t'},
					org_agenda_set_tags = '<prefix>g',
					org_time_stamp = { '<prefix>i.', '<prefix>d' },
					org_change_date = { 'cid', '<prefix>D' },
					org_clock_in = { '<prefix>xi', '<prefix>i' },
					org_clock_out = { '<prefix>xo', '<prefix>x' },
					org_schedule = { '<prefix>is', '<prefix>s' },
					-- org_meta_return = '<CR>', -- Add heading, item or row (context-dependent)
					-- org_return = '<Leader><CR>',
					org_timestamp_up_day = false,
					org_timestamp_down_day = false,
					-- org_toggle_checkbox = '<C-Space>',
				},
				agenda = {
					org_agenda_later = '<',
					org_agenda_earlier = '>',
					org_agenda_goto_today = { '.', 'T' },
					org_agenda_switch_to = '<TAB>',
					org_agenda_goto = '<CR>',
				},
			}
		})
		
		-- Global key bindings
		vim.keymap.set('n', '<leader>a', function()
			-- org.agenda:agenda({ org_agenda_start_day = '-3d', show_clock_report = true })
			org.agenda:agenda({ show_clock_report = true })
		end)

		-- Custom org colors
		vim.api.nvim_set_hl(0, '@org.headline.level1', { link = 'markdownH1' })
		vim.api.nvim_set_hl(0, '@org.headline.level2', { link = 'markdownH2' })
		vim.api.nvim_set_hl(0, '@org.headline.level3', { link = 'markdownH3' })
		vim.api.nvim_set_hl(0, '@org.headline.level4', { link = 'markdownH4' })
		vim.api.nvim_set_hl(0, '@org.headline.level5', { link = 'markdownH5' })
		vim.api.nvim_set_hl(0, '@org.headline.level6', { link = 'markdownH6' })
		vim.api.nvim_set_hl(0, '@org.headline.level7', { link = 'markdownH6' })
		vim.api.nvim_set_hl(0, '@org.headline.level8', { link = 'markdownH6' })
		vim.api.nvim_set_hl(0, '@org.keyword.todo', { fg = '#9c1919', bold = true })
		vim.api.nvim_set_hl(0, '@org.keyword.done', { fg = '#779e4d', bold = true })
		vim.api.nvim_set_hl(0, '@org.keyword.plan', { fg = '#e334fa' })
		vim.api.nvim_set_hl(0, '@org.timestamp.active', { fg = '#e334fa' })
		vim.api.nvim_set_hl(0, '@org.properties', { fg = '#91866a' })
		vim.api.nvim_set_hl(0, '@org.drawer', { fg = '#8f8774' })
		vim.api.nvim_set_hl(0, '@org.table.delimiter', { fg = '#8f8774' })
		vim.api.nvim_set_hl(0, '@org.table.heading', { fg = '#a0a8b7' })
	end,
}
