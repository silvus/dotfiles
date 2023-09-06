local orgmode = require('orgmode')

local org = orgmode.setup({
	org_agenda_files = { '/data/doc/orgmode/**/*' },
	org_default_notes_file = '/data/doc/orgmode/todo.org',
	org_indent_mode = 'noindent', -- Disable indentation. All lines start from 1st column
	org_todo_keywords = {
		'TODO(t)',
		'NEXT(n)',
		'WAIT(w@/!)',
		'|',
		'DONE(d!)',
		'CANCELED(c@)',
		'DELEGATED(g@)',
		'INACTIVE(i@)' },
	win_split_mode = '80vsplit', -- auto, horizontal, vertical, { 'float', 0.3 }, tabnew
	org_log_into_drawer = 'LOGBOOK',
	org_log_done = 'time',
	org_agenda_span = 'day', -- day/week/month/year/number of days
	-- org_agenda_skip_scheduled_if_done = false,
	-- org_agenda_skip_deadline_if_done = false,
	-- org_time_stamp_rounding_minutes = 5,

	mappings = {
		global = {
			org_agenda = { 'gA', '<prefix>a' },
			org_capture = { 'gC', '<prefix>c' },
		},
		org = {
			org_todo = { 'cit', '<prefix>t' },
			org_time_stamp = { '<prefix>i.', '<prefix>d' },
			org_change_date = { 'cid', '<prefix>D' },
			org_clock_in = { '<prefix>xi', '<prefix>i' },
			org_clock_out = { '<prefix>xo', '<prefix>x' },
			org_schedule = { '<prefix>is', '<prefix>s' },
			org_meta_return = '<CR>', -- Add heading, item or row (context-dependent)
			org_return = '<Leader><CR>',
		},
		agenda = {
			org_agenda_later = '>',
			org_agenda_earlier = '<',
			org_agenda_goto_today = { '.', 'T' },
			org_agenda_switch_to = '<TAB>',
			org_agenda_goto = '<CR>',
		},
	}
})

-- Load custom treesitter grammar for org filetype
orgmode.setup_ts_grammar()

-- Global key bindings
vim.keymap.set('n', '<leader>a', function()
	-- org.agenda:agenda({ org_agenda_start_day = '-3d', show_clock_report = true })
	org.agenda:agenda({ show_clock_report = true })
end)
