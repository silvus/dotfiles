require('orgmode').setup({
	org_agenda_files = { '/data/doc/orgmode/**/*' },
	org_default_notes_file = '/data/doc/orgmode/todo.org',
	org_indent_mode = 'noindent',
	org_todo_keywords = {'TODO(t)', 'NEXT(n)', 'WAIT(w@/!)', '|', 'DONE(d!)', 'CANCELED(c@)', 'DELEGATED(g@)', 'INACTIVE(i@)'},
	win_split_mode = 'auto',
	org_log_into_drawer = 'LOGBOOK',
	org_log_done = 'time',
})



-- Load custom treesitter grammar for org filetype
require('orgmode').setup_ts_grammar()

