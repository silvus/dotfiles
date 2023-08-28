require('orgmode').setup({
	org_agenda_files = { '/data/doc/orgmode/**/*' },
	org_default_notes_file = '/data/doc/orgmode/todo.org',
})



-- Load custom treesitter grammar for org filetype
require('orgmode').setup_ts_grammar()
