local textredux = require('textredux')
textredux.hijack()

events.connect(events.LEXER_LOADED, function(lexer)
	-- Tabs
	buffer.tab_width = 4
	buffer.use_tabs = true

	if lexer == 'markdown' then
		-- No auto pairing in markdown
		-- textadept.editing.auto_pairs = nil
		textadept.editing.brace_matches = {}
	end
end)

-- Theme
-- ui.set_theme(not CURSES and 'dark' or 'term')
-- From https://github.com/rgieseke/textadept-themes
ui.set_theme(not CURSES and 'base16-default-dark' or 'term')
--ui.set_theme(not CURSES and 'base16-ocean-dark' or 'term')
-- ui.set_theme(not CURSES and 'base16-solarized-dark' or 'term')
--ui.set_theme(not CURSES and 'base16-monokai-dark' or 'term')

-- Strip trailing whitespace on save
textadept.editing.strip_trailing_spaces = true

-- Run python as python 3
textadept.run.run_commands.python = 'python3 "%f"'

-- Keymap
keys['cP'] = textadept.menu.select_command
keys['ct'] = buffer.new
keys['cd'] = buffer.line_delete

local m_buffer = textadept.menu.menubar[_L['_Buffer']]
keys['cpgdn'] = m_buffer[_L['_Next Buffer']][2]
keys['cpgup'] = m_buffer[_L['_Previous Buffer']][2]

-- Already on default
-- keys['cE'] = Command Selection
keys['cp'] = io.open_recent_file
-- shows buffers by their z-order (most recently viewed to least recently viewed
-- keys['ce'] = function() ui.switch_buffer(true) end
keys['ce'] = textredux.buffer_list.show

