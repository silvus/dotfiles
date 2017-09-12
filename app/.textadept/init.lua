
events.connect(events.LEXER_LOADED, function(lang)
	buffer.tab_width = 4
	buffer.use_tabs = true
end)

-- Theme
ui.set_theme(not CURSES and 'dark' or 'term')

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

local m_tools = textadept.menu.menubar[_L['_Tools']]
local m_quick_open = m_tools[_L['Quick _Open']]
keys['ce'] = m_quick_open[_L['Quickly Open _Current Directory']][2]
keys['cp'] = io.open_recent_file
