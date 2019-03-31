-- https://notabug.org/reback00/ta-config/src/master/init.lua

-- Modules
-- ----------------------------------------------------------------------------
local textredux = require('textredux')
textredux.hijack()


-- Buffers options
-- ----------------------------------------------------------------------------
events.connect(events.LEXER_LOADED, function(lexer)
	-- Multiple Cussor
	buffer.multiple_selection = true
	-- Paste into all selections
	buffer.multi_paste = buffer.MULTIPASTE_EACH

	-- Tabs
	buffer.tab_width = 4
	buffer.use_tabs = true
	-- show trailing whitespace
	-- buffer.view_ws = buffer.WS_VISIBLEAFTERINDENT
	-- wrap long lines, always
	buffer.wrap_mode = buffer.WRAP_WHITESPACE
	-- paste on multiple cursor places
	buffer.multi_paste = buffer.MULTIPASTE_EACH

	if lexer == 'markdown' then
		-- No auto pairing in markdown
		-- textadept.editing.auto_pairs = nil
		textadept.editing.brace_matches = {}
		-- Contract folds
		buffer:fold_all(buffer.FOLDACTION_CONTRACT)
	end
end)

-- highlight trailing whitespace
-- from: https://foicica.com/wiki/highlight-trailing-whitespace
local tw_indicator = _SCINTILLA.next_indic_number()
buffer.indic_style[tw_indicator] = buffer.INDIC_ROUNDBOX
buffer.indic_fore[tw_indicator] = 0x0000FF
events.connect(events.UPDATE_UI, function(updated)
	if updated ~= buffer.UPDATE_CONTENT then return end
	buffer.target_start = 0
	buffer.search_flags = buffer.FIND_REGEXP
	buffer.indicator_current = tw_indicator
	buffer:indicator_clear_range(0, buffer.length)
	while true do
	buffer.target_end = buffer.length
	if buffer:search_in_target('[ \t]+$') == -1 then break end
		buffer:indicator_fill_range(
		buffer.target_start, buffer.target_end - buffer.target_start)
		buffer.target_start = buffer.target_end
	end
end)


-- Config
-- ----------------------------------------------------------------------------
-- ui.set_theme(not CURSES and 'dark' or 'term')
-- From https://github.com/rgieseke/textadept-themes
buffer:set_theme(not CURSES and 'base16-default-dark' or 'term')
-- ui.set_theme(not CURSES and 'base16-ocean-dark' or 'term')
-- ui.set_theme(not CURSES and 'base16-solarized-dark' or 'term')
-- ui.set_theme(not CURSES and 'base16-monokai-dark' or 'term')

-- Strip trailing whitespace on save
textadept.editing.strip_trailing_spaces = true

-- Run python as python 3
textadept.run.run_commands.python = 'python3 "%f"'


-- Keymap
-- ----------------------------------------------------------------------------
keys['cg'] = textadept.editing.goto_line

-- Buffers
keys['ct'] = buffer.new
keys['cd'] = buffer.line_delete
keys['ce'] = textredux.buffer_list.show
-- keys['cE'] = Command Selection
local m_buffer = textadept.menu.menubar[_L['_Buffer']]
keys['cpgdn'] = m_buffer[_L['_Next Buffer']][2]
keys['cpgup'] = m_buffer[_L['_Previous Buffer']][2]

-- Files
keys['cp'] = function() io.quick_open(io.get_project_root(), '!__pycache__') end
-- keys['cP'] = io.open_recent_file
-- keys['co'] = Open
keys['cO'] = io.open_recent_file

-- Execute
local m_tools = textadept.menu.menubar[_L['_Tools']]
keys['cX'] = m_tools[_L['Command _Entry']][2]
keys['f5'] = textadept.run.run

-- Multiple selections
keys['cl'] = textadept.editing.select_word

-- Fold
local m_view = textadept.menu.menubar[_L['_View']]
keys['c\n'] = m_view[_L['Toggle Current _Fold']][2]
