-- Modules
-- ----------------------------------------------------------------------------
local textredux = require('textredux')
textredux.hijack()

local lsp = require('lsp')
lsp.server_commands.python = 'python-language-server'

require('file_diff')
require('spellcheck')

-- Theme
-- ----------------------------------------------------------------------------
-- Adjust the default theme's font and size.
if not CURSES then
	-- view:set_theme('base16-onedark', {font = 'DejaVu Sans Mono', size = 9})
	view:set_theme('base16-default-dark', {font = 'DejaVu Sans Mono', size = 9})
	-- view:set_theme('dark', {font = 'DejaVu Sans Mono', size = 9})
end
-- buffer:set_theme(not CURSES and 'base16-default-dark' or 'term')


-- Config
-- ----------------------------------------------------------------------------
-- Tabs
buffer.use_tabs = true
buffer.tab_width = 4

-- Multiple Cussor
buffer.multiple_selection = true
-- Paste into all selections
buffer.multi_paste = buffer.MULTIPASTE_EACH

-- Show trailing whitespace
-- buffer.view_ws = buffer.WS_VISIBLEAFTERINDENT
-- wrap long lines, always
-- buffer.wrap_mode = buffer.WRAP_WHITESPACE

-- Strip trailing spaces on save
textadept.editing.strip_trailing_spaces = true
-- Automatically highlight the current word
textadept.editing.highlight_words = textadept.editing.HIGHLIGHT_CURRENT

-- Create a key binding to the "Edit > Preferences" menu item.
-- if not OSX and not CURSES then
-- 	keys['ctrl+,'] = textadept.menu.menubar['Edit/Preferences'][2]
-- end

-- Recognize .luadoc files as Lua code.
lexer.detect_extensions.luadoc = 'lua'

-- Change the run commands for Lua and Python
-- textadept.run.run_commands.lua = 'lua5.1 "%f"'
textadept.run.run_commands.python = 'python3 "%f"'

-- Always use PEP-8 indentation style for Python files, and spaces for YAML files.
events.connect(events.LEXER_LOADED, function(name)
	if name == 'python' or name == 'yaml' then
		buffer.use_tabs = false
		buffer.tab_width = 4
	end
end)


-- Keymap
-- ----------------------------------------------------------------------------
keys['cg'] = textadept.editing.goto_line

-- Buffers
keys['ct'] = buffer.new
keys['cd'] = buffer.line_delete
keys['ce'] = textredux.buffer_list.show
-- keys['cE'] = Command Selection
-- local m_buffer = textadept.menu.menubar[_L['_Buffer']]
-- keys['cpgdn'] = m_buffer[_L['_Next Buffer']][2]
-- keys['cpgup'] = m_buffer[_L['_Previous Buffer']][2]

-- Files
io.quick_open_max = 10000
keys['cp'] = function() io.quick_open(io.get_project_root()) end
-- keys['cP'] = io.open_recent_file
-- keys['co'] = Open
keys['cO'] = io.open_recent_file

-- Execute
-- local m_tools = textadept.menu.menubar[_L['_Tools']]
-- keys['cX'] = m_tools[_L['Command _Entry']][2]
keys['f5'] = textadept.run.run

-- Multiple selections
keys['cl'] = textadept.editing.select_word

-- Fold
-- local m_view = textadept.menu.menubar[_L['_View']]
-- keys['c\n'] = m_view[_L['Toggle Current _Fold']][2]

-- Autocomplete
-- local m_edit = textadept.menu.menubar[_L['_Edit']]
-- keys['c '] = m_edit[_L['Complete _Word']][2]
-- keys['c '] = m_tools[_L['_Complete Symbol']][2]

-- keys['ctrl+f12'] = lsp.goto_declaration
