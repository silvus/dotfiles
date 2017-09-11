
-- Theme
ui.set_theme(not CURSES and 'dark' or 'term')

-- Strip trailing whitespace on save
textadept.editing.strip_trailing_spaces = true

-- Run python as python 3
textadept.run.run_commands.python = 'python3 "%f"'
