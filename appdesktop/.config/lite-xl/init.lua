-- Customs settings

-- this module will be loaded after everything else when the application starts
-- it will be automatically reloaded when saved
-- See https://lite-xl.com/en/documentation/usage
------------------------------ ------ ----------------------------------------


local core = require "core"
local keymap = require "core.keymap"
local config = require "core.config"
local style = require "core.style"

------------------------------ Themes ----------------------------------------

-- Theme:
-- core.reload_module("colors.focus")
core.reload_module("colors.bearded-theme-vivid-black")

--------------------------- Key bindings -------------------------------------

-- key binding:
-- keymap.add { ["ctrl+escape"] = "core:quit" }

-- pass 'true' for second parameter to overwrite an existing binding
-- keymap.add({ ["ctrl+pageup"] = "root:switch-to-previous-tab" }, true)
-- keymap.add({ ["ctrl+pagedown"] = "root:switch-to-next-tab" }, true)

keymap.add({ ["ctrl+e"] = "core:find-file" })
keymap.add({ ["ctrl+d"] = "doc:delete-lines" })
keymap.add({ ["ctrl+l"] = "doc:select-word" })
keymap.add({ ["ctrl+l"] = "find-replace:select-add-next" })

keymap.add({ ["ctrl+b"] = "treeview:toggle" })
keymap.add({ ["ctrl+t"] = "core:new-doc" })
keymap.add({ ["ctrl+q"] = "core:quit" })

keymap.add({ ["ctrl+pagedown"] = "root:switch-to-next-tab" })
keymap.add({ ["ctrl+pageup"] = "root:switch-to-previous-tab" })
keymap.add({ ["ctrl+shift+pagedown"] = "root:move-tab-right" })
keymap.add({ ["ctrl+shift+pageup"] = "root:move-tab-left" })

keymap.add({ ["alt+down"] = "root:switch-to-down" })
keymap.add({ ["alt+up"] = "root:switch-to-up" })
keymap.add({ ["alt+left"] = "root:switch-to-left" })
keymap.add({ ["alt+right"] = "root:switch-to-right" })

keymap.add({ ["shift+alt+down"] = "root:split-down" })
keymap.add({ ["shift+alt+up"] = "root:split-up" })
keymap.add({ ["shift+alt+left"] = "root:split-left" })
keymap.add({ ["shift+alt+right"] = "root:split-right" })

keymap.add({ ["return"] = "autocomplete:complete" })
keymap.add({ ["ctrl+shift+w"] = "core:change-project-folder" })
keymap.add({ ["ctrl+alt+s"] = "ui:settings" })


------------------------------- Fonts ----------------------------------------

-- customize fonts:
-- style.font = renderer.font.load("/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf", 11 * SCALE)
-- style.code_font = renderer.font.load("/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf", 12 * SCALE, {
-- 	antialiasing = "subpixel",
-- 	hinting = "slight",
-- 	smoothing = false,
-- 	bold = false,
-- 	italic = false,
-- 	underline = false,
-- 	strikethrough = false
-- })
style.code_font = renderer.font.load("/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf", 12 * SCALE)
style.font = renderer.font.load("/usr/share/fonts/truetype/firacode/FiraCode-Regular.ttf", 11 * SCALE)
-- style.code_font = renderer.font.load("/usr/share/fonts/truetype/firacode/FiraCode-Regular.ttf", 12 * SCALE)

-- style.font = renderer.font.load(DATADIR .. "/fonts/FiraSans-Regular.ttf", 14 * SCALE)
-- style.code_font = renderer.font.load(DATADIR .. "/fonts/JetBrainsMono-Regular.ttf", 14 * SCALE)
--
-- DATADIR is the location of the installed Lite XL Lua code, default color
-- schemes and fonts.
-- USERDIR is the location of the Lite XL configuration directory.
--
-- font names used by lite:
-- style.font          : user interface
-- style.big_font      : big text in welcome screen
-- style.icon_font     : icons
-- style.icon_big_font : toolbar icons
-- style.code_font     : code
--
-- the function to load the font accept a 3rd optional argument like:
--
-- {antialiasing="grayscale", hinting="full", bold=true, italic=true, underline=true, smoothing=true, strikethrough=true}
--
-- possible values are:
-- antialiasing: grayscale, subpixel
-- hinting: none, slight, full
-- bold: true, false
-- italic: true, false
-- underline: true, false
-- smoothing: true, false
-- strikethrough: true, false

------------------------------ Plugins ----------------------------------------

-- disable plugin loading setting config entries:

-- disable plugin detectindent, otherwise it is enabled by default:
-- config.plugins.detectindent = false

-- Show whitespaces
config.plugins.drawwhitespace = {
	enabled = true,
	show_middle = false,
	show_trailing_error = false
}
-- Trim whitespaces
config.plugins.trimwhitespace = {
	enabled = true,
	-- TODO: How to keep one line?
	trim_empty_end_lines = false
}

-- Disable ctrl+ mousewheel to zoom
config.plugins.scale = {
	use_mousewheel = false
}

-- Hide treeview on startup
-- TODO: Work only in user_settings.lua?
config.plugins.treeview = {
	visible = true
}

---------------------------- Miscellaneous -------------------------------------

-- modify list of files to ignore when indexing the project:
-- config.ignore_files = {
--   -- folders
--   "^%.svn/",        "^%.git/",   "^%.hg/",        "^CVS/", "^%.Trash/", "^%.Trash%-.*/",
--   "^node_modules/", "^%.cache/", "^__pycache__/",
--   -- files
--   "%.pyc$",         "%.pyo$",       "%.exe$",        "%.dll$",   "%.obj$", "%.o$",
--   "%.a$",           "%.lib$",       "%.so$",         "%.dylib$", "%.ncb$", "%.sdf$",
--   "%.suo$",         "%.pdb$",       "%.idb$",        "%.class$", "%.psd$", "%.db$",
--   "^desktop%.ini$", "^%.DS_Store$", "^%.directory$",
-- }

-- Indents
config.tab_type = "hard"
config.indent_size = 4

-- Small line height
config.line_height = 1.1

-- Allow scanning of big projets
config.max_project_files = 10000

-- Show only a small scrollbar
config.force_scrollbar_status = "contracted"

config.max_tabs = 4

