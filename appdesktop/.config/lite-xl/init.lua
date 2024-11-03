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
keymap.add({ ["ctrl+b"] = "treeview:toggle" })
keymap.add({ ["ctrl+t"] = "core:new-doc" })
keymap.add({ ["ctrl+q"] = "core:quit" })
keymap.add({ ["return"] = "autocomplete:complete" })
keymap.add({ ["ctrl+shift+w"] = "core:change-project-folder" })


------------------------------- Fonts ----------------------------------------

-- customize fonts:
style.font = renderer.font.load("/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf", 11 * SCALE)
style.code_font = renderer.font.load("/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf", 12 * SCALE)

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

config.tab_type = "hard"
config.indent_size = 4
config.line_height = 1.1
