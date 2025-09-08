-- Customs settings
-- See ~/.config/lite-xl/user_settings.lua

-- put user settings here
-- this module will be loaded after everything else when the application starts
-- it will be automatically reloaded when saved

local core = require("core")
local keymap = require("core.keymap")
local config = require("core.config")
local style = require("core.style")
local common = require("core.common")
local formatter = require("plugins.formatter")
local lsp = require("plugins.lsp")
local lspconfig = require("plugins.lsp.config")

------------------------------ Themes ----------------------------------------

-- Theme:
-- core.reload_module("colors.everforest")
-- core.reload_module("colors.everblush")
core.reload_module("colors.custom_everblush")

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

-- keymap.add({ ["return"] = "autocomplete:complete" })
keymap.add({ ["ctrl+shift+w"] = "core:change-project-folder" })
keymap.add({ ["ctrl+alt+s"] = "ui:settings" })
keymap.add({ ["ctrl+shift+c"] = "copy-file-location:copy-file-location" })
keymap.add({ ["alt+t"] = "datetimestamps:insert-datestamp" })
keymap.add({ ["ctrl+l"] = "find-replace:select-add-next" })

keymap.add({ ["ctrl+pageup"] = "root:switch-to-previous-tab" })
keymap.add({ ["ctrl+pagedown"] = "root:switch-to-next-tab" })
keymap.add({ ["ctrl+shift+pagedown"] = "root:move-tab-right" })
keymap.add({ ["ctrl+shift+pageup"] = "root:move-tab-left" })

keymap.add({ ["ctrl+up"] = "doc:move-to-previous-block-start" })
keymap.add({ ["ctrl+down"] = "doc:move-to-next-block-end" })
keymap.add({ ["ctrl+alt+up"] = "doc:move-lines-up" })
keymap.add({ ["ctrl+alt+down"] = "doc:move-lines-down" })
keymap.add({ ["ctrl+shift+up"] = "doc:create-cursor-previous-line" })
keymap.add({ ["ctrl+shift+down"] = "doc:create-cursor-next-line" })

keymap.add({ ["alt+up"] = "root:switch-to-up" })
keymap.add({ ["alt+down"] = "root:switch-to-down" })
keymap.add({ ["alt+left"] = "root:switch-to-left" })
keymap.add({ ["alt+right"] = "root:switch-to-right" })

keymap.add({ ["alt+shift+down"] = "root:split-down" })
keymap.add({ ["alt+shift+up"] = "root:split-up" })
keymap.add({ ["alt+shift+left"] = "root:split-left" })
keymap.add({ ["alt+shift+right"] = "root:split-right" })

------------------------------- Fonts ----------------------------------------

-- customize fonts:
-- style.font = renderer.font.load(DATADIR .. "/fonts/FiraSans-Regular.ttf", 14 * SCALE)
-- style.code_font = renderer.font.load(DATADIR .. "/fonts/JetBrainsMono-Regular.ttf", 14 * SCALE)
--
-- DATADIR is the location of the installed Pragtical Lua code, default color
-- schemes and fonts.
-- USERDIR is the location of the Pragtical configuration directory.
--
-- font names used by pragtical:
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

local home = os.getenv("HOME")

local function file_exists(path)
	local f = io.open(path, "r")
	if f then
		f:close()
		return true
	end
	return false
end

if file_exists("/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf") then
	style.code_font = renderer.font.load("/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf", 12 * SCALE)
	style.font = renderer.font.load("/usr/share/fonts/truetype/firacode/FiraCode-Regular.ttf", 11 * SCALE)
else
	-- No /usr/share/fonts in Nixos
	style.code_font = renderer.font.load(home .. "/.local/share/fonts/dejavu/DejaVuSansMono.ttf", 12 * SCALE)
	style.font = renderer.font.load(home .. "/.local/share/fonts/hack/Hack-Regular.ttf", 11 * SCALE)
end

------------------------------ Plugins ----------------------------------------

-- disable plugin loading setting config entries:

-- disable plugin detectindent, otherwise it is enabled by default:
-- config.plugins.detectindent = false

-- Show whitespaces
config.plugins.drawwhitespace = {
	enabled = true,
	show_middle = false,
	show_trailing_error = false,
}
-- Trim whitespaces
config.plugins.trimwhitespace = {
	enabled = true,
	trim_empty_end_lines = false,
}

-- Disable ctrl+ mousewheel to zoom
config.plugins.scale = {
	use_mousewheel = false,
}

-- Format datetime inserted
config.plugins.datetimestamps = {
	format_datestamp = "%Y-%m-%d",
}

-- Bracket matching style
config.plugins.bracketmatch = {
	style = "frame",
	color_char = true,
}

-- Alert when file is changed
config.plugins.autoreload = {
	always_show_nagview = true,
}

-- Hide treeview on startup
config.plugins.treeview = {
	visible = true,
	expand_dirs_to_focused_file = true,
	scroll_to_focused_file = true,
	show_hidden = true,
}

-- Format
config.plugins.formatter = {
	-- TODO: create a 'save without format' command and enable format_on_save
	format_on_save = false,
}

-- LSP
config.plugins.lsp = {
	mouse_hover = false,
	-- show_diagnostics = false
}
formatter.config("clangformat", {
	-- Disable this formater (used on invalid files)
	enabled = false,
})

-- Python
formatter.config("ruff", {
	file_patterns = {
		"%.py$",
		"%/dotfiles",
		"%/dotinstall",
		"%/tmux_sessionizer",
	},
})
lspconfig.pyright.setup(common.merge({
	file_patterns = {
		"%.py$",
		"%/dotfiles",
		"%/dotinstall",
		"%/tmux_sessionizer",
	},
	command = {
		os.getenv("HOME") .. "/.nix-profile/bin/pyright-langserver",
	},
	-- verbose = true,
}, config.plugins.lsp_python or {}))
-- lsp.add_server {
-- 	-- Name of server
-- 	name = "ty",
-- 	-- Main language
-- 	language = "python",
-- 	-- If no pattern matches, the file extension is used instead.
-- 	-- File types that are supported by this server
-- 	file_patterns = { "%.py$", '%/dotfiles' },
-- 	-- LSP command and optional arguments
-- 	command = { os.getenv("HOME") .. "/.nix-profile/bin/ty", "server"},

-- 	 -- True to debug the lsp client when developing it
--   verbose = true,
-- }

-- Shell
formatter.config("shformat", {
	path = os.getenv("HOME") .. "/.nix-profile/bin/shfmt",
	file_patterns = {
		"%.sh$",
		"%.bash$",
		"%/.bashrc",
		"%/install-pragtical",
	},
})
lspconfig.bashls.setup(common.merge({
	command = {
		os.getenv("HOME") .. "/.nix-profile/bin/bash-language-server",
		"start",
	},
	file_patterns = {
		"%.sh$",
		"%.bash$",
		"%/.bashrc",
		"%/install-pragtical",
	},
	verbose = true,
}, config.plugins.bashls or {}))

-- Lua
formatter.config("stylua", {
	path = os.getenv("HOME") .. "/.nix-profile/bin/stylua",
})
lspconfig.sumneko_lua.setup(common.merge({
	command = {
		os.getenv("HOME") .. "/.nix-profile/bin/lua-language-server",
	},
}, config.plugins.lsp_lua or {}))

-- Nix
formatter.add_formatter({
	name = "nixfmt",
	label = "nixfmt",
	file_patterns = { "%.nix$" },
	command = { "nixfmt", "$ARGS", "$FILENAME" },
	path = os.getenv("HOME") .. "/.nix-profile/bin/nixfmt",
})
lsp.add_server({
	-- Name of server
	name = "nil",
	-- Main language
	language = "Nix",
	-- If no pattern matches, the file extension is used instead.
	-- File types that are supported by this server
	file_patterns = { "%.nix$" },
	-- LSP command and optional arguments
	command = { os.getenv("HOME") .. "/.nix-profile/bin/nil" },
	-- Set by default to 16 should only be modified if having issues with a server
	-- requests_per_second = 16,
	-- True to debug the lsp client when developing it
	-- verbose = true,
	settings = {
		["nil"] = {
			formatting = {
				command = { os.getenv("HOME") .. "/.nix-profile/bin/nixfmt" },
			},
		},
	},
})

-- Rust
lspconfig.rust_analyzer.setup(common.merge({
	command = {
		-- os.getenv("HOME") .. "/.nix-profile/bin/rust-analyzer",
		os.getenv("HOME") .. "/.local/share/cargo/bin/rust-analyzer",
	},
}, config.plugins.lsp_rust or {}))

------------------ Miscellaneous -------------------------------------

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

---Default line endings for new files.
config.line_endings = "lf"

-- Indents
config.tab_type = "hard"
config.indent_size = 4

-- Small line height
config.line_height = 1.1

-- Allow scanning of big projets
config.max_project_files = 10000

-- Show only a small scrollbar
config.force_scrollbar_status = "contracted"

-- Small number of tabs show
config.max_tabs = 1
-- config.hide_tabs = true

