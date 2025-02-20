-- Pull in the wezterm API
local wezterm = require('wezterm')
local act = wezterm.action

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will help provide clearer error messages
if wezterm.config_builder then
	config = wezterm.config_builder()
end

-- Disable update notifications
config.check_for_updates = false

-- Disable missing glyph notification
config.warn_about_missing_glyphs = false

-- See https://gogh-co.github.io/Gogh/
config.color_scheme = 'Tango (terminal.sexy)'
-- config.color_scheme = 'Elementary'
-- config.color_scheme = 'Pro'

config.use_cap_height_to_scale_fallback_fonts = true
config.font = wezterm.font_with_fallback({
	'Hack Nerd Font',
	'Hack',
	{
		family = 'Noto Color Emoji',
		-- scale = 0.7,
		assume_emoji_presentation = true,
	},
	'DejaVu Sans Mono',
	'JetBrains Mono',
})
config.font_size = 9

config.hide_tab_bar_if_only_one_tab = true
config.tab_bar_at_bottom = true
-- Place window management buttons (minimize, maximize, close) into the tab bar instead of showing a title bar
config.window_decorations = "INTEGRATED_BUTTONS|RESIZE"

config.enable_scroll_bar = false
config.window_padding = {
	left = 1,
	right = 1,
	top = 0,
	bottom = 0,
}

-- Reset all bindings (https://wezfurlong.org/wezterm/config/default-keys.html)
config.disable_default_key_bindings = true
config.keys = {

	-- Fullscreen toggle
	{ key = 'F11',   mods = '',           action = act.ToggleFullScreen },

	-- Font size
	{ key = '-',     mods = 'CTRL',       action = act.DecreaseFontSize },
	{ key = '+',     mods = 'CTRL|SHIFT', action = act.IncreaseFontSize },
	{ key = '0',     mods = 'CTRL',       action = act.ResetFontSize },

	-- Copy
	{ key = 'c',     mods = 'CTRL|SHIFT', action = act.CopyTo('Clipboard') },
	{ key = 'Copy',  mods = '',           action = act.CopyTo('Clipboard') },

	-- Paste
	{ key = 'v',     mods = 'CTRL|SHIFT', action = act.PasteFrom('Clipboard') },
	{ key = 'Paste', mods = '',           action = act.PasteFrom('Clipboard') },

	-- Quickselect
	{ key = 'b',     mods = 'CTRL|SHIFT', action = act.QuickSelect },

	-- Command Palette
	{ key = 'p',     mods = 'CTRL|SHIFT', action = act.ActivateCommandPalette },

	-- -- Hide
	-- { key = 'm', mods = 'SUPER', action = 'DisableDefaultAssignment' },

	-- -- ToggleFullScreen
	-- { key = 'Enter', mods = 'ALT', action = 'DisableDefaultAssignment' },

	-- -- ShowDebugOverlay
	-- { key = 'L', mods = 'CTRL|SHIFT', action = 'DisableDefaultAssignment' },

	-- -- Open new Window
	-- { key = 'n', mods = 'SUPER', action = 'DisableDefaultAssignment' },
	-- { key = 'n', mods = 'SHIFT|CTRL', action = 'DisableDefaultAssignment' },

	-- -- Open new tab
	-- { key = 't', mods = 'SUPER', action = 'DisableDefaultAssignment' },
	-- { key = 't', mods = 'SHIFT|CTRL', action = 'DisableDefaultAssignment' },
	-- { key = 'T', mods = 'SHIFT|SUPER', action = 'DisableDefaultAssignment' },

	-- -- Close tab
	-- { key = 'w', mods = 'SHIFT|CTRL', action = 'DisableDefaultAssignment' },

	-- -- Tabs Navigation
	-- { key = '1', mods = 'SUPER', action = 'DisableDefaultAssignment' },
	-- { key = '2', mods = 'SUPER', action = 'DisableDefaultAssignment' },
	-- { key = '3', mods = 'SUPER', action = 'DisableDefaultAssignment' },
	-- { key = '4', mods = 'SUPER', action = 'DisableDefaultAssignment' },
	-- { key = '5', mods = 'SUPER', action = 'DisableDefaultAssignment' },
	-- { key = '6', mods = 'SUPER', action = 'DisableDefaultAssignment' },
	-- { key = '7', mods = 'SUPER', action = 'DisableDefaultAssignment' },
	-- { key = '8', mods = 'SUPER', action = 'DisableDefaultAssignment' },
	-- { key = '9', mods = 'SUPER', action = 'DisableDefaultAssignment' },
}

-- Return if a file is readable
function file_exists(name)
	local f = io.open(name, "r")
	if f ~= nil then
		io.close(f)
		return true
	else return false end
end

--- Override elements in the target table with values from the source table.
--
-- Note that this method doesn't copy entries found in `__index`.
-- Nested tables are copied by reference and not recursed into.
function table_crush(target, source, raw)
	if raw then
		for k, v in pairs(source) do
			rawset(target, k, v)
		end
	else
		for k, v in pairs(source) do
			target[k] = v
		end
	end

	return target
end

-- Include dotfile_custom config if exist
local dotfile_custom_path = os.getenv("SILVUSDOTFILES_CUSTOM") .. '/wezterm.lua'
if file_exists(dotfile_custom_path) then
	local config_custom = dofile(dotfile_custom_path)
	if config_custom then
		-- Override elements in the first table by the one in the second.
		table_crush(config, config_custom)
	end
end

-- and finally, return the configuration to wezterm
return config
