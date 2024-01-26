-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will help provide clearer error messages
if wezterm.config_builder then
	config = wezterm.config_builder()
end

-- See https://gogh-co.github.io/Gogh/
config.color_scheme = 'Elementary'
-- config.color_scheme = 'Pro'

config.font = wezterm.font 'Hack Nerd Font'
config.font_size = 8

config.hide_tab_bar_if_only_one_tab = true
config.tab_bar_at_bottom = true

config.enable_scroll_bar = true

-- Reset some bindings
config.keys = {

	-- Fullscreen toggle
	{ key = 'F11', mods = '', action = 'ToggleFullScreen' },

	-- Hide
	{ key = 'm', mods = 'SUPER', action = 'DisableDefaultAssignment' },
	
	-- Open new Window
	{ key = 'n', mods = 'SUPER', action = 'DisableDefaultAssignment' },
	{ key = 'n', mods = 'SHIFT|CTRL', action = 'DisableDefaultAssignment' },
	
	-- Open new tab
	{ key = 't', mods = 'SUPER', action = 'DisableDefaultAssignment' },
	{ key = 't', mods = 'SHIFT|CTRL', action = 'DisableDefaultAssignment' },
	{ key = 'T', mods = 'SHIFT|SUPER', action = 'DisableDefaultAssignment' },
	
	-- Close tab
	{ key = 'w', mods = 'SHIFT|CTRL', action = 'DisableDefaultAssignment' },
	
	-- Tabs Navigation
	{ key = '1', mods = 'SUPER', action = 'DisableDefaultAssignment' },
	{ key = '2', mods = 'SUPER', action = 'DisableDefaultAssignment' },
	{ key = '3', mods = 'SUPER', action = 'DisableDefaultAssignment' },
	{ key = '4', mods = 'SUPER', action = 'DisableDefaultAssignment' },
	{ key = '5', mods = 'SUPER', action = 'DisableDefaultAssignment' },
	{ key = '6', mods = 'SUPER', action = 'DisableDefaultAssignment' },
	{ key = '7', mods = 'SUPER', action = 'DisableDefaultAssignment' },
	{ key = '8', mods = 'SUPER', action = 'DisableDefaultAssignment' },
	{ key = '9', mods = 'SUPER', action = 'DisableDefaultAssignment' },
}

-- and finally, return the configuration to wezterm
return config
