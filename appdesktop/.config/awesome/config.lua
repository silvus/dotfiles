
local awful = require("awful")
local gears = require("gears")
-- Dmenu-like launcher
local menubar = require("menubar")


local config = {}


config.theme = gears.filesystem.get_configuration_dir() .. "themes/customblue/theme.lua"

-- This is used later as the default terminal and editor to run.
-- terminal = "x-terminal-emulator"
-- terminal = "urxvt"
config.terminal = "rxvt-unicode"
-- editor = os.getenv("EDITOR") or "editor"
-- editor_cmd = terminal .. " -e " .. editor

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
config.modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
config.layouts = {
	awful.layout.suit.tile,
	-- awful.layout.suit.tile.left,
	awful.layout.suit.tile.bottom,
	-- awful.layout.suit.tile.top,
	-- awful.layout.suit.spiral,
	-- awful.layout.suit.spiral.dwindle,
	awful.layout.suit.fair,
	-- awful.layout.suit.fair.horizontal,
	awful.layout.suit.max,
	-- awful.layout.suit.max.fullscreen,
	-- awful.layout.suit.magnifier,
	-- awful.layout.suit.floating,
	-- awful.layout.suit.corner.nw,
	-- awful.layout.suit.corner.ne,
	-- awful.layout.suit.corner.sw,
	-- awful.layout.suit.corner.se,
}

return config