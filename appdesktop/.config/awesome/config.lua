
local os = require("os")
local awful = require("awful")
local gears = require("gears")
-- Dmenu-like launcher
local menubar = require("menubar")
local lain = require("lain")


local config = {}

config.home = os.getenv("HOME")


-- config.theme = "customblue"
config.theme = "matrix"

-- This is used later as the default terminal and editor to run.
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

	lain.layout.termfair,
	-- lain.layout.termfair.center,
	-- lain.layout.cascade,
	-- lain.layout.cascade.tile,
	lain.layout.centerwork,
	-- lain.layout.centerwork.horizontal,
	
	-- awful.layout.suit.tile.top,
	-- awful.layout.suit.spiral,
	-- awful.layout.suit.spiral.dwindle,
	-- awful.layout.suit.fair,
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

lain.layout.termfair.nmaster = 2
lain.layout.termfair.ncol    = 1
lain.layout.termfair.center.nmaster = 2
lain.layout.termfair.center.ncol    = 1

return config