local os = require("os")
local awful = require("awful")
local table = require("gears.table")

-- Return if a file is readable
function file_exists(name)
	local f = io.open(name, "r")
	if f ~= nil then
		io.close(f)
		return true
	else
		return false
	end
end

local config = {}

config.home = os.getenv("HOME") or "/home/silvus"
config.dotfiles = os.getenv("SILVUSDOTFILES") or "/home/silvus/.dotfiles"
config.dotfiles_custom = os.getenv("SILVUSDOTFILES_CUSTOM") or "/home/silvus/.dotfiles/custom"

-- config.theme = "lagoon"
config.theme = "matrix"
-- config.theme = "bloodmoon"

-- This is used later as the default terminal and guake-like terminal to run.
if file_exists("/usr/bin/wezterm") then
	config.terminal = "/usr/bin/wezterm"
	config.terminal_quake = '/usr/bin/wezterm start --class guaketerm -- "' .. config.dotfiles .. '/bin/tmuxdev"'
elseif file_exists("/usr/bin/urxvt") then
	config.terminal = "/usr/bin/urxvt"
	config.terminal_quake = '/usr/bin/urxvt -name guaketerm -title terminal -e "' .. config.dotfiles .. '/bin/tmuxdev"'
else
	config.terminal = "xterm"
end
-- editor = os.getenv("EDITOR") or "editor"
-- editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
config.modkey = "Mod4"

-- Videos windows placement (auto, float, fullscreen, tiled)
config.rules_videos = "auto"

-- Show bar at start
config.show_bar = true

-- Set a different wallpaper for each tag (and each screen), else take a random on init
config.wallpapers_by_tag = false

-- Use blink1 led to flash on notification (if installed)
config.blink_path = nil
if file_exists(config.home .. "/bin/blink1-flash") then
	config.blink_path = config.home .. "/bin/blink1-flash"
elseif file_exists("/usr/local/blink-tool") then
	config.blink_path = "/usr/local/blink-tool"
end

-- Table of layouts to cover with awful.layout.inc, order matters.
config.layouts = {
	awful.layout.suit.tile, -- Can be resized
	-- awful.layout.suit.tile.left,
	-- awful.layout.suit.tile.bottom,
	-- awful.layout.suit.tile.top,

	awful.layout.suit.fair,
	-- awful.layout.suit.fair.horizontal,

	-- awful.layout.suit.spiral,
	-- awful.layout.suit.spiral.dwindle,

	awful.layout.suit.max,
	-- awful.layout.suit.max.fullscreen,
	-- awful.layout.suit.magnifier,
	-- awful.layout.suit.floating,

	-- awful.layout.suit.corner.nw,
	-- awful.layout.suit.corner.ne,
	-- awful.layout.suit.corner.sw,
	-- awful.layout.suit.corner.se,
}

-- Include config customisation to override previous default values
-- This file should return a table, for exemple:
-- local config = {}
-- config.theme = "customblue"
-- return config
local config_custom_path = config.dotfiles_custom .. "/awesome.lua"
if file_exists(config_custom_path) then
	local config_custom = dofile(config_custom_path)
	if config_custom then
		-- Override elements in the first table by the one in the second.
		table.crush(config, config_custom)
	end
end

return config

