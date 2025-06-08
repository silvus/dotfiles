local screens = {}

local awful = require("awful")
local io = require("io")

local config = require("config")

-- Set global screen variable to a local one
-- local screen = screen

-- Execute specific xrandr script
local function init()
	-- Update primary and screens change after xrandr command
	-- TODO: how to update wibar and tags on primary changed ?
	screen.emit_signal("list")
	screen.emit_signal("primary_changed")
end

-- Count numbers of screens
local function count()
	if screen.count() then
		return screen:count()
	end

	-- Fallback to at least one screen
	return 1
end

-- Get primary screen
local function get_primary()
	return screen.primary
end

screens.init = init
screens.count = count
screens.get_primary = get_primary

return screens

