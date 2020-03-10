local screens = {}

local awful = require("awful")
local io = require("io")

-- Set global screen variable to a local one
-- local screen = screen

-- Execute specific xrandr script
-- Init can not be done each time we start awesome because of add screen event (infinite loop)
-- local function init()
-- 	awful.spawn.with_shell("~/.dotfiles/bin/autostart_screen")
-- end

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


-- screens.init = init
screens.count = count
screens.get_primary = get_primary

return screens
