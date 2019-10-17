local awful = require("awful")
local beautiful = require("beautiful")

local screens = require("screens")

local desktops = {}


local tags_names = { "1", "2", "3", "4", "5", "6", "7", "8", "9", "0"}

-- Inits tags for each scren
local function init(s)

	if s == screens.get_primary() then
		awful.tag.add(tags_names[1], {
			layout             = awful.layout.suit.tile,
			screen             = s,
			selected           = true,
			icon               = beautiful.firefox,
			-- icon_only          = true,
		})
		awful.tag.add(tags_names[2], {
			layout             = awful.layout.suit.tile,
			screen             = s,
			icon               = beautiful.code,
		})
		awful.tag.add(tags_names[3], {
			layout             = awful.layout.suit.max,
			screen             = s,
			icon               = beautiful.mail,
		})
		awful.tag.add(tags_names[4], {
			layout             = awful.layout.suit.tile,
			screen             = s,
			icon               = beautiful.folder,
		})
		awful.tag.add(tags_names[5], {
			layout             = awful.layout.suit.tile,
			screen             = s,
			icon               = beautiful.fire,
		})
		awful.tag.add(tags_names[6], {
			layout             = awful.layout.suit.tile,
			screen             = s,
			icon               = beautiful.paint,
		})
		awful.tag.add(tags_names[7], {
			layout             = awful.layout.suit.tile,
			screen             = s,
			icon               = beautiful.paragraph,
		})
		awful.tag.add(tags_names[8], {
			layout             = awful.layout.suit.max,
			screen             = s,
			icon               = beautiful.gamepad,
		})
		awful.tag.add(tags_names[9], {
			layout             = awful.layout.suit.tile,
			screen             = s,
			icon               = beautiful.lock,
		})

		-- Scratchpad
		awful.tag.add(tags_names[10], {
			layout = awful.layout.suit.tile,
			screen = s,
			icon = beautiful.terminal,
		})

	else
		-- secondary screens (Vertical layout)
		awful.tag(tags_names, s, awful.layout.suit.tile.bottom)

	end
end

-- Use to launch program only one time
local launched_list = {}

-- This function will run once every time Awesome is started, only one time per programm (https://github.com/lcpz/awesome-copycats/blob/master/rc.lua.template)
local function run_once(cmd_arr)
	for _, cmd in ipairs(cmd_arr) do
		-- if not already launched
		if launched_list[cmd] == nil then
			-- Add to programms launched list
			launched_list[cmd] = true

			-- Doesn't work with symlinks
			-- awful.spawn.with_shell(string.format("pgrep -u $USER -fx '%s' > /dev/null || (%s)", cmd, cmd))
			awful.spawn.with_shell(string.format("pgrep -u $USER \"$(basename %s)\" > /dev/null || (%s)", cmd, cmd))
		end
	end
end

-- Switch tag event
local function switch(t)
	if t.selected then
		-- Auto launch programms on tag access
		if t.name == tags_names[1] then
			run_once({"firefox"})
		elseif t.name == tags_names[2] then
			run_once({"/usr/share/codium/bin/codium"})
		elseif t.name == tags_names[3] then
			run_once({"thunderbird"})
		elseif t.name == tags_names[4] then
			run_once({"pcmanfm"})
		end
	end
end


desktops.init = init
desktops.switch = switch
desktops.tags_names = tags_names

return desktops
