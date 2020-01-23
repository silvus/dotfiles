local awful = require("awful")
local beautiful = require("beautiful")

local screens = require("screens")

local desktops = {}


local tags_names = { "1", "2", "3", "4", "5", "6", "7", "8", "9", "0"}

-- Inits tags for each scren
local function init(s)

	if s == screens.get_primary() then
		local tags_definitions = {
			{
				name   = tags_names[1],
				icon   = beautiful.firefox,
			},
			{
				name   = tags_names[2],
				icon   = beautiful.code,
			},
			{
				name   = tags_names[3],
				icon   = beautiful.mail,
			},
			{
				name   = tags_names[4],
				icon   = beautiful.folder,
			},
			{
				name   = tags_names[5],
				icon   = beautiful.fire,
			},
			{
				name   = tags_names[6],
				icon   = beautiful.paint,
			},
			{
				name   = tags_names[7],
				icon   = beautiful.paragraph,
			},
			{
				name   = tags_names[8],
				layout = awful.layout.suit.max,
				icon   = beautiful.gamepad,
			},
			{
				name   = tags_names[9],
				icon   = beautiful.lock,
			},
			{  -- Scratchpad
				name   = tags_names[10],
				icon   = beautiful.terminal,
			},
		}
		
		-- Primary screen
		for i, tag in pairs(tags_definitions) do
			awful.tag.add(tag.name, {
				layout = (tag.layout or awful.layout.suit.tile),
				icon = tag.icon,
				screen = s,
				selected = (i == 1),
				-- icon_only = true,
			})
		end
	else
		-- secondary screens (Vertical layout)
		awful.tag(tags_names, s, awful.layout.suit.tile)

	end
end

-- Use to launch program only one time
-- local launched_list = {}

-- -- This function will run once every time Awesome is started, only one time per programm (https://github.com/lcpz/awesome-copycats/blob/master/rc.lua.template)
-- local function run_once(cmd_arr)
-- 	for _, cmd in ipairs(cmd_arr) do
-- 		-- if not already launched
-- 		if launched_list[cmd] == nil then
-- 			-- Add to programms launched list
-- 			launched_list[cmd] = true

-- 			-- Doesn't work with symlinks
-- 			-- awful.spawn.with_shell(string.format("pgrep -u $USER -fx '%s' > /dev/null || (%s)", cmd, cmd))
-- 			awful.spawn.with_shell(string.format("pgrep -u $USER \"$(basename %s)\" > /dev/null || (%s)", cmd, cmd))
-- 		end
-- 	end
-- end

-- -- Switch tag event
-- local function switch(t)
-- 	if t.selected then
-- 		-- Auto launch programms on tag access
-- 		if t.name == tags_names[1] then
-- 			run_once({"firefox"})
-- 		elseif t.name == tags_names[2] then
-- 			run_once({"/usr/share/codium/bin/codium"})
-- 		elseif t.name == tags_names[3] then
-- 			run_once({"thunderbird"})
-- 		elseif t.name == tags_names[4] then
-- 			run_once({"pcmanfm"})
-- 		end
-- 	end
-- end


desktops.init = init
-- desktops.switch = switch
desktops.tags_names = tags_names

return desktops
