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
				icon   = beautiful.note,
			},
			{
				name   = tags_names[3],
				icon   = beautiful.code,
				-- layout = awful.layout.suit.fair,
			},
			{
				name   = tags_names[4],
				icon   = beautiful.folder,
			},
			{
				name   = tags_names[5],
				icon   = beautiful.mail,
			},
			{
				name   = tags_names[6],
				icon   = beautiful.gamepad,
				layout = awful.layout.suit.max,
			},
			{
				name   = tags_names[7],
				icon   = beautiful.paragraph,
			},
			{
				name   = tags_names[8],
				icon   = beautiful.fire,
				-- layout = awful.layout.suit.max,
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
		-- secondary screens (One tag only)
		awful.tag({"S"..s.index}, s, awful.layout.suit.tile)

	end
end


desktops.init = init
desktops.tags_names = tags_names

return desktops

