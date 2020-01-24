local wibox = require("wibox")
local awful = require("awful")
local screens = require("screens")
local beautiful = require("beautiful")

local widget_battery = require("widgets.battery_mini")

local bar = {}

-- Bar (Wibox) management


-- Build a bar
function init(s)
	
	-- For main screen only
	if s == screens.get_primary() then

		-- Create a horizontal wibox (not a wibar)
		-- Left side is empty to insert a vertical wibar
		local wibox_custom = wibox({
			screen = s,
			visible = true,
			ontop = true,
			height = 2,
			width = s.geometry.width - 22,
			bg = beautiful.normal,
			border_width = 1,
			border_color = beautiful.normal,
			opacity = 1,
			type = 'dock',
			x = 22,
			y = s.geometry.height - 3,
		})

		-- Add widgets to the wibox
		wibox_custom:setup {
			layout = wibox.layout.align.horizontal,
			{
				layout = wibox.layout.fixed.horizontal,
				widget_battery.widget
			},
		}
	end

	return wibox_custom
end


bar.init = init

return bar
