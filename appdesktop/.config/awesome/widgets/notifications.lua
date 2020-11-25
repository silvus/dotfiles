local awful  = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local naughty = require("naughty")

local customwidget = {}

-- Notifications status

customwidget.icon = wibox.widget.imagebox(beautiful.bell)
customwidget.widget = wibox.container.margin(customwidget.icon, 3, 4, 3, 4)


function update()
	if naughty.is_suspended() then
		customwidget.icon:set_image(beautiful.bell_slash)
	else
		customwidget.icon:set_image(beautiful.bell)
	end
end

customwidget.update = update

-- clics events
buttons_event = awful.util.table.join (
	awful.button({}, 1, function()
		naughty.toggle()
		update()
	end),
	awful.button({}, 2, function()
		naughty.notify({
			title = 'Test notification',
			text = "This a notification, for testing purpose.",
			ontop = true,
			-- preset = naughty.config.presets.critical
		})
	end),
	awful.button({}, 3, function()
		naughty.destroy_all_notifications()
		update()
	end)
)
customwidget.widget:buttons(buttons_event)


return customwidget
