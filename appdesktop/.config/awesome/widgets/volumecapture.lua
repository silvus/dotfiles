local string = require("string")
local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")
local lain = require("lain")

local customwidget = {}

local volume_value = 0

customwidget.icon = wibox.widget.imagebox(beautiful.microphone)
customwidget.widget = wibox.container.margin(customwidget.icon, 3, 4, 3, 4)

-- ALSA volume capture bar

customwidget.volumecapture = lain.widget.alsa({
	channel = 'Capture',
	timeout = 2,
	settings = function()
		local level = tonumber(volume_now.level)
		volume_value = level
		if volume_now.status == "off" then
			customwidget.icon:set_image(beautiful.microphone_off)
		elseif level and level == 0 then
			customwidget.icon:set_image(beautiful.microphone_off)
			-- customwidget.icon:set_image(beautiful.volcapture_no)
		-- elseif level and level <= 20 then
		-- 	customwidget.icon:set_image(beautiful.volcapture_low)
		else
			customwidget.icon:set_image(beautiful.microphone)
		end
	end,
})

-- Tooltip
local widget_tooltip = awful.tooltip {
	objects        = { customwidget.widget},
	timer_function = function()
		return string.format("%d%%", volume_value)
	end,
}

-- events
buttons_event = awful.util.table.join (
	awful.button({}, 1, function()
		-- awful.spawn.with_shell(string.format("%s -e alsamixer", terminal))
		awful.spawn.with_shell('pavucontrol')
	end),
	awful.button({}, 2, function()
		awful.spawn(string.format("%s -D pulse set %s 100%%", customwidget.volumecapture.cmd, customwidget.volumecapture.channel))
		customwidget.volumecapture.update()
	end),
	awful.button({}, 3, function()
		awful.spawn(string.format("%s -D pulse set %s toggle", customwidget.volumecapture.cmd, customwidget.volumecapture.togglechannel or customwidget.volumecapture.channel))
		customwidget.volumecapture.update()
	end),
	awful.button({}, 4, function()
		awful.spawn(string.format("%s -D pulse set %s 5%%+", customwidget.volumecapture.cmd, customwidget.volumecapture.channel))
		customwidget.volumecapture.update()
	end),
	awful.button({}, 5, function()
		awful.spawn(string.format("%s -D pulse set %s 5%%-", customwidget.volumecapture.cmd, customwidget.volumecapture.channel))
		customwidget.volumecapture.update()
	end)
)
-- customwidget.icon:buttons(buttons_event)
customwidget.widget:buttons(buttons_event)

return customwidget
