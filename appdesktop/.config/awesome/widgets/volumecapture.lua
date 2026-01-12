local string = require("string")
local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
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
	objects        = { customwidget.widget },
	timer_function = function()
		return string.format("%d%%", volume_value)
	end,
}

-- events
local buttons_event = awful.util.table.join(
	awful.button({}, 1, function()
		awful.spawn("pavucontrol")
	end),
	awful.button({}, 2, function()
		awful.spawn("pactl set-source-volume @DEFAULT_SOURCE@ 100%")
		customwidget.volumecapture.update()
	end),
	awful.button({}, 3, function()
		awful.spawn("pactl set-source-mute @DEFAULT_SOURCE@ toggle")
		customwidget.volumecapture.update()
	end),
	awful.button({}, 4, function()
		awful.spawn("pactl set-source-volume @DEFAULT_SOURCE@ +5%")
		customwidget.volumecapture.update()
	end),
	awful.button({}, 5, function()
		awful.spawn("pactl set-source-volume @DEFAULT_SOURCE@ -5%")
		customwidget.volumecapture.update()
	end)
)

-- customwidget.icon:buttons(buttons_event)
customwidget.widget:buttons(buttons_event)

return customwidget
