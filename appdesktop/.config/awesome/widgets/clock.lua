local beautiful = require("beautiful")
local wibox = require("wibox")
local awful = require("awful")
local globalos = require("os")

local customwidget = {}

customwidget.icon = wibox.widget.imagebox(beautiful.clock)

-- Textclock widget

-- customwidget.widget = wibox.widget.textclock("%a %d %b  <span color='#ffffff'>%H:%M:%S</span>", 1)
customwidget.widget = wibox.widget.textclock("%a %d %b  <span color='#ffffff'>%T</span>", 1)

-- Tooltip
local widget_tooltip = awful.tooltip {
	objects        = { customwidget.widget, customwidget.icon },
	timer_function = function()
		return globalos.date('%A %d %B %Y\n%T')
	end,
}

return customwidget

