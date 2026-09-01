local beautiful = require("beautiful")
local wibox = require("wibox")
local awful = require("awful")
local globalos = require("os")

local customwidget = {}

-- Textclock widget
local icon = wibox.widget.imagebox(beautiful.clock)
customwidget.icon = wibox.container.margin(icon, 3, 4, 3, 4)

customwidget.widget = wibox.container {
	wibox.widget.textclock("<span color='#ffffff'>%T</span>", 1),
	direction = 'east',
	widget = wibox.container.rotate
}

-- Tooltip
awful.tooltip {
	objects = { customwidget.widget, customwidget.icon },
	timer_function = function()
		return globalos.date('%A %d %B %Y\n%T')
	end,
}

return customwidget
