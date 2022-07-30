local beautiful = require("beautiful")
local wibox = require("wibox")
local awful = require("awful")
local globalos = require("os")

local customwidget = {}

customwidget.icon = wibox.widget.imagebox(beautiful.clock)

-- Textclock widget

-- customwidget.widget = wibox.widget.textclock("<span color='#000000' size='x-small'>H</span><span color='#ffffff'>%H</span>\n<span color='#000000' size='x-small'>M</span><span color='#ffffff'>%M</span>\n<span color='#000000' size='x-small'>S</span><span color='#ffffff'>%S</span>", 1)
-- customwidget.widget = wibox.widget.textclock("<span color='#ffffff' size='x-large'>%H</span>\n<span color='#ffffff' size='x-large'>%M</span>\n<span color='#ffffff' size='x-large'>%S</span>", 1)

customwidget.widget = wibox.container {
	wibox.widget.textclock("<span color='#ffffff'>%T</span>", 1),
	direction = 'east',
	widget = wibox.container.rotate
}

-- Tooltip
local widget_tooltip = awful.tooltip {
	objects = { customwidget.widget, customwidget.icon },
	timer_function = function()
		return globalos.date('%A %d %B %Y\n%T')
	end,
}

return customwidget
