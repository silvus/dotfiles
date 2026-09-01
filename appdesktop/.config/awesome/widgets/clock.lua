local beautiful = require("beautiful")
local wibox = require("wibox")
local awful = require("awful")
local globalos = require("os")

local customwidget = {}

-- Textclock widget

local icon = wibox.widget {
	{
		image  = beautiful.clock,
		resize = true,
		widget = wibox.widget.imagebox,
	},
	strategy      = "exact",
	forced_width  = 14,
	forced_height = 14,
	widget        = wibox.container.constraint,
}
customwidget.icon = wibox.container.margin(
	wibox.container.place(icon, "center", "center"),
	0, 0, 0, 6
)

customwidget.widget = wibox.widget.textclock("<span color='#ffffff'>%T</span>", 1)

-- Tooltip
awful.tooltip {
	objects        = { customwidget.widget, customwidget.icon },
	timer_function = function()
		return globalos.date('%A %d %B %Y\n%T')
	end,
}

return customwidget
