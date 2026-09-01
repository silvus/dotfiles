local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")
local lain = require("lain")

local customwidget = {}
local ram_value = 0

customwidget.icon = wibox.widget.imagebox(beautiful.mem)

-- Ram graph

local membar = wibox.widget {
	forced_height    = beautiful.graph_height or 1,
	forced_width     = beautiful.graph_width or 75,
	margins          = 1,
	paddings         = 1,
	ticks            = true,
	ticks_size       = 10,
	step_width       = 3,
	max_value        = 100,
	min_value        = 0,
	value            = 0,
	color            = beautiful.success,
	background_color = beautiful.bg_normal,
	border_color     = beautiful.info,
	widget           = wibox.widget.graph
}
lain.widget.mem({
	settings = function()
		membar:add_value(mem_now.perc)
		ram_value = mem_now.perc
	end
})
local membg = wibox.container.background(membar, beautiful.info, gears.shape.rectangle)

customwidget.widget = wibox.container.margin(membg, 2, 4, 4, 4)

-- Tooltip
local widget_tooltip = awful.tooltip {
	objects        = { customwidget.widget },
	timer_function = function()
		return string.format("%d%%", ram_value)
	end,
}

return customwidget
