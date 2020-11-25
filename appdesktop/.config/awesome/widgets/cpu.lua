local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")
local lain = require("lain")

local customwidget = {}

customwidget.icon = wibox.widget.imagebox(beautiful.cpu)

-- Cpu bar

local cpubar = wibox.widget {
	forced_height 	= 1,
	forced_width 	= 100,
	margins 		= 1,
	paddings 		= 1,
	ticks 			= true,
	ticks_size 		= 10,
	step_width 		= 3,
	max_value 		= 100,
	min_value 		= 0,
	value 			= 0,
	color 			= beautiful.success,
	background_color = beautiful.bg_normal,
	border_color 	= beautiful.info,
	-- widget		   = wibox.widget.progressbar
	widget 			= wibox.widget.graph
}
local cpu = lain.widget.cpu({
	width = 100, border_width = 0, ticks = true, ticks_size = 10,
	settings = function()
		-- cpubar:set_value(cpu_now.usage)
		cpubar:add_value(cpu_now.usage)
		-- cpubar:set_value(cpu_now.used)
	end
})
local cpubg = wibox.container.background(cpubar, beautiful.info, gears.shape.rectangle)

customwidget.widget = wibox.container.margin(cpubg, 2, 4, 4, 4)

return customwidget
