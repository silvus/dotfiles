local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")
local lain = require("lain")

local customwidget = {}

customwidget.icon = wibox.widget.imagebox(beautiful.net)

-- Net bar

local netbar = wibox.widget {
	forced_height 	= beautiful.graph_height or 1,
	forced_width 	= beautiful.graph_width or 75,
	margins 		= 1,
	paddings 		= 1,
	ticks 			= true,
	ticks_size 		= 10,
	step_width 		= 3,
	max_value 		= 800,
	value 			= 0,
	color 			= beautiful.success,
	background_color = beautiful.bg_normal,
	border_color 	= beautiful.info,
	-- widget 			= wibox.widget.progressbar
	widget 			= wibox.widget.graph,
}
local net = lain.widget.net({
	-- width = 100, border_width = 0, ticks = true, ticks_size = 100,
	settings = function()
		-- netbar:set_value(net_now.received)
		netbar:add_value(tonumber(net_now.received))
	end
})
local netbg = wibox.container.background(netbar, beautiful.info, gears.shape.rectangle)

customwidget.widget = wibox.container.margin(netbg, 2, 4, 4, 4)

return customwidget
