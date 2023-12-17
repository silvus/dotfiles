local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")
local lain = require("lain")

-- Sysload graph

local customwidget = {}
local sysload_value = 0

customwidget.icon = wibox.widget.imagebox(beautiful.cpu)

-- Get color based on value
local function rgba_to_hex(r, g, b, a)
	local h = "#"
	return h..string.format("%02x%02x%02x",
		math.floor(r),
		math.floor(g),
		math.floor(b))
		--this part only shows the alpha channel if it's not 1
		..(a ~= 1 and string.format("%02x", math.floor(a*255)) or "")
end

local function get_color(val)
	local col1_hex = beautiful.primary:gsub("#","")
	local col1_r = tonumber("0x"..col1_hex:sub(1,2))
	local col1_g = tonumber("0x"..col1_hex:sub(3,4))
	local col1_b = tonumber("0x"..col1_hex:sub(5,6))

	local col2_hex = beautiful.error:gsub("#","")
	local col2_r = tonumber("0x"..col2_hex:sub(1,2))
	local col2_g = tonumber("0x"..col2_hex:sub(3,4))
	local col2_b = tonumber("0x"..col2_hex:sub(5,6))

	r = math.min(math.max(col1_r + val * (col2_r - col1_r), 0), 255)
	g = math.min(math.max(col1_g + val * (col2_g - col1_g), 0), 255)
	b = math.min(math.max(col1_b + val * (col2_b - col1_b), 0), 255)
	-- a = math.min(math.max(col1.a + val * (col2.a - col1.a), 0), 1)

	return rgba_to_hex(r, g, b, 1)

	-- if val < 1 then
	-- 	return beautiful.primary
	-- elseif val < 2 then
	-- 	return beautiful.success
	-- else
	-- 	return beautiful.error
	-- end
end

local sysloadbar = wibox.widget {
	forced_height 	= beautiful.graph_height or 1,
	forced_width 	= beautiful.graph_width or 75,
	margins 		= 1,
	paddings 		= 1,
	ticks 			= true,
	ticks_size 		= 10,
	step_width 		= 3,
	step_spacing 	= 1,
	max_value 		= 3,
	min_value 		= 0,
	value 			= 0,
	color 			= beautiful.success,
	background_color = beautiful.bg_normal,
	border_color 	= beautiful.info,
	widget 			= wibox.widget.graph
}
local sysload = lain.widget.sysload({
	width = 100, border_width = 0, ticks = true, ticks_size = 10,
	settings = function()
		sysload_value = tonumber(load_1)
		sysloadbar:add_value(sysload_value)
		-- Remove 1 to have a more natural color (load generaly between 1 and 2)
		sysloadbar.color = get_color(sysload_value - 1)
	end
})
local sysloadbg = wibox.container.background(sysloadbar, beautiful.info, gears.shape.rectangle)

customwidget.widget = wibox.container.margin(sysloadbg, 2, 4, 4, 4)

-- Tooltip
local widget_tooltip = awful.tooltip {
	objects        = { customwidget.widget},
	timer_function = function()
		return string.format("%.2f", sysload_value)
	end,
}

return customwidget
