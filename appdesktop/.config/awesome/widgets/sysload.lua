local lain = require("lain")
local beautiful = require("beautiful")
local wibox = require("wibox")

local customwidget = {}

customwidget.sysload_text = wibox.widget.textbox()

customwidget.icon = wibox.widget.imagebox(beautiful.hdd)

-- Sysload

-- Get color based on value
local function get_color(val)
	if tonumber(val) < 1 then
		return beautiful.primary
	elseif tonumber(val) < 2 then
		return beautiful.success
	else
		return beautiful.error
	end
end

lain.widget.sysload {
	settings = function()
		customwidget.sysload_text:set_markup("<span font='DejaVu Sans 8' color='" .. get_color(load_1) .. "'>" .. load_1 .. "</span> " ..
		"<span font='DejaVu Sans 8' color='" .. get_color(load_5) .. "'>" .. load_5 .. "</span> " ..
		"<span font='DejaVu Sans 8' color='" .. get_color(load_15) .. "'>" .. load_15 .. "</span> ")
	end
}

customwidget.widget = wibox.container.margin(customwidget.sysload_text, 1, 1, 1, 1)

return customwidget
