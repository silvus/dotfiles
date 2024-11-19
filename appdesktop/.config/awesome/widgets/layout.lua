local wibox = require("wibox")
local awful = require("awful")

local customwidget = {}

-- Layout box

function widget(s)
	local w = awful.widget.layoutbox(s)

	-- Buttons
	w:buttons(awful.util.table.join(
		awful.button({}, 1, function() awful.layout.inc(1) end),
		awful.button({}, 3, function() awful.layout.inc(-1) end),
		awful.button({}, 4, function() awful.layout.inc(1) end),
		awful.button({}, 5, function() awful.layout.inc(-1) end))
	)

	return w
end

customwidget.widget = widget

return customwidget

