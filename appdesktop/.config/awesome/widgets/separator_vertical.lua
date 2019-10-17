local wibox = require("wibox")
local beautiful = require("beautiful")

local customwidget = {}

-- Separator vertical

customwidget.widget = wibox.widget {
	orientation = 'horizontal',
	border_width = 1,
	color = beautiful.titlebar_fg_normal,
	border_color = beautiful.titlebar_fg_normal,
	forced_height = 5,
	forced_width = nil,
	widget = wibox.widget.separator,
}

return customwidget
