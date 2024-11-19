local wibox = require("wibox")

local customwidget = {}

-- Systray

customwidget.widget = wibox.container.margin(wibox.widget.systray(), 1, 1, 2, 2)

return customwidget

