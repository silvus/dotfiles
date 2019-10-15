local beautiful = require("beautiful")
local wibox = require("wibox")
local calendar = require("calendar")

local customwidget = {}

customwidget.icon = wibox.widget.imagebox(beautiful.clock)

-- Textclock widget with calendar

customwidget.widget = wibox.widget.textclock("%a %d %b  <span color='#ffffff'>%H:%M:%S</span>", 1)

-- attach it as popup to your text clock widget:
calendar({}):attach(customwidget.widget)
calendar({}):attach(customwidget.icon)

return customwidget






