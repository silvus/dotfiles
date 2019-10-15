local awful = require("awful")

local customwidget = {}

customwidget.icon = nil

-- Prompt box

customwidget.widget = awful.widget.prompt()

return customwidget
