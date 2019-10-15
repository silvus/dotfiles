local wibox = require("wibox")
local awful = require("awful")

local customwidget = {}

customwidget.icon = nil

-- Keyboard Layout

customwidget.widget = awful.widget.keyboardlayout()

return customwidget
