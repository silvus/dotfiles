local wibox = require("wibox")
local awful = require("awful")
local widget_common = require("awful.widget.common")

local widget_separator_vertical = require("widgets.separator_vertical")

local customwidget = {}

-- Apps list

customwidget.buttons = awful.util.table.join(
	awful.button({}, 1, function(c)
		if c == client.focus then
			c.minimized = true
		else
			-- Without this, the following :isvisible() makes no sense
			c.minimized = false
			if not c:isvisible() and c.first_tag then
				c.first_tag:view_only()
			end
			-- This will also un-minimize the client, if needed
			client.focus = c
			c:raise()
		end
	end)
)

-- Create a tasklist widget
function widget(s)
	return awful.widget.tasklist({
		screen          = s,
		-- For minimized clients only
		-- filter   = awful.widget.tasklist.filter.minimizedcurrenttags,
		filter          = awful.widget.tasklist.filter.currenttags,
		buttons         = customwidget.buttons,
		-- style    = {
		-- 	shape_border_width = 1,
		-- 	shape_border_color = '#777777',
		-- 	shape  = gears.shape.rounded_bar,
		-- },
		layout          = {
			spacing = 1,
			-- spacing_widget = widget_separator_vertical.widget,
			layout  = wibox.layout.fixed.vertical,
			-- layout  = wibox.layout.flex.vertical
		},
		widget_template = {
			{
				{
					{
						id     = "icon_role",
						widget = wibox.widget.imagebox,
						resize = true,
					},
					{
						{
							id     = "text_role", -- Used fo status indicators (e.g., urgent, minimized)
							widget = wibox.widget.textbox,
						},
						halign = "center",
						valign = "center",
						widget = wibox.container.place, -- Text align
					},
					layout = wibox.layout.fixed.vertical, -- Stack icon above status
				},
				margins = 5,
				widget  = wibox.container.margin,
			},
			id      = "background_role",
			widget  = wibox.container.background,
		}
	})
end

customwidget.widget = widget

return customwidget
