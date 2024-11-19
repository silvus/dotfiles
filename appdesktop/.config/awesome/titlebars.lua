local awful = require("awful")
local wibox = require("wibox")


local titlebars = {}


function setup_titlebar(c)
	-- buttons for the titlebar
	local buttons = awful.util.table.join(
		awful.button({}, 1, function()
			client.focus = c
			c:raise()
			awful.mouse.client.move(c)
		end),
		awful.button({}, 3, function()
			client.focus = c
			c:raise()
			awful.mouse.client.resize(c)
		end)
	)

	local titlebar = awful.titlebar(c, {
		size = 18,
	})

	return titlebar:setup {
		{ -- Left
			{
				awful.titlebar.widget.iconwidget(c),
				buttons = buttons,
				layout  = wibox.layout.fixed.horizontal
			},
			top = 3,
			bottom = 3,
			left = 3,
			right = 6,
			widget = wibox.container.margin,
		},
		{ -- Middle
			{ -- Title
				align  = "left",
				widget = awful.titlebar.widget.titlewidget(c)
			},
			buttons = buttons,
			layout  = wibox.layout.flex.horizontal
		},
		{ -- Right
			awful.titlebar.widget.floatingbutton(c),
			awful.titlebar.widget.stickybutton(c),
			awful.titlebar.widget.ontopbutton(c),
			awful.titlebar.widget.maximizedbutton(c),
			{
				{
					awful.titlebar.widget.minimizebutton(c),
					layout = wibox.layout.fixed.horizontal
				},
				left = 20,
				widget = wibox.container.margin,
			},
			awful.titlebar.widget.closebutton(c),

			layout = wibox.layout.fixed.horizontal
		},
		layout = wibox.layout.align.horizontal
	}
end

titlebars.setup_titlebar = setup_titlebar

return titlebars

