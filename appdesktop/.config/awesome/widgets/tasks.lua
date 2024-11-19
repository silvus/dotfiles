local wibox = require("wibox")
local awful = require("awful")
local widget_common = require("awful.widget.common")

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
-- awful.button({ }, 3, client_menu_toggle_fn())
-- awful.button({ }, 4, function ()
--						  awful.client.focus.byidx(1)
--					  end),
-- awful.button({ }, 5, function ()
--						  awful.client.focus.byidx(-1)
--					  end))
)

-- Build tasks widget list
function widget(s)
	-- Create a tasklist widget
	-- return awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, customwidget.buttons)

	-- Create a tasklist widget with a max width
	-- return awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags , customwidget.buttons, nil, function(w, buttons, label, data, objects)
	-- 	widget_common.list_update(w, buttons, label, data, objects)
	-- 	w:set_max_widget_size(300)
	-- end, wibox.layout.flex.horizontal())

	-- Create a tasklist widget for minimized clients only
	-- return awful.widget.tasklist(s, awful.widget.tasklist.filter.minimizedcurrenttags, customwidget.buttons)

	-- Create a tasklist widget for minimized clients only with a max width
	return awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, customwidget.buttons, nil,
		function(w, buttons, label, data, objects)
			widget_common.list_update(w, buttons, label, data, objects)
			w:set_max_widget_size(300)
		end, wibox.layout.flex.horizontal())
end

customwidget.widget = widget

return customwidget

