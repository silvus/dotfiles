local wibox = require("wibox")
local awful = require("awful")
local screens = require('screens')
local config = require('config')


local customwidget = {}

-- Tags list

-- buttons
customwidget.buttons = awful.util.table.join(
	awful.button({ }, 1, function(t)
		t:view_only()
	end),
	awful.button({ config.modkey }, 1, function(t)
		if client.focus then
			client.focus:move_to_tag(t)
		end
	end),
	awful.button({ }, 3, function(t)
		awful.tag.viewtoggle(t)
	end),
	awful.button({ config.modkey }, 3, function(t)
		if client.focus then
			client.focus:toggle_tag(t)
		end
	end),
	awful.button({ }, 4, function(t)
		awful.tag.viewnext(t.screen)
	end),
	awful.button({ }, 5, function(t)
		awful.tag.viewprev(t.screen)
	end)
)

-- Build tag widget list 
-- https://awesomewm.org/doc/api/classes/awful.widget.taglist.html
function widget(s)
	if s == screens.get_primary() then
		return awful.widget.taglist({
			screen  = s,
			filter  = customwidget.tags_filter,
			buttons = customwidget.buttons,
			layout   = {
				layout  = wibox.layout.fixed.vertical
			},
			widget_template = {
				{
					{
						{
							{
								id = 'icon_role',
								widget = wibox.widget.imagebox,
								resize = true,
								forced_height = 25,
								forced_width = 25,
							},
							layout = wibox.container.margin,
							top = 2,
							right = 2,
							bottom= 2,
							left = 4,
						},
						{
							{
								{
									id = 'text_role',
									widget = wibox.widget.textbox,
									valign = 'center',
									align = 'center',
								},
								bg = '#000000',
								fg = '#ffffff',
								-- forced_height = 12,
								-- forced_width = 8,
								opacity = 0.6,
								widget = wibox.container.background,
							},
							widget = wibox.container.place,
							valign = 'bottom',
							halign = 'left',
						},
						layout = wibox.layout.stack,
					},
					-- left  = 1,
					-- right = 1,
					widget = wibox.container.margin
				},
				id     = 'background_role',
				widget = wibox.container.background,
				forced_height = 25,
				forced_width = 25,
			},
		})
	else
		-- secondary screens
		return awful.widget.taglist(s, customwidget.tags_filter, customwidget.buttons)
	end
end


-- Filter for tags widgets
function tags_filter(t)
	-- No empty and not the scratchpad (except if selected)
	return (#t:clients() > 0 or t.selected) and (t.name ~= "0" or t.selected)
end


customwidget.widget = widget
customwidget.tags_filter = tags_filter

return customwidget
