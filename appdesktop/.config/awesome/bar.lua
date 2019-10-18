local wibox = require("wibox")
local awful = require("awful")
local screens = require("screens")
local beautiful = require("beautiful")

-- Customs widgets definitions
local widget_separator = require("widgets.separator")
local widget_separator_vertical = require("widgets.separator_vertical")
local widget_layout = require("widgets.layout")
local widget_tags = require("widgets.tags")
local widget_tags_vertical = require("widgets.tags_vertical")
local widget_tasks = require("widgets.tasks")
local widget_tasks_vertical = require("widgets.tasks_vertical")
local widget_clock = require("widgets.clock")
local widget_clock_vertical = require("widgets.clock_vertical")
local widget_volume = require("widgets.volume")
local widget_cpu = require("widgets.cpu")
local widget_ram = require("widgets.ram")
local widget_net = require("widgets.net")
local widget_vpn = require("widgets.vpn")
local widget_moc = require("widgets.moc")
local widget_systray = require("widgets.systray")
local widget_prompt = require("widgets.prompt")
local widget_keyboardlayout = require("widgets.keyboardlayout")
local widget_battery = require("widgets.battery")

local bar = {}

-- Bar (Wibox) management

function widget_rotate(w, inverse)
	local direction = 'east'
	if inverse then
		direction = 'west'
	end
		
	return wibox.container {
		w,
		direction = direction,
		widget = wibox.container.rotate
	}
end

-- Build a bar
function init(s)
	-- Create an imagebox widget which will contains an icon indicating which layout we're using. One layoutbox per screen.
	local layoutbox = widget_layout.widget(s)
	
	-- Widget for main screen only
	if s == screens.get_primary() then
		-- Create a promptbox (on screen object to trigger in keys bindings)
		s.promptbox = widget_prompt.widget

		if beautiful.bar_orientation == "vertical" then
			-- Create a vertical wibox
			local wibox_custom = awful.wibar({
				position = "left",
				screen = s,
			})

			-- Add widgets to the wibox
			wibox_custom:setup {
				layout = wibox.layout.align.vertical,
				{ -- Left widgets
					layout = wibox.layout.fixed.vertical,
					widget_tags_vertical.widget(s),
					widget_separator_vertical.widget,
					s.promptbox,
				},
				{ -- Middle widget
					layout = wibox.layout.fixed.vertical,
					widget_tasks_vertical.widget(s),
				},
				{ -- Right widgets
					layout = wibox.layout.fixed.vertical,
					widget_moc.icon,
					widget_rotate(widget_moc.widgetbar),
					widget_rotate(widget_moc.widget),
					widget_vpn.icon,
					widget_vpn.widget,
					widget_battery.icon,
					widget_rotate(widget_battery.widget),
					widget_separator_vertical.widget,
					widget_net.icon,
					widget_rotate(widget_net.widget, true),
					widget_separator_vertical.widget,
					widget_cpu.icon,
					widget_rotate(widget_cpu.widget, true),
					widget_separator_vertical.widget,
					widget_ram.icon,
					widget_rotate(widget_ram.widget),
					widget_separator_vertical.widget,
					widget_volume.icon,
					widget_rotate(widget_volume.widget),
					widget_separator_vertical.widget,
					-- widget_keyboardlayout.widget,
					widget_rotate(widget_systray.widget),
					widget_separator_vertical.widget,
					widget_clock.icon,
					widget_clock_vertical.widget,
					widget_separator_vertical.widget,
					{
						layoutbox,
						layout = wibox.container.margin(layoutbox ,0 ,0 ,0 ,-5)
					},
				},
			}
			
		else
			-- Create a horizontal wibox
			local wibox_custom = awful.wibar({
				position = "top",
				screen = s,
			})

			-- Add widgets to the wibox
			wibox_custom:setup {
				layout = wibox.layout.align.horizontal,
				{ -- Left widgets
					layout = wibox.layout.fixed.horizontal,
					widget_tags.widget(s),
					s.promptbox,
				},
				{ -- Middle widget
					layout = wibox.layout.fixed.horizontal,
					widget_tasks.widget(s),
				},
				{ -- Right widgets
					layout = wibox.layout.fixed.horizontal,
					-- layout = awful.widget.only_on_screen,
					-- screen = "primary", -- Only display on primary screen
					widget_moc.icon,
					widget_moc.widgetbar,
					widget_moc.widget,
					widget_separator.widget,
					widget_vpn.icon,
					widget_vpn.widget,
					widget_net.icon,
					widget_net.widget,
					widget_cpu.icon,
					widget_cpu.widget,
					widget_ram.icon,
					widget_ram.widget,
					widget_battery.icon,
					widget_battery.widget,
					widget_volume.icon,
					widget_volume.widget,
					widget_separator.widget,
					widget_keyboardlayout.widget,
					widget_systray.widget,
					widget_separator.widget,
					widget_separator.widget,
					widget_clock.icon,
					widget_separator.widget,
					widget_clock.widget,
					widget_separator.widget,
					layoutbox,
				},
			}
		end
	else
		-- secondary screen (always horizontal)
		-- TODO: improve like this : https://github.com/awesomeWM/awesome/blob/dd5be865c3d00c580389c38ea41b6719ab567d3e/tests/_wibox_helper.lua
		local wibox_custom = awful.wibar({
			position = "top",
			screen = s,
			--height = 25
		})
		wibox_custom:setup {
			layout = wibox.layout.align.horizontal,
			{ -- Left widgets
				layout = wibox.layout.fixed.horizontal,
				widget_tags.widget(s),
			},
			{ -- Middle widget
				layout = wibox.layout.fixed.horizontal,
				tasklist,
			},
			{ -- Right widgets
				layout = wibox.layout.fixed.horizontal,
				widget_separator.widget,
				layoutbox,
			},
		}
	end

	return wibox_custom
end


bar.init = init

return bar
