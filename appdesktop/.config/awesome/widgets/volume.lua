local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")
local lain = require("lain")

local customwidget = {}

customwidget.icon = wibox.widget.imagebox(beautiful.vol)

-- ALSA volume bar

customwidget.volume = lain.widget.alsa({
	timeout = 2,
	settings = function()
		local level = tonumber(volume_now.level)
		customwidget.widgetbar:set_value(level)
		if volume_now.status == "off" then
			customwidget.icon:set_image(beautiful.vol_mute)
			-- TODO: How to trigger widget update ?
			customwidget.widgetbar.colors[1] = beautiful.error
			customwidget.widgetbar.border_color = beautiful.error
		elseif level == 0 then
			customwidget.icon:set_image(beautiful.vol_no)
			customwidget.widgetbar.colors[1] = beautiful.error
			customwidget.widgetbar.border_color = beautiful.error
		elseif level <= 50 then
			customwidget.icon:set_image(beautiful.vol_low)
		else
			customwidget.icon:set_image(beautiful.vol)
		end
	end,
})

-- Widget bar
customwidget.widgetbar = wibox.widget {
	colors = {
		beautiful.success,
		-- beautiful.bg_normal,
		-- beautiful.bg_highlight,
		-- beautiful.border_color,
	},
	value = 0,
	max_value = 100,
	min_value = 0,
	rounded_edge = false,
	background_color = beautiful.info,
	bg = beautiful.info,
	border_width = 1,
	border_color = beautiful.border_focus,
	paddings     = {
		left   = 6,
		right  = 6,
		top    = 6,
		bottom = 6,
	},
	start_angle = 3*math.pi/2,
	thickness = 2,
	forced_width = 18,
	forced_height = 18,
	widget = wibox.container.arcchart,
}

-- Stack icon + bar
customwidget.widget = wibox.widget {
	customwidget.widgetbar,
	customwidget.icon,
	layout  = wibox.layout.stack
}

-- events
buttons_event = awful.util.table.join (
	awful.button({}, 1, function()
		awful.spawn.with_shell(string.format("%s -e alsamixer", terminal))
	end),
	awful.button({}, 2, function()
		awful.spawn(string.format("%s set %s 100%%", customwidget.volume.cmd, customwidget.volume.channel))
		customwidget.volume.update()
	end),
	awful.button({}, 3, function()
		awful.spawn(string.format("%s set %s toggle", customwidget.volume.cmd, customwidget.volume.togglechannel or customwidget.volume.channel))
		customwidget.volume.update()
	end),
	awful.button({}, 4, function()
		awful.spawn(string.format("%s set %s 5%%+", customwidget.volume.cmd, customwidget.volume.channel))
		customwidget.volume.update()
	end),
	awful.button({}, 5, function()
		awful.spawn(string.format("%s set %s 5%%-", customwidget.volume.cmd, customwidget.volume.channel))
		customwidget.volume.update()
	end)
)
-- customwidget.icon:buttons(buttons_event)
customwidget.widget:buttons(buttons_event)

return customwidget
