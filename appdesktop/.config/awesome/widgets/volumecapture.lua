local string = require("string")
local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")
local lain = require("lain")

local customwidget = {}

local volume_value = 0

customwidget.icon = wibox.widget.imagebox(beautiful.volcapture)

-- ALSA volume capture bar

customwidget.volume = lain.widget.alsa({
	channel = 'Capture',
	timeout = 2,
	settings = function()
		customwidget.widgetbar.visible = true
		local level = tonumber(volume_now.level)
		-- customwidget.widgetbar:set_value(level)
		volume_value = level
		customwidget.widgetbar.border_color = beautiful.info
		if volume_now.status == "off" then
			customwidget.icon:set_image(beautiful.volcapture_mute)
		elseif level and level == 0 then
			customwidget.icon:set_image(beautiful.volcapture_no)
		elseif level and level <= 20 then
			customwidget.icon:set_image(beautiful.volcapture_low)
		else
			customwidget.icon:set_image(beautiful.volcapture)
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
	-- start_angle = 3*math.pi/2,
	-- thickness = 2,
	forced_width = 18,
	forced_height = 18,
	-- widget = wibox.container.arcchart,
	widget = wibox.container.background,
}

-- Stack icon + bar
customwidget.widget = wibox.widget {
	wibox.container.margin(customwidget.widgetbar, 0, 1, 0, 1),
	customwidget.icon,
	layout  = wibox.layout.stack
}

-- Tooltip
local widget_tooltip = awful.tooltip {
	objects        = { customwidget.widget},
	timer_function = function()
		return string.format("%d%%", volume_value)
	end,
}

-- events
buttons_event = awful.util.table.join (
	awful.button({}, 1, function()
		-- awful.spawn.with_shell(string.format("%s -e alsamixer", terminal))
		awful.spawn.with_shell('pavucontrol')
	end),
	awful.button({}, 2, function()
		awful.spawn(string.format("%s -D pulse set %s 100%%", customwidget.volume.cmd, customwidget.volume.channel))
		customwidget.volume.update()
	end),
	awful.button({}, 3, function()
		awful.spawn(string.format("%s -D pulse set %s toggle", customwidget.volume.cmd, customwidget.volume.togglechannel or customwidget.volume.channel))
		customwidget.volume.update()
	end),
	awful.button({}, 4, function()
		awful.spawn(string.format("%s -D pulse set %s 5%%+", customwidget.volume.cmd, customwidget.volume.channel))
		customwidget.volume.update()
	end),
	awful.button({}, 5, function()
		awful.spawn(string.format("%s -D pulse set %s 5%%-", customwidget.volume.cmd, customwidget.volume.channel))
		customwidget.volume.update()
	end)
)
-- customwidget.icon:buttons(buttons_event)
customwidget.widget:buttons(buttons_event)

return customwidget
