local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")
local lain = require("lain")

local customwidget = {}

customwidget.icon = wibox.widget.imagebox(beautiful.vol)

-- ALSA volume bar

customwidget.volume = lain.widget.alsabar({
	width = 100,
	border_width = 0,
	ticks = false,
	ticks_size = 10,
	timeout = 2,
	notification_preset = { font = beautiful.font },
	--togglechannel = "IEC958,3",
	settings = function()
		if volume_now.status == "off" then
			customwidget.icon:set_image(beautiful.vol_mute)
		elseif volume_now.level == 0 then
			customwidget.icon:set_image(beautiful.vol_no)
		elseif volume_now.level <= 50 then
			customwidget.icon:set_image(beautiful.vol_low)
		else
			customwidget.icon:set_image(beautiful.vol)
		end
	end,
	colors = {
		background	= beautiful.bg_normal,
		mute 		= beautiful.error,
		unmute		= beautiful.fg_normal
	}
})
customwidget.volume.tooltip.wibox.fg = beautiful.fg_focus

local volumebg = wibox.container.background(customwidget.volume.bar, beautiful.info, gears.shape.rectangle)

customwidget.widget = wibox.container.margin(volumebg, 2, 7, 4, 4)

-- events
customwidget.widget:buttons(awful.util.table.join (
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
))


return customwidget
