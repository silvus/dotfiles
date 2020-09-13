local beautiful = require("beautiful")
local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")
local lain = require("lain")
local naughty = require("naughty")
local math = require("math")

local customwidget = {}

customwidget.icon = wibox.widget.imagebox(beautiful.note)
customwidget.icon.visible = false

-- Moc

-- First widget, a bar
customwidget.widgetbar = wibox.widget {
	visible = false,
	colors = {
		beautiful.success,
		-- beautiful.bg_normal,
		-- beautiful.bg_highlight,
		-- beautiful.border_color,
	},
	value = 0,
	max_value    = 100,
	min_value    = 0,
	rounded_edge = false,
	background_color = beautiful.error,
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

-- second widget, current song and update bar and icon
local moc = lain.widget.contrib.moc({
	music_dir = "/data/media/music",
	cover_size = 50,
	default_art = beautiful.music,
	settings  = function()
		moc_notification_preset = naughty.config.presets.low
		moc_notification_preset.title = moc_now.artist .. " - " .. moc_now.title
		moc_notification_preset.text    = string.format("%s (%s) - %s", moc_now.artist, moc_now.album, moc_now.title)

		if moc_now.state == 'PLAY' or moc_now.state == 'PAUSE' then
			customwidget.icon.visible = true

			if moc_now.total == 'N/A' then
				-- Remote m3a (Like Rainwave)
				if moc_now.title == nil or moc_now.title == '' then
					widget:set_markup("<span color='#ffffff'>" .. moc_now.state .. "</span>")
					customwidget.customtooltip:set_markup("<span color='#ffffff'>" .. moc_now.state .. "</span>")
				else
					widget:set_markup("<span color='#ffffff'>" .. moc_now.title .. "</span>")
					customwidget.customtooltip:set_markup("<span color='#ffffff'>" .. moc_now.title .. "</span>")
				end
			else
				-- Local file
				widget:set_markup("<span color='#ffffff'>" .. string.sub(moc_now.file:match( "([^/]+)$" ), 0 , 30) .. "</span>")
				-- widget:set_markup("<span color='#ffffff'>" .. string.sub(moc_now.file:match( "([^/]+)$" ), 0 , 30) .. ' | ' .. moc_now.elapsed .. ' / ' .. moc_now.total .. "</span>")
				customwidget.customtooltip:set_markup("<span color='#ffffff'>" .. string.sub(moc_now.file:match( "([^/]+)$" ), 0 , 30) .. "</span>")

				local time_pattern = "(%d+):(%d+)"
				local totalminute, totalseconds = moc_now.total:match(time_pattern)

				local total_time = (totalminute * 60) + totalseconds
				local nowminute, nowseconds = moc_now.elapsed:match(time_pattern)
				local now_time = (nowminute * 60) + nowseconds

				-- Build current song progress bar
				if total_time > 0 then
					customwidget.widgetbar.visible = true
					if now_time > 0 then
						customwidget.widgetbar:set_value(now_time * 100 / total_time)
					else
						customwidget.widgetbar:set_value(0)
					end
				end
			end
		else
			-- No music, hide bar and icon
			widget:set_markup("")
			customwidget.customtooltip:set_markup("")
			customwidget.icon.visible = false
			customwidget.widgetbar.visible = false
			-- mymocbar:set_value(0)
		end
	end,
})
local mocbg = wibox.container.background(moc.widget, beautiful.bg_normal, gears.shape.rectangle)
customwidget.widget = wibox.container.margin(mocbg, 2, 7, 4, 4)

-- Events
local events_actions = awful.util.table.join (
	awful.button({}, 1, function()
		awful.util.spawn("music --toggle-pause", false)
	end),
	awful.button({}, 2, function()
		awful.util.spawn("music --previous", false)
	end),
	awful.button({}, 3, function()
		awful.util.spawn("music --next", false)
	end)
)
customwidget.icon:buttons(events_actions)
customwidget.widgetbar:buttons(events_actions)
customwidget.widget:buttons(events_actions)

-- Tooltip
customwidget.customtooltip = awful.tooltip {
	objects = { customwidget.icon, customwidget.widgetbar, customwidget.widget},
	text = '',
}


return customwidget
