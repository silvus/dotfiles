local beautiful = require("beautiful")
local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")
local lain = require("lain")
local naughty = require("naughty")

local customwidget = {}

customwidget.icon = wibox.widget.imagebox(beautiful.music)
customwidget.icon.visible = false

-- Moc

-- First widget, a bar
local mymocbar = wibox.widget {
	forced_height	= 1,
	forced_width	= 100,
	margins 		= 1,
	paddings		= 1,
	ticks			= false,
	ticks_size		= 10,
	step_width		= 5,
	max_value		= 100,
	min_value		= 0,
	value			= 0,
	color 			= beautiful.success,
	background_color = beautiful.bg_normal,
	border_color	= beautiful.info,
	widget			= wibox.widget.progressbar
}
local mymocbarbg = wibox.container.background(mymocbar, beautiful.info, gears.shape.rectangle)
customwidget.widgetbar = wibox.container.margin(mymocbarbg, 2, 7, 4, 4)
customwidget.widgetbar.visible = false

-- second widget, current song and update bar and icon
local moc = lain.widget.contrib.moc({
	music_dir = "/data/media/music",
	cover_size = 0,
	settings  = function()
		moc_notification_preset = {
			title   = moc_now.artist .. " - " .. moc_now.title,
			timeout = 10,
			text    = string.format("%s (%s) - %s", moc_now.artist, moc_now.album, moc_now.title),
			preset  = naughty.config.defaults
		}
		if moc_now.state == 'PLAY' or moc_now.state == 'PAUSE' then
			customwidget.icon.visible = true

			if moc_now.total == 'N/A' then
				-- Remote m3a (Like Rainwave)
				if moc_now.title == nil or moc_now.title == '' then
					widget:set_markup("<span color='#ffffff'>" .. moc_now.state .. "</span>")
				else
					widget:set_markup("<span color='#ffffff'>" .. moc_now.title .. "</span>")
				end
			else
				-- Local file
				widget:set_markup("<span color='#ffffff'>" .. string.sub(moc_now.file:match( "([^/]+)$" ), 0 , 30) .. "</span>")
				-- widget:set_markup("<span color='#ffffff'>" .. string.sub(moc_now.file:match( "([^/]+)$" ), 0 , 30) .. ' | ' .. moc_now.elapsed .. ' / ' .. moc_now.total .. "</span>")

				local time_pattern = "(%d+):(%d+)"
				local totalminute, totalseconds = moc_now.total:match(time_pattern)

				local total_time = (totalminute * 60) + totalseconds
				local nowminute, nowseconds = moc_now.elapsed:match(time_pattern)
				local now_time = (nowminute * 60) + nowseconds

				-- Build current song progress bar
				if total_time > 0 then
					customwidget.widgetbar.visible = true
					if now_time > 0 then
						mymocbar:set_value(now_time * 100 / total_time)
					else
						mymocbar:set_value(0)
					end
				end
			end
		else
			-- No music, hide bar and icon
			widget:set_markup("")
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

return customwidget
