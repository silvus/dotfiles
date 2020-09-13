-- Build full screen dashboard
-- https://github.com/elenapan/dotfiles/blob/master/config/awesome/elemental/dashboard/amarena.lua
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi
local escape_f = require("awful.util").escape

local keygrabber = require("awful.keygrabber")

local config = require("config")

-- Theme handling library
local beautiful = require("beautiful")

local calendar = require("utils.calendar")

local widgets_background = '#00060f'
-- local widgets_foreground = '#eeeeec'

local box_radius = beautiful.dashboard_box_border_radius or dpi(12)
local box_gap = dpi(12)

-- Create the widget
local dashboard = wibox({visible = false, ontop = true, type = "dock", screen = screen.primary})
awful.placement.maximize(dashboard)

dashboard.bg = beautiful.dashboard_bg or (beautiful.bg_normal .. "ED") or "#111111"
dashboard.fg = beautiful.dashboard_fg or "#FEFEFE"

-- Adds a maximized mask to a screen
local function screen_mask(s)
	local mask = wibox({
		visible = false,
		ontop = true,
		type = "splash",
		screen = s,
	})
	awful.placement.maximize(mask)
	mask.bg = "#11111190"
	return mask
end

-- Add dashboard or mask to each screen
for s in screen do
	if s == screen.primary then
		s.dashboard = dashboard
	else
		s.dashboard = screen_mask(s)
	end
end

local function set_visibility(v)
	for s in screen do
		if s.dashboard then
			s.dashboard.visible = v
		end
	end
end

dashboard:buttons(gears.table.join(
	-- Middle click - Hide dashboard
	awful.button({ }, 2, function ()
		dashboard_hide()
	end)
))

-- Create rounded rectangle shape
local function rrect(radius)
	return function(cr, width, height)
		gears.shape.rounded_rect(cr, width, height, radius)
	end
end

local function prrect(radius, tl, tr, br, bl)
	return function(cr, width, height)
		gears.shape.partially_rounded_rect(cr, width, height, tl, tr, br, bl, radius)
	end
end

local function firstToUpper(str)
	return (str:gsub("^%l", string.upper))
end

-- Helper function that puts a widget inside a box with a specified background color
-- Invisible margins are added so that the boxes created with this function are evenly separated
-- The widget_to_be_boxed is vertically and horizontally centered inside the box
local function create_boxed_widget(widget_to_be_boxed, width, height, bg_color)
	local box_container = wibox.container.background()
	box_container.bg = bg_color
	box_container.forced_height = height
	box_container.forced_width = width
	box_container.shape = rrect(box_radius)

	local boxed_widget = wibox.widget {
		-- Add margins
		{
			-- Add background color
			{
				-- Center widget_to_be_boxed horizontally
				nil,
				{
					-- Center widget_to_be_boxed vertically
					nil,
					-- The actual widget goes here
					widget_to_be_boxed,
					layout = wibox.layout.align.vertical,
					expand = "none"
				},
				layout = wibox.layout.align.horizontal,
				expand = "none"
			},
			widget = box_container,
		},
		margins = box_gap,
		color = "#FF000000",
		widget = wibox.container.margin
	}

	return boxed_widget
end

-- Fortune
local fortune_command = "fortune -n 140 -s"
local fortune_update_interval = 3600
-- local fortune_command = "fortune -n 140 -s computers"
local fortune = wibox.widget {
	-- font = "sans medium 11",
	text = "Loading your cookie...",
	widget = wibox.widget.textbox
}

local update_fortune = function()
	awful.spawn.easy_async_with_shell(fortune_command, function(out)
		-- Remove trailing whitespaces
		out = escape_f(out:gsub('^%s*(.-)%s*$', '%1'))
		fortune.markup = "<i>"..out.."</i>"
	end)
end

gears.timer {
	autostart = true,
	timeout = fortune_update_interval,
	single_shot = false,
	call_now = true,
	callback = update_fortune
}

local fortune_widget = wibox.widget {
	{
		nil,
		fortune,
		layout = wibox.layout.align.horizontal,
	},
	margins = box_gap * 4,
	color = "#00000000",
	widget = wibox.container.margin
}

local fortune_box = create_boxed_widget(fortune_widget, dpi(300), dpi(140), widgets_background)
fortune_box:buttons(gears.table.join(
	-- Left click - New fortune
	awful.button({}, 1, update_fortune)
))
-- helpers.add_hover_cursor(fortune_box, "hand1")

-- Calendar
local calendar = require("widgets.calendar")
-- Update calendar whenever dashboard is shown
dashboard:connect_signal("property::visible", function ()
	 if dashboard.visible then
		 calendar.date = os.date('*t')
	 end
end)
local calendar_box = create_boxed_widget(calendar, dpi(300), dpi(300), widgets_background)

-- Music
local music_text = awful.widget.watch("mocp -M " .. os.getenv("HOME") .. "/.config/moc" .. " -i", 10, function(music_widget, stdout)
	moc_now = {
		state   = "N/A",
		file    = "N/A",
		artist  = "N/A",
		title   = "N/A",
		album   = "N/A",
		elapsed = "N/A",
		total   = "N/A"
	}

	for line in string.gmatch(stdout, "[^\n]+") do
		for k, v in string.gmatch(line, "([%w]+):[%s](.*)$") do
			if     k == "State"       then moc_now.state   = v
			elseif k == "File"        then moc_now.file    = v
			elseif k == "Artist"      then moc_now.artist  = escape_f(v)
			elseif k == "SongTitle"   then moc_now.title   = escape_f(v)
			elseif k == "Album"       then moc_now.album   = escape_f(v)
			elseif k == "CurrentTime" then moc_now.elapsed = escape_f(v)
			elseif k == "TotalTime"   then moc_now.total   = escape_f(v)
			end
		end
	end

	if moc_now.state == 'PLAY' or moc_now.state == 'PAUSE' then
		music_widget.visible = true

		if moc_now.total == 'N/A' then
			-- Remote m3a (Like Rainwave)
			music_widget:set_markup("<span color='" .. beautiful.success .. "'>" .. moc_now.title .. "</span>")
		else
			if moc_now.artist == "N/A" then
				music_widget:set_markup("<span color='" .. beautiful.success .. "'>" .. moc_now.title .. "</span>")
			else
				music_widget:set_markup(moc_now.artist .. " - <span color='" .. beautiful.success .. "'>" .. moc_now.title .. "</span>")
			end
		end
	else
		music_widget.visible = false
	end

end)
local music = wibox.widget {
	{
		align = "left",
		valign = "center",
		widget = music_text
	},
	margins = box_gap * 4,
	color = "#00000000",
	widget = wibox.container.margin
}
local music_box = create_boxed_widget(music, dpi(300), dpi(300), widgets_background)
-- music_box:buttons(gears.table.join(
-- 	awful.button({}, 1, function ()
-- 		exit_screen_show()
-- 		gears.timer.delayed_call(function()
-- 			dashboard_hide()
-- 		end)
-- 	end)
-- ))

-- Uptime
local uptime_text = wibox.widget.textbox()
awful.widget.watch("uptime -p | sed 's/^...//'", 60, function(_, stdout)
	-- Remove trailing whitespaces
	local out = stdout:gsub('^%s*(.-)%s*$', '%1')
	uptime_text.text = firstToUpper(out)
end)
local uptime = wibox.widget {
	{
		align = "center",
		valign = "center",
		-- font = "sans medium 11",
		widget = uptime_text
	},
	spacing = dpi(5),
	layout = wibox.layout.fixed.horizontal
}
local uptime_box = create_boxed_widget(uptime, dpi(300), dpi(80), widgets_background)
uptime_box:buttons(gears.table.join(
	awful.button({}, 1, function ()
		exit_screen_show()
		gears.timer.delayed_call(function()
			dashboard_hide()
		end)
	end)
))
-- helpers.add_hover_cursor(uptime_box, "hand1")

-- Item placement
dashboard:setup {
	-- Center boxes vertically
	nil,
	{
		-- Center boxes horizontally
		nil,
		{
			-- Column container
			{
				-- Column 1
				-- user_box,
				music_box,
				fortune_box,
				layout = wibox.layout.fixed.vertical
			},
			-- {
				-- Column 2
			--	 url_petals_box,
			--	 notification_state_box,
			--	 screenshot_box,
			--	 disk_box,
			--	calendar_box,
			--	layout = wibox.layout.fixed.vertical
			-- },
			-- {
			--	 -- Column 3
			--	 bookmarks_box,
			--	 corona_box,
			--	 layout = wibox.layout.fixed.vertical
			-- },
			{
				-- Column 4
				calendar_box,
				uptime_box,
				layout = wibox.layout.fixed.vertical
			},
			layout = wibox.layout.fixed.horizontal
		},
		nil,
		expand = "none",
		layout = wibox.layout.align.horizontal

	},
	nil,
	expand = "none",
	layout = wibox.layout.align.vertical
}


local dashboard_grabber
dashboard.hide = function()
	awful.keygrabber.stop(dashboard_grabber)
	set_visibility(false)
end


dashboard.show = function()
	dashboard_grabber = awful.keygrabber.run(function(_, key, event)
		if event == "release" then return end
		-- Press Escape or others to hide it
		if key == 'Escape' or key == 'q' or key == 'a' or key == '²' or key == config.modkey then
			dashboard.hide()
		end
	end)
	set_visibility(true)
end

return dashboard
