local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")
local naughty = require("naughty")
local lain = require("lain")

local widgets = {}

-- Separator
-- ----------------------------------------------------------------------------
widgets.spaceseparator = wibox.widget.textbox('  ')

-- Moc
-- ----------------------------------------------------------------------------
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
widgets.mocbarwidget = wibox.container.margin(mymocbarbg, 2, 7, 4, 4)
widgets.mocbarwidget.visible = false

local musicicon = wibox.widget.imagebox(beautiful.music)
musicicon.visible = false
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
			musicicon.visible = true

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
					widgets.mocbarwidget.visible = true
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
			musicicon.visible = false
			widgets.mocbarwidget.visible = false
			-- mymocbar:set_value(0)
		end
	end,
})
local mocbg = wibox.container.background(moc.widget, beautiful.bg_normal, gears.shape.rectangle)
widgets.mocwidget = wibox.container.margin(mocbg, 2, 7, 4, 4)
widgets.mocbarwidget:buttons(awful.util.table.join (
	awful.button({}, 1, function()
		-- TODO: doesn't work. PATH not set ?
		awful.util.spawn("music --toggle-pause", false)
	end),
	awful.button({}, 3, function()
		awful.util.spawn("music --next", false)
	end)
))

-- VPN
-- ----------------------------------------------------------------------------
widgets.vpnicon = wibox.widget.imagebox(beautiful.net_wired)
widgets.vpnicon.visible = false
local vpn = awful.widget.watch(
	"ip addr show tun0",
	10,
	function(widget, stdout, stderr, exitreason, exitcode)
		if exitcode == 0 then
			widget:set_markup("<span color='" .. beautiful.success .. "'>VPN: ON</span>")
			widgets.vpnicon.visible = true
		else
			widget:set_markup("")
			widgets.vpnicon.visible = false
		end
	end
)
widgets.vpn = wibox.container.margin(vpn, 2, 7, 4, 4)

-- Battery
-- ----------------------------------------------------------------------------
widgets.baticon = wibox.widget.imagebox(beautiful.battery)
local batbar = wibox.widget {
	forced_height	= 1,
	forced_width	= 100,
	margins			= 1,
	paddings		= 1,
	ticks			= true,
	ticks_size		= 10,
	step_width		= 10,
	max_value		= 100,
	min_value		= 0,
	value			= 0,
	color 			= beautiful.success,
	background_color = beautiful.bg_normal,
	border_color	= beautiful.info,
	widget		    = wibox.widget.progressbar
}
batbar.visible  = false
widgets.baticon.visible = false
local battery = lain.widget.bat({
	settings = function()
		if bat_now.status == "N/A" then
			-- No battery
			batbar.visible  = false
			widgets.baticon.visible = false
		else
			batbar.visible  = true
			widgets.baticon.visible = true
			batbar:set_value(bat_now.perc)

			-- Change icon and color if full or low battery
			if bat_now.perc >= 95 then
				batbar.color = beautiful.success_alt
			elseif bat_now.perc <= 15 then
				widgets.baticon:set_image(beautiful.battery_low)
				batbar.color = beautiful.error
			elseif bat_now.perc <= 5 then
				widgets.baticon:set_image(beautiful.battery_empty)
				batbar.color = beautiful.error
			else
				widgets.baticon:set_image(beautiful.battery)
				if bat_now.status == "Full" then
					batbar.color = beautiful.success_alt
				elseif bat_now.status == "Discharging" then
					batbar.color = beautiful.success
				elseif bat_now.status == "Charging" then
					batbar.color = beautiful.info

				end
			end
		end
	end,

	-- Batterie notifications
	bat_notification_charged_preset = {
		-- title   = "Battery full",
		-- text    = "You can unplug the cable",
		timeout = naughty.config.presets.low.timeout,
		fg      = naughty.config.presets.low.fg,
		bg      = naughty.config.presets.low.bg,
		preset  = naughty.config.presets.low
	},
	bat_notification_low_preset = {
		-- title = "Battery low",
		-- text = "Plug the cable!",
		timeout = naughty.config.presets.critical.timeout,
		fg = naughty.config.presets.critical.fg,
		bg = naughty.config.presets.critical.bg,
		preset = naughty.config.presets.critical
	},
	bat_notification_critical_preset = {
		-- title = "Battery exhausted",
		-- text = "Shutdown imminent",
		timeout = naughty.config.presets.critical.timeout,
		fg = naughty.config.presets.critical.fg,
		bg = naughty.config.presets.critical.bg,
		preset = naughty.config.presets.critical
	},
})
local batbg = wibox.container.background(batbar, beautiful.info, gears.shape.rectangle)
widgets.batwidget = wibox.container.margin(batbg, 2, 7, 4, 4)

-- Net bar
-- ----------------------------------------------------------------------------
widgets.neticon = wibox.widget.imagebox(beautiful.net)
local netbar = wibox.widget {
	forced_height 	= 1,
	forced_width 	= 100,
	margins 		= 1,
	paddings 		= 1,
	ticks 			= true,
	ticks_size 		= 10,
	step_width 		= 3,
	max_value 		= 1000,
	value 			= 0,
	color 			= beautiful.success,
	background_color = beautiful.bg_normal,
	border_color 	= beautiful.info,
	-- widget 			= wibox.widget.progressbar
	widget 			= wibox.widget.graph,
	-- TODO: not autodetected ?
	iface = 'enp2s0'
}
local net = lain.widget.net({
	-- width = 100, border_width = 0, ticks = true, ticks_size = 100,
	settings = function()
		-- netbar:set_value(net_now.received)
		netbar:add_value(tonumber(net_now.received))
	end
})
local netbg = wibox.container.background(netbar, beautiful.info, gears.shape.rectangle)
widgets.netwidget = wibox.container.margin(netbg, 2, 7, 4, 4)

-- Disks bar
-- ----------------------------------------------------------------------------
-- local fsicon = wibox.widget.imagebox(beautiful.hdd)
-- fsbar = wibox.widget {
--	 forced_height	= 1,
--	 forced_width	 = 100,
--	 margins		  = 1,
--	 paddings		 = 1,
--	 ticks			= true,
--	 ticks_size	   = 6,
--	 max_value 		 = 100,
--	 value			= 0,
--	 color 			 = beautiful.success,
--	 background_color = beautiful.info,
--	 border_color	 = beautiful.info,
--	 widget		   = wibox.widget.progressbar
-- }
-- fs = lain.widget.fs({
--	 partition = "/",
--	 -- options = "--exclude-type=tmpfs",
--	 settings  = function()
--		 if tonumber(fs_now.used) < 90 then
--			 fsbar:set_color(beautiful.success)
--		 else
--			 fsbar:set_color(beautiful.error)
--		 end
--		 fsbar:set_value(fs_now.used)
--	 end
-- })
-- local fsbg = wibox.container.background(fsbar, beautiful.info, gears.shape.rectangle)
-- local myfswidget = wibox.container.margin(fsbg, 2, 7, 4, 4)

-- CPU bar
-- ----------------------------------------------------------------------------
widgets.cpuicon = wibox.widget.imagebox(beautiful.cpu)
local cpubar = wibox.widget {
	forced_height 	= 1,
	forced_width 	= 100,
	margins 		= 1,
	paddings 		= 1,
	ticks 			= true,
	ticks_size 		= 10,
	step_width 		= 3,
	max_value 		= 100,
	min_value 		= 0,
	value 			= 0,
	color 			= beautiful.success,
	background_color = beautiful.bg_normal,
	border_color 	= beautiful.info,
	-- widget		   = wibox.widget.progressbar
	widget 			= wibox.widget.graph
}
local cpu = lain.widget.cpu({
	width = 100, border_width = 0, ticks = true, ticks_size = 10,
	settings = function()
		-- cpubar:set_value(cpu_now.usage)
		cpubar:add_value(cpu_now.usage)
		-- cpubar:set_value(cpu_now.used)
	end
})
local cpubg = wibox.container.background(cpubar, beautiful.info, gears.shape.rectangle)
widgets.cpuwidget = wibox.container.margin(cpubg, 2, 7, 4, 4)

-- Ram bar
-- ----------------------------------------------------------------------------
widgets.memicon = wibox.widget.imagebox(beautiful.mem)
local membar = wibox.widget {
	forced_height	= 1,
	forced_width	= 100,
	margins			= 1,
	paddings		= 1,
	ticks			= true,
	ticks_size		= 10,
	step_width		= 10,
	max_value		= 100,
	min_value		= 0,
	value			= 0,
	color 			= beautiful.success,
	background_color = beautiful.bg_normal,
	border_color	= beautiful.info,
	widget		   = wibox.widget.progressbar
	-- widget			= wibox.widget.graph
}
local mem = lain.widget.mem({
	width = 100, border_width = 0, ticks = true, ticks_size = 10,
	settings = function()
		membar:set_value(mem_now.perc)
		-- membar:add_value(mem_now.perc)
	end
})
local membg = wibox.container.background(membar, beautiful.info, gears.shape.rectangle)
widgets.memwidget = wibox.container.margin(membg, 2, 7, 4, 4)

-- Mail
-- in mail.lua
-- ----------------------------------------------------------------------------
-- local mailicondev = wibox.widget.imagebox()
-- mailicondev:buttons(awful.util.table.join(awful.button({ }, 1, function () awful.util.spawn(mail) end)))
-- local myimapcheckdev = lain.widget.imap({
-- 	timeout  = 180,
--  	is_plain = true,
--  	password = "nope",
--  	server = "mail.nope.net",
--  	mail = "nope@nope.fr",
--  	icon = beautiful.mail,
--  	settings = function()
--  		if mailcount > 0 then
--  			widget:set_markup("Dev <span color='#ffffff'>" .. mailcount .. '</span>')
--  			mailicondev:set_image(beautiful.mail_on)
--  		else
--  			widget:set_markup("")
--  			mailicondev:set_image(beautiful.mail)
--  		end
--  	end
-- })

-- ALSA volume bar
-- ----------------------------------------------------------------------------
widgets.volicon = wibox.widget.imagebox(beautiful.vol)
local volume = lain.widget.alsabar({
	width = 100,
	border_width = 0,
	ticks = false,
	ticks_size = 10,
	timeout = 2,
	notification_preset = { font = beautiful.font },
	--togglechannel = "IEC958,3",
	settings = function()
		if volume_now.status == "off" then
			widgets.volicon:set_image(beautiful.vol_mute)
		elseif volume_now.level == 0 then
			widgets.volicon:set_image(beautiful.vol_no)
		elseif volume_now.level <= 50 then
			widgets.volicon:set_image(beautiful.vol_low)
		else
			widgets.volicon:set_image(beautiful.vol)
		end
	end,
	colors = {
		background 	= beautiful.bg_normal,
		mute 		= beautiful.error,
		unmute 		= beautiful.fg_normal
	}
})
volume.tooltip.wibox.fg = beautiful.fg_focus
volume.bar:buttons(awful.util.table.join (
		awful.button({}, 1, function()
			awful.spawn.with_shell(string.format("%s -e alsamixer", terminal))
		end),
		awful.button({}, 2, function()
			awful.spawn(string.format("%s set %s 100%%", volume.cmd, volume.channel))
			volume.update()
		end),
		awful.button({}, 3, function()
			awful.spawn(string.format("%s set %s toggle", volume.cmd, volume.togglechannel or volume.channel))
			volume.update()
		end),
		awful.button({}, 4, function()
			awful.spawn(string.format("%s set %s 5%%+", volume.cmd, volume.channel))
			volume.update()
		end),
		awful.button({}, 5, function()
			awful.spawn(string.format("%s set %s 5%%-", volume.cmd, volume.channel))
			volume.update()
		end)
))
local volumebg = wibox.container.background(volume.bar, beautiful.info, gears.shape.rectangle)
widgets.volume = volume
widgets.volumewidget = wibox.container.margin(volumebg, 2, 7, 4, 4)

-- Prompt box
-- ----------------------------------------------------------------------------
widgets.promptbox = awful.widget.prompt()

-- Keyboard Layout
-- ----------------------------------------------------------------------------
widgets.systraykeyboardlayout = awful.widget.keyboardlayout()

-- Systray
-- ----------------------------------------------------------------------------
widgets.systray = wibox.widget.systray()

-- Textclock widget with calendar
-- ----------------------------------------------------------------------------
widgets.textclock = wibox.widget.textclock("%a %d %b  <span color='#ffffff'>%H:%M:%S</span>", 1)
widgets.clockicon = wibox.widget.imagebox(beautiful.clock)
local calendar = require("calendar")
-- attach it as popup to your text clock widget:
calendar({}):attach(widgets.textclock)
calendar({}):attach(widgets.clockicon)


return widgets