-- ---------------------------------------------------------------------
-- Init
-- ---------------------------------------------------------------------

-- init random
math.randomseed(os.time());

-- Standard lua
local string = require("string")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local lain	= require("lain")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup").widget
local os	  = { getenv = os.getenv, setlocale = os.setlocale }

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
	naughty.notify({ preset = naughty.config.presets.critical,
					 title = "Oops, there were errors during startup!",
					 text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
	local in_error = false
	awesome.connect_signal("debug::error", function (err)
		-- Make sure we don't go into an endless error loop
		if in_error then return end
		in_error = true

		naughty.notify({ preset = naughty.config.presets.critical,
						 title = "Oops, an error happened!",
						 text = tostring(err) })
		in_error = false
	end)
end
-- }}}

-- ---------------------------------------------------------------------
-- Include personal config
-- ---------------------------------------------------------------------
function isModuleAvailable(name)
  if package.loaded[name] then
	return true
  else
	for _, searcher in ipairs(package.searchers or package.loaders) do
	  local loader = searcher(name)
	  if type(loader) == 'function' then
		package.preload[name] = loader
		return true
	  end
	end
	return false
  end
end

function requireSafe(lib)
	if isModuleAvailable(lib) then
		require(lib)
	end
end

-- require("mail")
-- only for mars
if awesome.hostname == 'mars' then
	requireSafe('mail')
end

-- ---------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
-- beautiful.init(awful.util.get_themes_dir() .. "default/theme.lua")
beautiful.init("~/.config/awesome/themes/thetheme/theme.lua")

-- This is used later as the default terminal and editor to run.
-- terminal = "x-terminal-emulator"
-- terminal = "urxvt"
terminal = "rxvt-unicode"
editor = os.getenv("EDITOR") or "editor"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Notifications
naughty.config.defaults.timeout = 30
naughty.config.defaults.screen = screen.primary
naughty.config.defaults.position = "top_right"
naughty.config.defaults.margin = 10
naughty.config.defaults.gap = 35
naughty.config.defaults.ontop = true
naughty.config.defaults.border_width = 1
naughty.config.defaults.hover_timeout = nil
naughty.config.defaults.fg = beautiful.fg_focus
naughty.config.defaults.bg = beautiful.bg_Efocus
naughty.config.defaults.border_color = beautiful.border_focus

naughty.config.presets.low.timeout = 10
naughty.config.presets.critical.bg = beautiful.error
naughty.config.presets.critical.border_color = beautiful.fg_urgent

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
	awful.layout.suit.tile,
	awful.layout.suit.tile.left,
	-- awful.layout.suit.tile.bottom,
	-- awful.layout.suit.tile.top,
	-- awful.layout.suit.spiral,
	-- awful.layout.suit.spiral.dwindle,
	awful.layout.suit.fair,
	awful.layout.suit.fair.horizontal,
	awful.layout.suit.max,
	-- awful.layout.suit.max.fullscreen,
	-- awful.layout.suit.magnifier,
	awful.layout.suit.floating,
	-- awful.layout.suit.corner.nw,
	-- awful.layout.suit.corner.ne,
	-- awful.layout.suit.corner.sw,
	-- awful.layout.suit.corner.se,
}
-- }}}

-- Get the list of files from a directory
function scanDir(directory)
	local i, fileList, popen = 0, {}, io.popen
	for filename in popen("find " .. directory .. " -type f | sort"):lines() do
		i = i + 1
		fileList[i] = filename
	end
	return fileList
end

-- Wallpaper
local function set_wallpaper(s)
	if awful.util.file_readable(os.getenv("HOME") .. '/.wallpaper') then
		-- if ~/.wallpaper is a file, use it
		local wallpaper = os.getenv("HOME") .. '/.wallpaper'
		gears.wallpaper.maximized(wallpaper, s, true)
	elseif awful.util. dir_readable (os.getenv("HOME") .. '/.wallpaper') then
		-- if ~/.wallpaper is a directory, pick one into it
		local wallpapers = scanDir(os.getenv("HOME") .. '/.wallpaper')
		-- If we got a tag and an associated wallpaper
		local tag = awful.screen.focused().selected_tag
		if tag and wallpapers[tag.index] then
			-- if on a tag, use his index to find a wallpaper
			local wallpaper = wallpapers[tag.index]
			gears.wallpaper.maximized(wallpaper, s, true)
		else
			-- Fallback to random one
			-- local wallpaper = wallpapers[math.random(#wallpapers)]
			-- Fallback to first one
			local wallpaper = wallpapers[1]
			gears.wallpaper.maximized(wallpaper, s, true)
		end

	else
		-- Fallback to beautiful.wallpaper
		if beautiful.wallpaper then
			local wallpaper = beautiful.wallpaper
			-- If wallpaper is a function, call it with the screen
			if type(wallpaper) == "function" then
				wallpaper = wallpaper(s)
			end
			gears.wallpaper.maximized(wallpaper, s, true)
		end
	end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

-- Quake like terminal (single instance for all screens)
local quaketerm = lain.util.quake({
	-- client name
	name = "guaketerm",
	-- client to spawn
	app = terminal ,
	-- extra app arguments
	extra = "-title terminal -e " .. os.getenv("HOME") .. "/.dotfiles/bin/tmuxdev",
	-- border width
	border = 0,
	-- initially visible
	-- visible = false,
	-- Overlap the wibox or not
	overlap = true,
	-- always spawn on currently focused screen
	followtag = false,
	-- dropdown client height (float in [0,1] or exact pixels number)
	height = 1,
	-- dropdown client width (float in [0,1] or exact pixels number)
	width = 1,
	-- vertical position (string, possible values: "top", "bottom", "center")
	vert = "top",
	-- horizontal position (string, possible values: "left", "right", "center")
	horiz = "center",
	-- settings is a function which takes the client as input, and can be used to customize its properties
	settings = function(c)
		-- titlebars_enabled = false
		c.fullscreen = true
		-- c.sticky = true
	end
})

-- Quake like editor (single instance for all screens)
local quakeeditor = lain.util.quake({
	-- client name
	name = "guakeeditor",
	-- client to spawn
	app = 'emacs',
	-- extra app arguments
	extra = "",
	-- border width
	border = 0,
	-- initially visible
	-- visible = false,
	-- Overlap the wibox or not
	overlap = false,
	-- always spawn on currently focused screen
	followtag = false,
	-- dropdown client height (float in [0,1] or exact pixels number)
	height = 1,
	-- dropdown client width (float in [0,1] or exact pixels number)
	width = 1,
	-- vertical position (string, possible values: "top", "bottom", "center")
	vert = "top",
	-- horizontal position (string, possible values: "left", "right", "center")
	horiz = "center",
	-- settings is a function which takes the client as input, and can be used to customize its properties
	-- settings = function(c)
		-- titlebars_enabled = false
		-- c.fullscreen = true
		-- c.sticky = true
	-- end
})

-- ---------------------------------------------------------------------
-- Status bar
-- ---------------------------------------------------------------------

-- {{{ Helper functions
local function client_menu_toggle_fn()
	local instance = nil

	return function ()
		if instance and instance.wibox.visible then
			instance:hide()
			instance = nil
		else
			instance = awful.menu.clients({ theme = { width = 250 } })
		end
	end
end
-- }}}

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

-- {{{ Wibar
-- Create a wibox for each screen and add it
local taglist_buttons = awful.util.table.join(
					awful.button({ }, 1, function(t) t:view_only() end),
					awful.button({ modkey }, 1, function(t)
											  if client.focus then
												  client.focus:move_to_tag(t)
											  end
										  end),
					awful.button({ }, 3, awful.tag.viewtoggle),
					awful.button({ modkey }, 3, function(t)
											  if client.focus then
												  client.focus:toggle_tag(t)
											  end
										  end),
					awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
					awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
				)

local tasklist_buttons = awful.util.table.join(
					 awful.button({ }, 1, function (c)
											if c == client.focus then
												c.minimized = true
											else
												-- Without this, the following
												-- :isvisible() makes no sense
												c.minimized = false
												if not c:isvisible() and c.first_tag then
													c.first_tag:view_only()
												end
												-- This will also un-minimize
												-- the client, if needed
												client.focus = c
												c:raise()
											  end
										  end),
					 awful.button({ }, 3, client_menu_toggle_fn()))
					-- awful.button({ }, 4, function ()
					--						  awful.client.focus.byidx(1)
					--					  end),
					-- awful.button({ }, 5, function ()
					--						  awful.client.focus.byidx(-1)
					--					  end))

-- Separator
myspaceseparator = wibox.widget.textbox('  ')

-- Textclock widget with calendar
local mytextclock = wibox.widget.textclock("%a %d %b  <span color='#ffffff'>%H:%M:%S</span>", 1)
-- TODO: icon doesn't show ?
local myclockicon = wibox.widget.imagebox(beautiful.clock)
lain.widget.calendar({
	-- TODO: fix current day highlighted https://github.com/lcpz/lain/issues/300
	cal = '/usr/bin/env TERM=linux ncal -M -b -3 -w',
	attach_to = { mytextclock  },
	notification_preset = {
	   font = beautiful.font,
   }
})

-- Disks bar
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

-- Net bar
local neticon = wibox.widget.imagebox(beautiful.net)
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
local mynetwidget = wibox.container.margin(netbg, 2, 7, 4, 4)

-- CPU bar
local cpuicon = wibox.widget.imagebox(beautiful.cpu)
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
local mycpuwidget = wibox.container.margin(cpubg, 2, 7, 4, 4)

-- Ram bar
local memicon = wibox.widget.imagebox(beautiful.mem)
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
local mymemwidget = wibox.container.margin(membg, 2, 7, 4, 4)

-- ALSA volume bar
local volicon = wibox.widget.imagebox(beautiful.vol)
local volume = lain.widget.alsabar({
	width = 100, border_width = 0, ticks = false, ticks_size = 10,
	timeout = 2, notification_preset = { font = beautiful.font },
	--togglechannel = "IEC958,3",
	settings = function()
		if volume_now.status == "off" then
			volicon:set_image(beautiful.vol_mute)
		elseif volume_now.level == 0 then
			volicon:set_image(beautiful.vol_no)
		elseif volume_now.level <= 50 then
			volicon:set_image(beautiful.vol_low)
		else
			volicon:set_image(beautiful.vol)
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
local myvolumewidget = wibox.container.margin(volumebg, 2, 7, 4, 4)

-- Moc
local mymocbar = wibox.widget {
	forced_height	= 1,
	forced_width	= 100,
	margins			= 1,
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
	widget		   = wibox.widget.progressbar
}
local mymocbarbg = wibox.container.background(mymocbar, beautiful.info, gears.shape.rectangle)
local mymocbarwidget = wibox.container.margin(mymocbarbg, 2, 7, 4, 4)
mymocbarwidget.visible = false

local musicicon = wibox.widget.imagebox(beautiful.music)
musicicon.visible = false
local moc = lain.widget.contrib.moc({
	music_dir = "/data/media/music",
	settings  = function()
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
					mymocbarwidget.visible = true
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
			mymocbarwidget.visible = false
			-- mymocbar:set_value(0)
		end
	end
})
local mocbg = wibox.container.background(moc.widget, beautiful.bg_normal, gears.shape.rectangle)
local mymoc = wibox.container.margin(mocbg, 2, 7, 4, 4)

-- VPN
local vpnicon = wibox.widget.imagebox(beautiful.net_wired)
vpnicon.visible = false
local vpn = awful.widget.watch(
	"ip addr show tun0",
	5,
	function(widget, stdout, stderr, exitreason, exitcode)
		if exitcode == 0 then
			widget:set_markup("<span color='" .. beautiful.success .. "'>VPN: ON</span>")
			vpnicon.visible = true
		else
			widget:set_markup("")
			vpnicon.visible = false
		end
	end
)
local myvpn = wibox.container.margin(vpn, 2, 7, 4, 4)


-- Battery
local baticon = wibox.widget.imagebox(beautiful.battery)
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
	widget		   = wibox.widget.progressbar
}
batbar.visible  = false
baticon.visible = false
local battery = lain.widget.bat({
	settings = function()
		if bat_now.status == "N/A" then
			-- No battery
			batbar.visible  = false
			baticon.visible = false
		else
			batbar.visible  = true
			baticon.visible = true
			batbar:set_value(bat_now.perc)

			if bat_now.status == "Charging" then
				batbar.color = beautiful.info
			elseif bat_now.status == "Discharging" then
				batbar.color = beautiful.success
			elseif bat_now.status == "Full" then
				batbar.color = beautiful.success
			end

			-- Change icon if low battery
			if bat_now.perc <= 15 then
				baticon:set_image(beautiful.battery_low)
				batbar.color = beautiful.error
			elseif bat_now.perc <= 5 then
				baticon:set_image(beautiful.battery_empty)
				batbar.color = beautiful.error
			else
			-- 	baticon:set_image(beautiful.battery)
			end
		end
	end,
})
local batbg = wibox.container.background(batbar, beautiful.info, gears.shape.rectangle)
local mybatwidget = wibox.container.margin(batbg, 2, 7, 4, 4)


-- theme.bat                                       = theme.dir .. "/icons/bat.png"
-- theme.bat_low                                   = theme.dir .. "/icons/bat_low.png"
-- theme.bat_no                                    = theme.dir .. "/icons/bat_no.png"
-- theme.battery                                   = theme.dir .. "/icons/battery.png"
-- theme.battery_empty                             = theme.dir .. "/icons/battery_empty.png"
-- theme.battery_low                               = theme.dir .. "/icons/battery_low.png"

-- Crypto
-- TODO: Use icons
-- local crypto = awful.widget.watch(
--	 'curl -m10 -s "https://min-api.cryptocompare.com/data/price?fsym=XMR&tsyms=USD"',
--	 10800, -- 3 heures
--	 function(widget, stdout, stderr, exitreason, exitcode)
-- 		local xmr, pos, err = require("lain.util").dkjson.decode(stdout, 1, nil)
-- 		local xmr_price = 'XMR: $'.. (not err and xmr and xmr["USD"]) or "N/A"
--		 widget:set_text(xmr_price)
--	 end
-- )
-- local mycrypto = wibox.container.margin(crypto, 2, 7, 4, 4)


-- -- Mail - in mail.lua
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

awful.screen.connect_for_each_screen(function(s)
	-- Wallpaper
	set_wallpaper(s)

	-- Each screen has its own tag table.
	-- local names = { "main", "www", "skype", "gimp", "office", "im", "7", "8", "9" }
	-- local l = awful.layout.suit  -- Just to save some typing: use an alias.
	-- local layouts = { l.floating, l.tile, l.floating, l.fair, l.max, l.floating, l.tile.left, l.floating, l.floating }
	-- awful.tag(names, s, layouts)
	awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])

	-- Create a promptbox for each screen
	s.mypromptbox = awful.widget.prompt()
	-- Create an imagebox widget which will contains an icon indicating which layout we're using.
	-- We need one layoutbox per screen.
	s.mylayoutbox = awful.widget.layoutbox(s)
	s.mylayoutbox:buttons(awful.util.table.join(
						   awful.button({ }, 1, function () awful.layout.inc( 1) end),
						   awful.button({ }, 3, function () awful.layout.inc(-1) end),
						   awful.button({ }, 4, function () awful.layout.inc( 1) end),
						   awful.button({ }, 5, function () awful.layout.inc(-1) end)))
	-- Create a taglist widget
	s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.noempty, taglist_buttons)
	-- s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)

	-- Create a tasklist widget
	s.mytasklist = awful.widget.tasklist(
		s,
		awful.widget.tasklist.filter.currenttags,
		tasklist_buttons
	)
	-- screen = s,
	--	filter = awful.widget.tasklist.filter.currenttags,
		--uttons = tasklist_buttons
	-- })

	-- Create the wibox
	-- TODO: improve like this : https://github.com/awesomeWM/awesome/blob/dd5be865c3d00c580389c38ea41b6719ab567d3e/tests/_wibox_helper.lua
	s.mywibox = awful.wibar({
		position = "top",
		screen = s,
		--height = 25
	})

	if s == screen.primary then
		-- Add widgets to the wibox
		s.mywibox:setup {
			layout = wibox.layout.align.horizontal,
			{ -- Left widgets
				layout = wibox.layout.fixed.horizontal,
				s.mytaglist,
				s.mypromptbox,
			},
			{ -- Middle widget
				widget = wibox.container.margin,
				left = 10,
				right = 10,
				{
					layout = s.mytasklist,
					-- layout = wibox.layout.fixed.horizontal,
					-- s.mytasklist,
				}
			},
			{ -- Right widgets
				layout = wibox.layout.fixed.horizontal,
				-- layout = awful.widget.only_on_screen,
				-- screen = "primary", -- Only display on primary screen
				musicicon,
				mymoc,
				mymocbarwidget,
				myspaceseparator,
				vpnicon,
				myvpn,
				neticon,
				mynetwidget,
				-- fsicon,
				-- myfswidget,
				cpuicon,
				mycpuwidget,
				memicon,
				mymemwidget,
				baticon,
				mybatwidget,
				volicon,
				myvolumewidget,
				-- Widget for main screen only
				-- TODO: Should use awful.widget.only_on_screen after upgrade
				-- s == screen.primary and myspaceseparator,
				-- s == screen.primary and myimapcheckdev,
				-- s == screen.primary and mailicondev,
				-- s == screen.primary and myimapcheckpers,
				-- s == screen.primary and mailiconpers,
				-- myspaceseparator,
				-- mycrypto,
				myspaceseparator,
				mykeyboardlayout,
				wibox.widget.systray(),
				myspaceseparator,
				myspaceseparator,
				myclockicon,
				myspaceseparator,
				mytextclock,
				myspaceseparator,
				s.mylayoutbox,
			},
		}
	else
		-- secondary screen
		s.mywibox:setup {
			layout = wibox.layout.align.horizontal,
			{ -- Left widgets
				layout = wibox.layout.fixed.horizontal,
				s.mytaglist,
				s.mypromptbox,
			},
			s.mytasklist,
			{
				layout = wibox.layout.fixed.horizontal,
				myspaceseparator,
				s.mylayoutbox,
			},
		}
	end
end)
-- }}}

-- ---------------------------------------------------------------------
-- Key bindings
-- ---------------------------------------------------------------------

-- {{{ Key bindings
globalkeys = awful.util.table.join(
	awful.key({ modkey,		   }, "h",	  hotkeys_popup.show_help,
			  {description="show help", group="awesome"}),
	-- awful.key({ modkey, "Shift"   }, "Left",   awful.tag.viewprev,
	--		   {description = "view previous", group = "tag"}),
	-- awful.key({ modkey, "Shift"   }, "Right",  awful.tag.viewnext,
	--		   {description = "view next", group = "tag"}),
	-- awful.key({ modkey,		   }, "Escape", awful.tag.history.restore,
	--		   {description = "go back", group = "tag"}),

	-- awful.key({ modkey,		   }, "Right",
	--	 function ()
	--		 awful.client.focus.byidx( 1)
	--	 end,
	--	 {description = "focus next by index", group = "client"}
	-- ),
	-- awful.key({ modkey,		   }, "Left",
	--	 function ()
	--		 awful.gclient.focus.byidx(-1)
	--	 end,
	--	 {description = "focus previous by index", group = "client"}
	-- ),

	 -- By direction client focus
	awful.key({ modkey }, "Down",
		function()
			awful.client.focus.global_bydirection("down")
			if client.focus then client.focus:raise() end
		end,
		{description = "change client focus", group = "client"}),
	awful.key({ modkey }, "Up",
		function()
			awful.client.focus.global_bydirection("up")
			if client.focus then client.focus:raise() end
		end,
		{description = "change client focus", group = "client"}),
	awful.key({ modkey }, "Left",
		function()
			awful.client.focus.global_bydirection("left")
			if client.focus then client.focus:raise() end
		end,
		{description = "change client focus", group = "client"}),
	awful.key({ modkey }, "Right",
		function()
			awful.client.focus.global_bydirection("right")
			if client.focus then client.focus:raise() end
		end,
		{description = "change client focus", group = "client"}),

	-- Next/previous tag
	-- PageUp doesn't work: https://github.com/awesomeWM/awesome/issues/2147
	--awful.key({ modkey, }, "PageUp",   awful.tag.viewprev ),
	--awful.key({ modkey, }, "PageDown", awful.tag.viewnext ),

	-- awful.key({ modkey,		   }, "w", function () mymainmenu:show() end,
	--		  {description = "show main menu", group = "awesome"}),

	-- Layout manipulation
	awful.key({ modkey, "Shift"   }, "Right", function () awful.client.swap.global_bydirection('right')	end,
			  {description = "swap with next client by index", group = "client"}),
	awful.key({ modkey, "Shift"   }, "Left", function () awful.client.swap.global_bydirection('left')	end,
			  {description = "swap with previous client by index", group = "client"}),
	awful.key({ modkey, "Shift"   }, "Up", function () awful.client.swap.global_bydirection('up')	end,
			  {description = "swap with next client by index", group = "client"}),
	awful.key({ modkey, "Shift"   }, "Down", function () awful.client.swap.global_bydirection('down')	end,
			  {description = "swap with previous client by index", group = "client"}),
	awful.key({ modkey, "Control" }, "Right", function () awful.screen.focus_relative(1) end,
			  {description = "focus the next screen", group = "screen"}),
	awful.key({ modkey, "Control" }, "Left", function () awful.screen.focus_relative(-1) end,
			  {description = "focus the previous screen", group = "screen"}),
	awful.key({ modkey,		   }, "u", awful.client.urgent.jumpto,
			  {description = "jump to urgent client", group = "client"}),
	awful.key({ modkey,		   }, "Tab",
		function ()
			awful.client.focus.history.previous()
			if client.focus then
				client.focus:raise()
			end
		end,
		{description = "go back", group = "client"}),

	-- On the fly useless gaps change
	-- awful.key({ altkey, "Control" }, "+", function () lain.util.useless_gaps_resize(1) end),
	-- awful.key({ altkey, "Control" }, "-", function () lain.util.useless_gaps_resize(-1) end),

	-- Reload
	awful.key({ modkey, "Shift"   }, "r", awesome.restart,
			  {description = "reload awesome", group = "awesome"}),
	-- awful.key({ modkey, "Shift"   }, "s", awesome.quit,
	--		  {description = "quit awesome", group = "awesome"}),

	-- Terminal
	-- awful.key({				   }, "²", function () awful.spawn(os.getenv("HOME") .. "/.dotfiles/bin/guakify 'rxvt-unicode.URxvt' '" .. terminal .. " -e " .. os.getenv("HOME") .. "/.dotfiles/bin/tmuxdev'") end,
	--		  {description = "open a terminal", group = "launcher"}),
	awful.key({ modkey,		   }, "Return", function () awful.spawn(terminal .. " -title terminal -e " .. os.getenv("HOME") .. "/.dotfiles/bin/tmuxdev") end,
			  {description = "open a terminal", group = "launcher"}),

	-- awful.key({ modkey,		   }, "l",	 function () awful.tag.incmwfact( 0.05)		  end,
	--		   {description = "increase master width factor", group = "layout"}),
	-- awful.key({ modkey,		   }, "h",	 function () awful.tag.incmwfact(-0.05)		  end,
	--		  {description = "decrease master width factor", group = "layout"}),
	-- awful.key({ modkey, "Shift"   }, "h",	 function () awful.tag.incnmaster( 1, nil, true) end,
	--		   {description = "increase the number of master clients", group = "layout"}),
	-- awful.key({ modkey, "Shift"   }, "l",	 function () awful.tag.incnmaster(-1, nil, true) end,
	--		   {description = "decrease the number of master clients", group = "layout"}),
	-- awful.key({ modkey, "Control" }, "h",	 function () awful.tag.incncol( 1, nil, true)	end,
	--		   {description = "increase the number of columns", group = "layout"}),
	-- awful.key({ modkey, "Control" }, "l",	 function () awful.tag.incncol(-1, nil, true)	end,
	--		   {description = "decrease the number of columns", group = "layout"}),
	awful.key({ modkey,	"Control" }, "space", function () awful.layout.inc( 1)				end,
			  {description = "select next", group = "layout"}),
	awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)				end,
			  {description = "select previous", group = "layout"}),

	-- awful.key({ modkey, "Control" }, "n",
	--		   function ()
	--			   local c = awful.client.restore()
	--			   -- Focus restored client
	--			   if c then
	--				   client.focus = c
	--				   c:raise()
	--			   end
	--		   end,
	--		   {description = "restore minimized", group = "client"}),

	-- Prompt
	awful.key({ modkey },			"x",	 function () awful.screen.focused().mypromptbox:run() end,
			  {description = "run prompt", group = "launcher"}),

	-- awful.key({ modkey }, "x",
	--		   function ()
	--			   awful.prompt.run {
	--				 prompt	   = "Run Lua code: ",
	--				 textbox	  = awful.screen.focused().mypromptbox.widget,
	--				 exe_callback = awful.util.eval,
	--				 history_path = awful.util.get_cache_dir() .. "/history_eval"
	--			   }
	--		   end,
	--		   {description = "lua execute prompt", group = "awesome"}),
	-- Menubar
	awful.key({ modkey,		    }, "d", function() menubar.show() end,
			  {description = "show the menubar", group = "launcher"}),
	awful.key({ modkey, "Shift" }, "d", function() menubar.refresh() end,
			  {description = "refresh the menubar", group = "launcher"}),

	-- awful.key({ modkey }, "e", function()
	-- 	awful.util.spawn("subl", false)
	-- end),

	-- quake-like terminal
	awful.key({}, "²", function ()
		quaketerm:toggle()
	end, {description = "Toggle guake like terminal", group = "launcher"}),

	-- quake-like editor
	awful.key({}, "F1", function ()
		quakeeditor:toggle()
	end, {description = "Toggle guake like editor", group = "launcher"}),

	-- Text Editor
	-- awful.key({}, "F1", function()
	 	-- awful.spawn(os.getenv("HOME") .. "/.dotfiles/bin/guakify 'sublime_text.Sublime_text' '/opt/sublime_text/sublime_text'")
	-- 	awful.spawn(os.getenv("HOME") .. "/.dotfiles/bin/guakify 'emacs.Emacs' 'emacs'")
	--end, {description = "Toggle guake like editor", group = "launcher"}),
	awful.key({ modkey }, "e", function()
		awful.util.spawn("emacs", false)
	end, {description = "launch editor", group = "launcher"}),

	-- Volume Keys
	awful.key({}, "XF86AudioLowerVolume", function ()
		awful.util.spawn("amixer -q sset Master 5%-", false)
		volume.update()
	end, {description = "Volume UP", group = "audio"}),
	awful.key({}, "XF86AudioRaiseVolume", function ()
		awful.util.spawn("amixer -q sset Master 5%+", false)
		volume.update()
	end, {description = "Volume down", group = "audio"}),
	awful.key({}, "XF86AudioMute", function ()
		awful.util.spawn("amixer set Master 1+ toggle", false)
		volume.update()
	end, {description = "volume mute", group = "audio"}),

	-- Media Keys
	awful.key({}, "XF86AudioPlay", function()
		awful.util.spawn("music --toggle-pause", false)
	end, {description = "audio toggle play/pause", group = "audio"}),
	awful.key({}, "XF86AudioStop", function()
		awful.util.spawn("music --stop", false)
	end, {description = "music stop", group = "audio"}),
	awful.key({}, "XF86AudioNext", function()
		awful.util.spawn("music --next", false)
	end, {description = "music next", group = "audio"}),
	awful.key({}, "XF86AudioPrev", function()
		awful.util.spawn("music --previous", false)
	end, {description = "music previous", group = "audio"}),

	-- lock
	awful.key({ modkey, "Shift" }, "l", function()
		-- awful.util.spawn("i3lock --color 001912 --show-failed-attempts --ignore-empty-password", false)
		awful.util.spawn("i3lock --color 001905 --show-failed-attempts --ignore-empty-password", false)
	end, {description = "lock screen", group = "launcher"}),
	-- shutdown or restart
	awful.key({ modkey, "Shift" }, "s", function()
		awful.util.spawn(os.getenv("HOME") .. "/.dotfiles/bin/dmenu_shutdown", false)
	end, {description = "shutdown", group = "launcher"}),
	-- manage VPN
	awful.key({ modkey, "Shift" }, "v", function()
		awful.util.spawn(os.getenv("HOME") .. "/.dotfiles/bin/dmenu_vpn", false)
	end, {description = "launch vpn", group = "launcher"})
)

clientkeys = awful.util.table.join(
	awful.key({ modkey,		   }, "f",
		function (c)
			c.fullscreen = not c.fullscreen
			c:raise()
		end,
		{description = "toggle fullscreen", group = "client"}),
	awful.key({ modkey,		   }, "c",
	   function (c)
		   -- toggle titlebar
		   awful.titlebar.toggle(c)
	   end,
	   {description = "toggle titlebar", group = "client"}),
	awful.key({ modkey, "Shift"   }, "q",	  function (c) c:kill()						 end,
			  {description = "close", group = "client"}),
	awful.key({ modkey,       }, "space",  awful.client.floating.toggle					 ,
			  {description = "toggle floating", group = "client"}),
	awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
			  {description = "move to master", group = "client"}),
	awful.key({ modkey,		   }, "o",	  function (c) c:move_to_screen()			   end,
			  {description = "move to screen", group = "client"}),
	awful.key({ modkey,		   }, "t",	  function (c) c.ontop = not c.ontop			end,
			  {description = "toggle keep on top", group = "client"}),
	awful.key({ modkey,		   }, "n",
		function (c)
			-- The client currently has the input focus, so it cannot be
			-- minimized, since minimized clients can't have the focus.
			c.minimized = true
		end ,
		{description = "minimize", group = "client"}),
	awful.key({ modkey,		   }, "z",
		function (c)
			c.maximized = not c.maximized
			c:raise()
		end ,
		{description = "maximize", group = "client"}),
	awful.key({ modkey,		   }, "m",
		function (c)
			c.maximized = not c.maximized
			c:raise()
		end ,
		{description = "maximize", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
	globalkeys = awful.util.table.join(globalkeys,
		-- View tag only.
		awful.key({ modkey }, "#" .. i + 9,
				  function ()
						local screen = awful.screen.focused()
						local tag = screen.tags[i]
						local tag_current = awful.screen.focused().selected_tag

						if tag then
							if tag == tag_current then
								-- If already on focused screen, go to previous one
								awful.tag.history.restore()
							else
								-- Just go to the screen
								tag:view_only()
							end
							-- change wallpaper
							-- TODO: Should be during tag creation
							-- TODO: here, it doesn't handle mouse event
							set_wallpaper(screen)
						end
				  end,
				  -- {description = "view tag #"..i, group = "tag"}),
				  {description = "view tag", group = "tag"}),
		-- Toggle tag display.
		awful.key({ modkey, "Control" }, "#" .. i + 9,
				  function ()
					  local screen = awful.screen.focused()
					  local tag = screen.tags[i]
					  if tag then
						 awful.tag.viewtoggle(tag)
					  end
				  end,
				  -- {description = "toggle tag #" .. i, group = "tag"}),
				  {description = "toggle tag", group = "tag"}),
		-- Move client to tag.
		awful.key({ modkey, "Shift" }, "#" .. i + 9,
				  function ()
					  if client.focus then
						  local tag = client.focus.screen.tags[i]
						  if tag then
							  client.focus:move_to_tag(tag)
							  tag:view_only()
						  end
					 end
				  end,
				  -- {description = "move focused client to tag #"..i, group = "tag"}),
				  {description = "move focused client to tag", group = "tag"}),
		-- Toggle tag on focused client.
		awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
				  function ()
					  if client.focus then
						  local tag = client.focus.screen.tags[i]
						  if tag then
							  client.focus:toggle_tag(tag)
						  end
					  end
				  end,
				  -- {description = "toggle focused client on tag #" .. i, group = "tag"})
				  {description = "toggle focused client on tag", group = "tag"})
	)
end

clientbuttons = awful.util.table.join(
	awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
	awful.button({ modkey }, 1, awful.mouse.client.move),
	awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- ---------------------------------------------------------------------
-- Rules
-- ---------------------------------------------------------------------

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
max_screen_count = screen:count()
awful.rules.rules = {
	-- All clients will match this rule.
	{ rule = { },
		properties = {
			border_width = beautiful.border_width,
			border_color = beautiful.border_normal,
			focus = awful.client.focus.filter,
			raise = true,
			keys = clientkeys,
			buttons = clientbuttons,
			screen = awful.screen.preferred,
			titlebars_enabled = true,
			placement = awful.placement.no_overlap+awful.placement.no_offscreen
		}
	},

	-- Floating clients.
	{ rule_any = {
		instance = {
		  "DTA",  -- Firefox addon DownThemAll.
		  "copyq",  -- Includes session name in class.
		},
		class = {
		  "Arandr",
		  "Gpick",
		  "Kruler",
		  "MessageWin",  -- kalarm.
		  "Sxiv",
		  "Wpa_gui",
		  "pinentry",
		  "veromix",
		  "xtightvncviewer"},

		name = {
		  "Event Tester",  -- xev.
		},
		type = {
			"dialog"
		},
		role = {
		  "AlarmWindow",  -- Thunderbird's calendar.
		  -- "pop-up",	   -- e.g. Google Chrome's (detached) Developer Tools.
		}
	  }, properties = { floating = true }
	},

	-- Add titlebars to normal clients and dialogs
	{ rule_any = { type = { "normal", "dialog" } },
		properties = {
			titlebars_enabled = true
		}
	},

	-- default normal client rules
	{ rule_any = { type = { "normal" } },
		properties = {
			titlebars_enabled = true,
			floating = false,
			maximized_vertical = false,
			maximized_horizontal = false
		}
	},

	-- Set Firefox to always map on the tag named "2" on screen 1.
	-- { rule = { class = "Firefox" },
	--   properties = { screen = 1, tag = "2" } },

	{ rule = { class = "mpv" },
		properties = {
			-- Full screen on second monitor
			-- floating = false,
			-- sticky = true,
			-- fullscreen = false,
			-- screen = max_screen_count -- Open on last screen

			-- Sticky in corner
			focus = false,
			sticky = true,
			fullscreen = false,
			floating = true,
			ontop = true, -- Not compatible with fullscreen
			callback = function(c)
				-- 1/3 bottom right of focused screen
				sreen_geometry = awful.screen.focused().geometry
				c:geometry( { width = sreen_geometry.width / 3 , height = sreen_geometry.height / 3 } )
				awful.placement.bottom_right(c)
			end

			-- maximized_vertical = true,
			-- maximized_horizontal = true,
			-- titlebars_enabled = false,
		}
	},
	{ rule = { class = "Firefox" },
		except = { type = "dialog" },
		properties = {
			tag = "1",
			-- floating = false,
			-- titlebars_enabled = false,
			-- maximized_vertical = true,
			-- maximized_horizontal = true
			--switchtotag = true
		}
	},
	{ rule_any = { class = { "Code", "krita" }},
		except = { type = "dialog" },
		properties = {
			tag = "2",
			--floating = false,
			--maximized_vertical = true,
			--maximized_horizontal = true
		}
	},
	{ rule = { class = "Thunderbird" },
		except = { type = "dialog" },
		properties = {
			tag = "3"
		}
	},
	{ rule = { class = "Steam" },
		except = { type = "dialog" },
		properties = {
			tag = "8"
		}
	}
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
	-- Set the windows at the slave,
	-- i.e. put it at the end of others instead of setting it master.
	if not awesome.startup then awful.client.setslave(c) end

	if awesome.startup and
	  not c.size_hints.user_position
	  and not c.size_hints.program_position then
		-- Prevent clients from being unreachable after screen count changes.
		awful.placement.no_offscreen(c)
	end

	if (c.class == "Firefox") then
		-- if it's a Firefox we will connect a signal which will call if 'name' changing
		c:connect_signal("property::name", function(c)
			if (string.find(c.name, "(Private Browsing)")) then
				-- if "(Private Browsing)" is part of 'c.name' then 'c' goes to tags[1][9]
				local tags = root.tags()
				c:tags({tags[9]})
			end
		end)
	end

end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
	-- buttons for the titlebar
	local buttons = awful.util.table.join(
		awful.button({ }, 1, function()
			client.focus = c
			c:raise()
			awful.mouse.client.move(c)
		end),
		awful.button({ }, 3, function()
			client.focus = c
			c:raise()
			awful.mouse.client.resize(c)
		end)
	)

	awful.titlebar(c) : setup {
		{ -- Left
			awful.titlebar.widget.iconwidget(c),
			buttons = buttons,
			layout  = wibox.layout.fixed.horizontal
		},
		{ -- Middle
			{ -- Title
				align  = "left",
				widget = awful.titlebar.widget.titlewidget(c)
			},
			buttons = buttons,
			layout  = wibox.layout.flex.horizontal
		},
		{ -- Right
			awful.titlebar.widget.floatingbutton (c),
			awful.titlebar.widget.stickybutton   (c),
			awful.titlebar.widget.ontopbutton	(c),
			awful.titlebar.widget.maximizedbutton(c),
			awful.titlebar.widget.closebutton	(c),
			layout = wibox.layout.fixed.horizontal()
		},
		layout = wibox.layout.align.horizontal
	}
end)

-- Enable sloppy focus, so that focus follows mouse.
-- client.connect_signal("mouse::enter", function(c)
--	 if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
--		 and awful.client.focus.filter(c) then
--		 client.focus = c
--	 end
-- end)

-- No border for maximized clients
-- function border_adjust(c)
-- 	if c.maximized then
-- 		-- transparents borders if maximized
-- 		c.border_color = beautiful.border_focus .. "00"
-- 	else
-- 		-- c.border_width = beautiful.border_width
-- 		if max_screen_count > 1 then
-- 			-- dual screen
-- 			c.border_color = beautiful.border_focus
-- 		elseif #awful.screen.focused().clients > 1 then
-- 			-- On simple screen, show borders if more than one app on focused screen
-- 			c.border_color = beautiful.border_focus
-- 		end
-- 	end
-- end
-- client.connect_signal("focus", border_adjust)
-- client.connect_signal("property::maximized", border_adjust)
-- client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

-- Border on focused clients
client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)


-- ---------------------------------------------------------------------
-- Auto start
-- ---------------------------------------------------------------------

awful.spawn.with_shell("setxkbmap -model pc105 -layout fr,us -variant oss")
awful.spawn.with_shell("~/.dotfiles/bin/autostart")
