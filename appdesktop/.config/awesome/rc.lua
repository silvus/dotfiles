-- ---------------------------------------------------------------------
-- Init
-- ---------------------------------------------------------------------

-- init random
-- math.randomseed(os.time());

-- Standard lua
local string = require("string")
local os = { getenv = os.getenv, setlocale = os.setlocale }

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")

-- Makes sure that there's always a client that will have focus on events such as tag switching, client unmanaging, etc
require("awful.autofocus")

-- Widget and layout library
local wibox = require("wibox")
local widget_common = require("awful.widget.common")

-- Theme handling library
local beautiful = require("beautiful")

-- Notification library
local naughty = require("naughty")

-- Contrib utilities
local lain	= require("lain")

-- Base custom config
local config = require('config')

-- Tags declarations
local desktops = require('desktops')

-- Screen custom definitions
local screens = require('screens')

-- Wallpapers utilities
local wallpaper = require("utils.wallpaper")

-- Set a global variable, a local one
local globaltag = tag
local globalclient = client


-- ---------------------------------------------------------------------
-- Errors and DEBUG
-- ---------------------------------------------------------------------
-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
	naughty.notify({ preset = naughty.config.presets.critical,
					 title = "Oops, there were errors during startup!",
					 text = awesome.startup_errors })
	-- naughty.notify({text = 'notif text' })
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
-- Screens
-- ---------------------------------------------------------------------
screen.connect_signal("added", screens.update)
screen.connect_signal("removed", screens.update)
screens.update()

-- Set focused tag to the one with a mouse or an active client ?
-- awful.screen.default_focused_args = {client = true, mouse = true}


-- ---------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------

-- Themes define colours, icons, font and wallpapers.
beautiful.init(config.theme)

-- Notifications
naughty.config.defaults.timeout = 30
naughty.config.defaults.screen = screens.get_primary()
naughty.config.defaults.position = "top_right"
naughty.config.defaults.margin = 10
naughty.config.defaults.gap = 35
naughty.config.defaults.ontop = true
naughty.config.defaults.border_width = 1
naughty.config.defaults.hover_timeout = nil
naughty.config.defaults.fg = beautiful.fg_focus
naughty.config.defaults.bg = beautiful.bg_focus
naughty.config.defaults.border_color = beautiful.border_focus

naughty.config.presets.low.timeout = 10
naughty.config.presets.critical.bg = beautiful.error
naughty.config.presets.critical.border_color = beautiful.fg_urgent

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = config.layouts


-- ---------------------------------------------------------------------
-- Status bar
-- ---------------------------------------------------------------------

-- Customs widgets definitions (need to be loaded after theme init (or naughty configurations ?))
local widget_separator = require("widgets.separator")
local widget_tags = require("widgets.tags")
local widget_tasks = require("widgets.tasks")
local widget_clock = require("widgets.clock")
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

awful.screen.connect_for_each_screen(function(s)
	-- Wallpaper
	wallpaper.update(s)

	-- Tags init
	desktops.init(s)

	-- Create an imagebox widget which will contains an icon indicating which layout we're using.
	-- We need one layoutbox per screen.
	s.layoutbox = awful.widget.layoutbox(s)
	s.layoutbox:buttons(awful.util.table.join(
						   awful.button({ }, 1, function () awful.layout.inc( 1) end),
						   awful.button({ }, 3, function () awful.layout.inc(-1) end),
						   awful.button({ }, 4, function () awful.layout.inc( 1) end),
						   awful.button({ }, 5, function () awful.layout.inc(-1) end)))
	-- Create a tags list widget
	s.mytaglist = widget_tags.widget(s)

	-- Create a tasklist widget
	s.mytasklist = widget_tasks.widget(s)

	-- Create the wibox
	-- TODO: improve like this : https://github.com/awesomeWM/awesome/blob/dd5be865c3d00c580389c38ea41b6719ab567d3e/tests/_wibox_helper.lua
	s.mywibox = awful.wibar({
		position = "top",
		screen = s,
		--height = 25
	})

	-- Widget for main screen only
	if s == screens.get_primary() then
		-- Create a promptbox
		s.promptbox = widget_prompt.widget

		-- Add widgets to the wibox
		s.mywibox:setup {
			layout = wibox.layout.align.horizontal,
			{ -- Left widgets
				layout = wibox.layout.fixed.horizontal,
				s.mytaglist,
				s.promptbox,
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
				s.layoutbox,
			},
		}
	else
		-- secondary screen
		s.mywibox:setup {
			layout = wibox.layout.align.horizontal,
			{ -- Left widgets
				layout = wibox.layout.fixed.horizontal,
				s.mytaglist,
			},
			-- Middle widget
			s.mytasklist,
			{
				-- Right widgets
				layout = wibox.layout.fixed.horizontal,
				widget_separator.widget,
				s.layoutbox,
			},
		}
	end
end)


-- ---------------------------------------------------------------------
-- Keybindings
-- ---------------------------------------------------------------------

local keys = require("keys")

-- Set keys
root.keys(keys.global)


-- ---------------------------------------------------------------------
-- Rules
-- ---------------------------------------------------------------------

-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = require("rules")


-- ---------------------------------------------------------------------
-- Signals
-- ---------------------------------------------------------------------

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", function (s)
	wallpaper.update(s)
end)

-- Re-set wallpaper when a new tag is selected
globaltag.connect_signal("property::selected", function (t)
	desktops.switch(t)
	wallpaper.update()
end)

-- Signal function to execute when a new client appears.
globalclient.connect_signal("manage", function (c)
	-- Set the windows at the slave,
	-- i.e. put it at the end of others instead of setting it master.
	if not awesome.startup then awful.client.setslave(c) end

	if awesome.startup and
	  not c.size_hints.user_position
	  and not c.size_hints.program_position then
		-- Prevent clients from being unreachable after screen count changes.
		awful.placement.no_offscreen(c)
	end

	-- if (c.class == "Firefox") then
	-- 	-- if it's a Firefox we will connect a signal which will call if 'name' changing
	-- 	c:connect_signal("property::name", function(c)
	-- 		if (string.find(c.name, "(Private Browsing)")) then
	-- 			-- if "(Private Browsing)" is part of 'c.name' then 'c' goes to tags[9]
	--			-- Private window do not keep focuse with this method
	-- 			local tags = root.tags()
	-- 			c:tags({tags[9]})
	-- 			tags[9]:view_only()
	-- 		end
	-- 	end)
	-- end
end)

-- -- Try to fix electron signals
-- local awfulrules = require("awful.rules")
-- client.connect_signal("manage", function (c)
-- 	-- Some applications (like Spotify) does not respect ICCCM rules correctly and redefine the window class property.
-- 	-- This leads to having window which does *NOT* follow the user rules defined in the table `awful.rules.rules`.
-- 	c:connect_signal("property::class", awfulrules.apply)
-- 	awfulrules.apply(c)
-- end)
-- client.connect_signal("unmanage", function (c)
-- 	c:disconnect_signal("property::class", awfulrules.apply)
-- end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
globalclient.connect_signal("request::titlebars", function(c)
	-- buttons for the titlebar
	local buttons = awful.util.table.join(
		awful.button({ }, 1, function()
			globalclient.focus = c
			c:raise()
			awful.mouse.client.move(c)
		end),
		awful.button({ }, 3, function()
			globalclient.focus = c
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

-- Border on focused clients
globalclient.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
globalclient.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

-- Force ontop on client fullscreen exit (fullscreen unsets ontop)
globalclient.connect_signal("property::fullscreen", function(c) if not c.fullscreen then c.ontop = true end end)

-- ---------------------------------------------------------------------
-- Auto start
-- ---------------------------------------------------------------------
awful.spawn.with_shell("~/.dotfiles/bin/autostart")
