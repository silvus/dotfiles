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

-- Theme handling library
local beautiful = require("beautiful")

-- Notification library
local naughty = require("naughty")

-- Base custom config
local config = require('config')

-- Tags declarations
local desktops = require('desktops')

-- Screen custom definitions
local screens = require('screens')

-- Wallpapers utilities
local wallpaper = require("utils.wallpaper")

-- To build client titlebars
local titlebars = require("titlebars")

-- Set a global variable, a local one
local globalscreen = screen
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
local widget_layout = require("widgets.layout")
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

	-- Create an imagebox widget which will contains an icon indicating which layout we're using. One layoutbox per screen.
	s.layoutbox = widget_layout.widget(s)
	
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
				layout = wibox.layout.fixed.horizontal,
				s.mytasklist,
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
			{ -- Middle widget
				layout = wibox.layout.fixed.horizontal,
				s.mytasklist,
			},
			{ -- Right widgets
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

-- Add a titlebar if titlebars_enabled is set to true in the rules.
globalclient.connect_signal("request::titlebars", function(c)
	titlebars.setup_titlebars(c)
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
