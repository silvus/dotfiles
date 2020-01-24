-- ---------------------------------------------------------------------
-- Init
-- ---------------------------------------------------------------------

-- init random
-- math.randomseed(os.time());

-- Standard lua
-- local string = require("string")
-- local os = { getenv = os.getenv, setlocale = os.setlocale }

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")

-- Makes sure that there's always a client that will have focus on events such as tag switching, client unmanaging, etc
require("awful.autofocus")

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
-- screen.connect_signal("added", screens.update)
-- screen.connect_signal("removed", screens.update)
screens.init()


-- ---------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------

-- Themes define colours, icons, font and wallpapers.
beautiful.init(gears.filesystem.get_configuration_dir() .. "themes/" .. config.theme .. "/theme.lua")

-- Notifications
naughty.config.defaults.timeout = 30
naughty.config.defaults.screen = screens.get_primary()
naughty.config.defaults.position = beautiful.notification_position
naughty.config.defaults.margin = 5
naughty.config.defaults.gap = 20
naughty.config.defaults.ontop = true
naughty.config.defaults.border_width = 1
naughty.config.defaults.hover_timeout = nil
naughty.config.defaults.fg = beautiful.fg_urgent
naughty.config.defaults.bg = beautiful.bg_focus
naughty.config.defaults.border_color = beautiful.border_focus
naughty.config.notify_callback = function(args)
	-- Set defaut icon
	if not args.icon then
		args.icon = beautiful.arrow_left
	end
	return args
end

naughty.config.presets.low.timeout = 10
naughty.config.presets.critical.bg = beautiful.error
naughty.config.presets.critical.border_color = beautiful.fg_urgent

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = config.layouts


-- ---------------------------------------------------------------------
-- Status bar
-- ---------------------------------------------------------------------

-- Custom status bar (need to be loaded after theme init (or naughty configurations ?))
local statusbar = nil
if beautiful.bar_orientation == "vertical" then
	-- vertical bar if theme defined it
	statusbar = require("bar.vertical")
else
	statusbar = require("bar.horizontal")
end
-- local statusbar_mini = require("bar.mini")

awful.screen.connect_for_each_screen(function(s)
	-- Wallpaper
	wallpaper.update(s)

	-- Tags init
	desktops.init(s)

	-- Bar init
	statusbar.init(s)
	-- statusbar_mini.init(s)
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
