-- ---------------------------------------------------------------------
-- Init
-- ---------------------------------------------------------------------

-- init random
math.randomseed(os.time());

-- Standard lua
-- local string = require("string")
-- local os = { getenv = os.getenv, setlocale = os.setlocale }

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")

-- Dmenu-like launcher
local menubar = require("menubar")

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

-- Init Dashboard
-- local dashboard = require("utils.dashboard")

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
	naughty.notify({
		preset = naughty.config.presets.critical,
		title = "Oops, there were errors during startup!",
		text = awesome.startup_errors
	})
	-- naughty.notify({text = 'notif text' })
end

-- Handle runtime errors after startup
do
	local in_error = false
	awesome.connect_signal("debug::error", function(err)
		-- Make sure we don't go into an endless error loop
		if in_error then return end
		in_error = true

		naughty.notify({
			preset = naughty.config.presets.critical,
			title = "Oops, an error happened!",
			text = tostring(err)
		})
		in_error = false
	end)
end
-- }}}


-- ---------------------------------------------------------------------
-- Screens
-- ---------------------------------------------------------------------
-- Launch specific xrandr script on init if presents (Need to be done before launching Awesome or primary screen can be wrong. Wainting for -m flag to awesome)
screens.init()
-- Restart awesome to update screens count and primary
-- screen.connect_signal("added", awesome.restart)
-- screen.connect_signal("removed", awesome.restart)

screen.connect_signal("primary_changed", function(s)
	-- on primary change, delete and redraw all bars
	for s in screen do
		if s.bar then
			s.bar:remove()
			s.bar = beautiful.bar(s)
		end
	end
end)


-- ---------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------

-- Themes define colours, icons, font and wallpapers.
beautiful.init(gears.filesystem.get_configuration_dir() .. "themes/" .. config.theme .. "/theme.lua")

-- Notifications
naughty.config.padding = 25 -- Space between popups and edge of the workarea
naughty.config.spacing = 2  -- Spacing between popups

-- Set defaut icon
naughty.config.notify_callback = function(args)
	if not args.icon then
		args.icon = beautiful.arrow_left
	end

	if config.blink_path then
		local blink_color = args.bg or beautiful.primary
		awful.util.spawn(config.blink_path .. " --on --rgb '" .. blink_color .. "' --blink 10", false)
	end
	return args
end

-- Connect a signal to be executed when the notification is destroyed
-- naughty.connect_signal("destroyed", function()
-- 	if config.blink_path then
-- 		awful.util.spawn(config.blink_path .. " --off", false)
-- 	end
-- end)

-- Default
naughty.config.defaults.timeout = 60
naughty.config.defaults.screen = screens.get_primary()
naughty.config.defaults.position = beautiful.notification_position
naughty.config.defaults.margin = 5 -- Space inside popup
naughty.config.defaults.ontop = true
naughty.config.defaults.border_width = 1
naughty.config.defaults.hover_timeout = nil
naughty.config.defaults.fg = beautiful.fg_urgent
naughty.config.defaults.bg = beautiful.bg_focus
naughty.config.defaults.border_color = beautiful.border_focus
-- Low
naughty.config.presets.low.fg = beautiful.fg_urgent
naughty.config.presets.low.bg = "#333333"
naughty.config.presets.low.border_color = beautiful.bg_normal
naughty.config.presets.low.timeout = 3
-- Critical
naughty.config.presets.critical.fg = beautiful.fg_urgent
naughty.config.presets.critical.bg = beautiful.error
naughty.config.presets.critical.border_color = beautiful.fg_urgent

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = config.layouts

-- Menubar configuration : Set the terminal for applications that require it
menubar.utils.terminal = config.terminal

-- ---------------------------------------------------------------------
-- Status bar
-- ---------------------------------------------------------------------
awful.screen.connect_for_each_screen(function(s)
	-- Wallpaper
	wallpaper.update(s)

	-- Tags init
	desktops.init(s)

	-- Bar init (keeped in screen to remove on events)
	s.bar = beautiful.bar(s)
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
screen.connect_signal("property::geometry", function(s)
	wallpaper.update(s)
end)

-- Remove border on solo client
screen.connect_signal("arrange", function(s)
	if s.selected_tag then
		local max = s.selected_tag.layout.name == "max"
		local only_one = #s.tiled_clients == 1 -- use tiled_clients so that other floating windows don't affect the count
		-- but iterate over clients instead of tiled_clients as tiled_clients doesn't include maximized windows
		for _, c in pairs(s.clients) do
			-- Exception for guaketerm (present everywhere), Firefox PictureInPicture (broken)
			if c.instance ~= "guaketerm" and c.role ~= "PictureInPicture" and not c.fullscreen then
				if (max or only_one) and not c.floating or c.maximized then
					c.border_width = 0
				else
					c.border_width = beautiful.border_width
				end
			end
		end
	end
end)

-- Re-set wallpaper when a new tag is selected (if config option on)
if config.wallpapers_by_tag then
	globaltag.connect_signal("property::selected", function(t)
		wallpaper.update()
	end)
end

-- Signal function to execute when a new client appears.
globalclient.connect_signal("manage", function(c)
	-- Set the windows at the slave,
	-- i.e. put it at the end of others instead of setting it master.
	if not awesome.startup then awful.client.setslave(c) end

	if awesome.startup and
		not c.size_hints.user_position
		and not c.size_hints.program_position then
		-- Prevent clients from being unreachable after screen count changes.
		awful.placement.no_offscreen(c)
	end
end)

-- Border on focused clients
globalclient.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
globalclient.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
globalclient.connect_signal("request::titlebars", titlebars.setup_titlebar)

-- Force ontop on client fullscreen exit (fullscreen unsets ontop)
globalclient.connect_signal("property::fullscreen", function(c) if not c.fullscreen then c.ontop = true end end)

-- ---------------------------------------------------------------------
-- Auto start
-- ---------------------------------------------------------------------
awful.spawn.with_shell(config.home .. "/.dotfiles/bin/autostart_launcher")
