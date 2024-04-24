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
-- Launch specific xrandr script on init if presents (Need to be done before launching Awesome or primary screen can be wrong. Wainting for -m flag to awesome)
screens.init()
-- Restart awesome to update screens count and primary
-- screen.connect_signal("added", awesome.restart)
-- screen.connect_signal("removed", awesome.restart)

screen.connect_signal("primary_changed", function (s)
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
naughty.config.spacing = 2 -- Spacing between popups

-- Set defaut icon
naughty.config.notify_callback = function(args)
	if not args.icon then
		args.icon = beautiful.arrow_left
	end

	if config.blink_path then
		local blink_color = args.bg or beautiful.primary
		awful.util.spawn(config.blink_path .. " --on '" .. blink_color .. "'", false)
	end
	return args
end

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

-- ---------------------------------------------------------------------
-- Status bar
-- ---------------------------------------------------------------------
awful.screen.connect_for_each_screen(function(s)
	-- Wallpaper
	wallpaper.update(s)

	-- Tags init
	-- desktops.init(s)

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


local tyrannical = require("tyrannical")

tyrannical.settings.block_children_focus_stealing = true --Block popups ()
tyrannical.settings.group_children = true --Force popups/dialogs to have the same tags as the parent client
tyrannical.settings.favor_focused = false -- Prefer the focused screen to the screen property
tyrannical.settings.master_width_factor = 0.66  -- Tiled layout master/slave ratio
tyrannical.settings.default_layout = awful.layout.suit.tile

tyrannical.tags = {
	-- {
	-- name        = "Term",                 -- Call the tag "Term"
	-- init        = true,                   -- Load the tag on startup
	-- init        = false, -- This tag wont be created at startup, but will be when one of the
	-- client in the "class" section will start. It will be created on the client startup screen
	-- exclusive   = true,                   -- Refuse any other type of clients (by classes)
	-- fallback    = true,
	-- volatile    = false,
	-- screen      = {1,2},                  -- Create this tag on screen 1 and screen 2
	-- screen      = screen.count()>1 and 2 or 1,-- Setup on screen 2 if there is more than 1 screen, else on screen 1
	-- icon        = "~net.png",                 -- Use this icon for the tag (uncomment with a real path)
	-- layout      = awful.layout.suit.tile, -- Use the tile layout
	-- exec_once   = { --When the tag is accessed for the first time, execute this command
	-- 		"obsidian"
	-- 	},
	-- instance    = {"dev", "ops"},         -- Accept the following instances. This takes precedence over 'class'
	-- class       = { --Accept the following classes, refuse everything else (because of "exclusive=true")
	-- 		"xterm" , "urxvt" , "aterm","URxvt","XTerm","konsole","terminator","gnome-terminal"
	-- }
	-- },
	{
		name        = "1", -- "Browser",
		icon        = beautiful.firefox,
		init        = true,
		exclusive   = true,
		selected    = true,
		no_focus_stealing_in = true,
		properties  = {
			titlebars_enabled = false
		},
		class       = {
			"firefox",
			"Firefox",
			"Chromium",
			"nightly",
		}
	},
	{
		name        = "2", -- "Notes",
		icon        = beautiful.note,
		init        = true,
		exclusive   = true,
		no_focus_stealing_in = true,
		properties  = {
			titlebars_enabled = false
		},
		class       = {
			"obsidian",
			"Zim",
			"VNote",
		}
	},
	{
		name        = "3", -- "Develop",
		icon        = beautiful.code,
		init        = true,
		exclusive   = true,
		no_focus_stealing_in = true,
		class       = {
			"VSCodium",
			"jetbrains-phpstorm",
			"Kate",
			"KDevelop",
			"Codeblocks",
			"Code::Blocks",
			"kate4",
		}
	},
	{
		name        = "4", -- "Files",
		icon        = beautiful.folder,
		init        = true,
		exclusive   = true,
		no_focus_stealing_in = true,
		class       = {
			"Pcmanfm",
			"pcmanfm-qt",
			"Thunar",
			"Nemo",
			"Nautilus",
			"Konqueror",
			"Dolphin",
		}
	},
	{
		name        = "5", -- "Mail",
		icon        = beautiful.mail,
		init        = true,
		exclusive   = true,
		no_focus_stealing_in = true,
		instance    = {
			"Msgcompose"
		},
		class       = {
			"thunderbird",
			"Thunderbird",
		}
	},
	{
		name        = "6", -- "Games",
		icon        = beautiful.fire,
		init        = true,
		exclusive   = true,
		no_focus_stealing_in = true,
		class       = {
			"steam",
			"Steam",
			"discord",
		}
	},
	{
		name        = "7", -- "Office and Graphics",
		icon        = beautiful.paragraph,
		init        = true,
		exclusive   = true,
		no_focus_stealing_in = true,
		instance    = {
			"libreoffice",
		},
		class       = {
			"libreoffice-startcenter",
			"libreoffice-writer",
			"libreoffice-calc",
			"libreoffice-impress",
			"Simple-scan",
			"Gnumeric",
			"Gimp",
			"Krita",
		}
	},
	{
		name        = "8", -- "Other",
		icon        = beautiful.fire,
		init        = true,
		exclusive   = false,
		no_focus_stealing_in = true,
		fallback    = true,
	},
	{
		name        = "9", -- "Privacy",
		icon        = beautiful.lock,
		init        = true,
		exclusive   = true,
		no_focus_stealing_in = true,
		class       = {
			"Tor Browser",
		}
	},
}

-- Doesn't work, apply_properties() removed here https://github.com/Elv13/tyrannical/commit/38f1f6087990d534d9ccfa02c1e5e684859fd38e
-- Ignore the tag "exclusive" property for the following clients (matched by classes)
-- tyrannical.properties.intrusive = {
-- 	"org.wezfurlong.wezterm", "Xfce4-terminal",
-- 	"ksnapshot"     , "pinentry"       , "gtksu"     , "kcalc"        , "xcalc"               ,
-- 	"feh"           , "Gradient editor", "About KDE" , "Paste Special", "Background color"    ,
-- 	"kcolorchooser" , "plasmoidviewer" , "Xephyr"    , "kruler"       , "plasmaengineexplorer",
-- }

-- -- Ignore the tiled layout for the matching clients
-- tyrannical.properties.floating = {
-- 	-- "org.wezfurlong.wezterm", "Xfce4-terminal",
-- 	"MPlayer"      , "pinentry"        , "ksnapshot"  , "pinentry"     , "gtksu"          ,
-- 	"xine"         , "feh"             , "kmix"       , "kcalc"        , "xcalc"          ,
-- 	"yakuake"      , "Select Color$"   , "kruler"     , "kcolorchooser", "Paste Special"  ,
-- 	"New Form"     , "Insert Picture"  , "kcharselect", "mythfrontend" , "plasmoidviewer" ,
-- }

-- Make the matching clients (by classes) on top of the default layout
-- tyrannical.properties.ontop = {
-- 	"Xephyr"       , "ksnapshot"       , "kruler",
-- 	"guaketerm",
-- }

-- Force the matching clients (by classes) to be centered on the screen on init
-- tyrannical.properties.placement = {
--     kcalc = awful.placement.centered
-- }


-- ---------------------------------------------------------------------
-- Signals
-- ---------------------------------------------------------------------

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", function (s)
	wallpaper.update(s)
end)

-- Re-set wallpaper when a new tag is selected (if config option on)
if config.wallpapers_by_tag then
	globaltag.connect_signal("property::selected", function (t)
		wallpaper.update()
	end)
end

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
end)

-- Border on focused clients
globalclient.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
globalclient.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

-- Remove border when only one window
local function set_border(c)
	local s = awful.screen.focused()

	local max = s.selected_tag.layout == awful.layout.suit.max
	local only_one = (#s.clients == 1)

	for _, c in pairs(s.clients) do
		if max or only_one or c.maximized then
			c.border_width = 0
		else
			c.border_width = beautiful.border_width
		end

		c.border_width = beautiful.border_width

	end
end
client.connect_signal("focus", set_border)
client.connect_signal("tagged", set_border)
client.connect_signal("request::tag", set_border)
client.connect_signal("request::border", set_border)
client.connect_signal("property::maximized", set_border)
client.connect_signal("manage", set_border)

-- Remove titlebar when only one window
-- Creates titlebar if it doesn't exist
local function setTitlebar(c)
	local s = awful.screen.focused()
	local max = s.selected_tag.layout == awful.layout.suit.max
	local only_one = (#s.clients == 1)
	-- Iterate over clients instead of tiled_clients as tiled_clients doesn't include maximized windows
	for _, c in pairs(s.clients) do
		if max or only_one or c.maximized then
			awful.titlebar.hide(c)
		else
			if c.titlebar == nil then
				c:emit_signal("request::titlebars", "rules", {})
			end
			awful.titlebar.show(c)
		end
	end
end
-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", titlebars.setup_titlebars)
screen.connect_signal("arrange", setTitlebar)


-- Force ontop on client fullscreen exit (fullscreen unsets ontop)
globalclient.connect_signal("property::fullscreen", function(c) if not c.fullscreen then c.ontop = true end end)

-- ---------------------------------------------------------------------
-- Auto start
-- ---------------------------------------------------------------------
awful.spawn.with_shell(config.home .. "/.dotfiles/bin/autostart_launcher")
