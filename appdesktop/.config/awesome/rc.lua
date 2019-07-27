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

-- Menubar (dmenu like)
local menubar = require("menubar")

-- Contrib utilities
local lain	= require("lain")


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

local function debug_log(text)
	naughty.notify({
		title = 'Debug',
		text = text,
		ontop = true,
		preset = naughty.config.presets.critical
	})
	-- local log = io.open('/tmp/awesomewm_debug.log', 'aw')
	-- log:write(text)
	-- log:flush()
	-- log:close()
end


-- ---------------------------------------------------------------------
-- Screens
-- ---------------------------------------------------------------------
local screens = require('screens')
screen.connect_signal("added", screens.update)
screen.connect_signal("removed", screens.update)
screens.update()

-- Set focused tag to the one with a mouse or an active client ?
-- awful.screen.default_focused_args = {client = true, mouse = true}


-- ---------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
-- beautiful.init(awful.util.get_themes_dir() .. "default/theme.lua")
beautiful.init("~/.config/awesome/themes/custom/theme.lua")

-- This is used later as the default terminal and editor to run.
-- terminal = "x-terminal-emulator"
-- terminal = "urxvt"
terminal = "rxvt-unicode"
-- editor = os.getenv("EDITOR") or "editor"
-- editor_cmd = terminal .. " -e " .. editor

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

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

-- Tags names
awful.util.tagnames = { "1", "2", "3", "4", "5", "6", "7", "8", "9" }
-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
	awful.layout.suit.tile,
	awful.layout.suit.tile.left,
	awful.layout.suit.tile.bottom,
	awful.layout.suit.tile.top,
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


-- ---------------------------------------------------------------------
-- Wallpaper
-- ---------------------------------------------------------------------
local wallpaper = require("wallpaper")
-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", wallpaper.update)


-- ---------------------------------------------------------------------
-- Status bar
-- ---------------------------------------------------------------------

-- {{{ Wibar

local widgets = require("widgets")

-- Create a wibox for each screen and add it
local taglist_buttons = awful.util.table.join(
	awful.button({ }, 1, function(t)
		t:view_only()
		wallpaper.update(t.screen)
	end),
	awful.button({ modkey }, 1, function(t)
		if client.focus then
			client.focus:move_to_tag(t)
		end
	end),
	awful.button({ }, 3, function(t)
		awful.tag.viewtoggle(t)
	end),
	awful.button({ modkey }, 3, function(t)
		if client.focus then
			client.focus:toggle_tag(t)
		end
	end),
	awful.button({ }, 4, function(t)
		awful.tag.viewnext(t.screen)
		wallpaper.update(t.screen)
	end),
	awful.button({ }, 5, function(t)
		awful.tag.viewprev(t.screen)
		wallpaper.update(t.screen)
	end)
)

-- Apps list actions
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
		end)
	-- awful.button({ }, 3, client_menu_toggle_fn())
	-- awful.button({ }, 4, function ()
	--						  awful.client.focus.byidx(1)
	--					  end),
	-- awful.button({ }, 5, function ()
	--						  awful.client.focus.byidx(-1)
	--					  end))
)

-- Filter used by tags widgets
function taglist_filter(t)
	-- No empty and not the scratchpad (except if selected)
	return (#t:clients() > 0 or t.selected) and (t.name ~= "S" or t.selected)
end

awful.screen.connect_for_each_screen(function(s)
	-- Wallpaper
	wallpaper.update(s)

	-- Each screen has its own tag table.
	-- local names = { "main", "www", "skype", "gimp", "office", "im", "7", "8", "9" }
	-- local l = awful.layout.suit  -- Just to save some typing: use an alias.
	-- local layouts = { l.floating, l.tile, l.floating, l.fair, l.max, l.floating, l.tile.left, l.floating, l.floating }
	-- awful.tag(names, s, layouts)

	if s == screens.get_primary() then
		-- Tag 0 is a Scratchpad !
		-- Scratchpad is a special tag, filtered from widget and bind to a key
		-- Better than "lain.guake" to manage multi windows apps
		awful.tag(awful.util.tagnames, s, awful.layout.suit.tile)
		-- Mail tag
		awful.tag.add("M", {
			icon = beautiful.mail,
			layout = awful.layout.suit.max,
			screen = s,
			icon_only = true,
		})
		-- Scratchpad
		awful.tag.add("S", {
			icon = beautiful.code,
			layout = awful.layout.suit.max,
			screen = s,
			icon_only = true,
			-- 	selected = true,
		})
	elseif s.geometry.height > s.geometry.width then
		-- vertical screen with adapted layout
		awful.tag(awful.util.tagnames, s, awful.layout.suit.tile.bottom)
	else
		-- secondary screens
		awful.tag(awful.util.tagnames, s, awful.layout.suit.tile)
	end

	-- Create an imagebox widget which will contains an icon indicating which layout we're using.
	-- We need one layoutbox per screen.
	s.layoutbox = awful.widget.layoutbox(s)
	s.layoutbox:buttons(awful.util.table.join(
						   awful.button({ }, 1, function () awful.layout.inc( 1) end),
						   awful.button({ }, 3, function () awful.layout.inc(-1) end),
						   awful.button({ }, 4, function () awful.layout.inc( 1) end),
						   awful.button({ }, 5, function () awful.layout.inc(-1) end)))
	-- Create a taglist widget
	s.mytaglist = awful.widget.taglist(s, taglist_filter, taglist_buttons)
	-- s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)

	-- Create a tasklist widget
	-- s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, tasklist_buttons)
	-- Create a tasklist widget with a max width
	s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, tasklist_buttons, nil, function(w, buttons, label, data, objects)
		widget_common.list_update(w, buttons, label, data, objects)
		w:set_max_widget_size(300)
	end, wibox.layout.flex.horizontal())

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
		s.promptbox = widgets.promptbox

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
				widgets.musicicon,
				widgets.mocbarwidget,
				widgets.mocwidget,
				widgets.spaceseparator,
				widgets.vpnicon,
				widgets.vpn,
				widgets.neticon,
				widgets.netwidget,
				widgets.cpuicon,
				widgets.cpuwidget,
				widgets.memicon,
				widgets.memwidget,
				widgets.baticon,
				widgets.batwidget,
				widgets.volicon,
				widgets.volumewidget,
				widgets.spaceseparator,
				widgets.systraykeyboardlayout,
				widgets.systray,
				widgets.spaceseparator,
				widgets.spaceseparator,
				widgets.clockicon,
				widgets.spaceseparator,
				widgets.textclock,
				widgets.spaceseparator,
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
				widgets.spaceseparator,
				s.layoutbox,
			},
		}
	end
end)
-- }}}

-- ---------------------------------------------------------------------
-- Keybindings
-- ---------------------------------------------------------------------
-- {{{ Keybindings
local keybindings = require("keybindings")

-- Set keys
root.keys(keybindings.globalkeys)
-- }}}

-- ---------------------------------------------------------------------
-- Rules
-- ---------------------------------------------------------------------

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = require("rules")
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

	-- if (c.class == "Firefox") then
	-- 	-- if it's a Firefox we will connect a signal which will call if 'name' changing
	-- 	c:connect_signal("property::name", function(c)
	-- 		if (string.find(c.name, "(Private Browsing)")) then
	-- 			-- if "(Private Browsing)" is part of 'c.name' then 'c' goes to tags[1][9]
	-- 			local tags = root.tags()
	-- 			c:tags({tags[9]})
	-- 		end
	-- 	end)
	-- end

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

-- Border on focused clients
client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)


-- ---------------------------------------------------------------------
-- Auto start
-- ---------------------------------------------------------------------
awful.spawn.with_shell("~/.dotfiles/bin/autostart")
