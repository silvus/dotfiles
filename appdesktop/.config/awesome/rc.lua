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

-- Used in bindings
local hotkeys_popup = require("awful.hotkeys_popup").widget

-- Menubar (dmenu like)
local menubar = require("menubar")

-- Contrib utilities
local lain	= require("lain")

-- Quake like terminal (single instance for all screens)
local quake = require("quake")

-- Tags declarations
local desktops = require('desktops')

-- Screen custom definitions
local screens = require('screens')

-- Wallpapers utilities
local wallpaper = require("wallpaper")

-- Set a global variable, a local one
local globaltag = tag


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

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
	awful.layout.suit.tile,
	-- awful.layout.suit.tile.left,
	awful.layout.suit.tile.bottom,
	-- awful.layout.suit.tile.top,
	-- awful.layout.suit.spiral,
	-- awful.layout.suit.spiral.dwindle,
	awful.layout.suit.fair,
	-- awful.layout.suit.fair.horizontal,
	awful.layout.suit.max,
	-- awful.layout.suit.max.fullscreen,
	-- awful.layout.suit.magnifier,
	-- awful.layout.suit.floating,
	-- awful.layout.suit.corner.nw,
	-- awful.layout.suit.corner.ne,
	-- awful.layout.suit.corner.sw,
	-- awful.layout.suit.corner.se,
}
-- }}}


-- ---------------------------------------------------------------------
-- Status bar
-- ---------------------------------------------------------------------

-- Create a wibox for each screen and add it
local taglist_buttons = awful.util.table.join(
	awful.button({ }, 1, function(t)
		t:view_only()
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
	end),
	awful.button({ }, 5, function(t)
		awful.tag.viewprev(t.screen)
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

-- Customs widgets definitions (need to be loaded after naughty configurations)
local widgets_custom = require("widgets")

-- Filter used by tags widgets
function taglist_filter(t)
	-- No empty and not the scratchpad (except if selected)
	return (#t:clients() > 0 or t.selected) and (t.name ~= "0" or t.selected)
end

awful.screen.connect_for_each_screen(function(s)
	-- Wallpaper
	wallpaper.update(s)

	-- Each screen has its own tag table.
	-- local names = { "main", "www", "skype", "gimp", "office", "im", "7", "8", "9" }
	-- local l = awful.layout.suit  -- Just to save some typing: use an alias.
	-- local layouts = { l.floating, l.tile, l.floating, l.fair, l.max, l.floating, l.tile.left, l.floating, l.floating }
	-- awful.tag(names, s, layouts)

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
	-- Create a taglist widget (https://awesomewm.org/doc/api/classes/awful.widget.taglist.html)
	if s == screens.get_primary() then
		s.mytaglist = awful.widget.taglist({
			screen  = s,
			filter  = taglist_filter,
			buttons = taglist_buttons,
			widget_template = {
				{
					{
						{
							{
								id = 'icon_role',
								widget = wibox.widget.imagebox,
								resize = true,
								forced_height = 28,
								forced_width = 28,
							},
							layout = wibox.container.margin,
							top = 2,
							right = 2,
							bottom= 2,
							left = 8,
						},
						{
							{
								{
									id = 'text_role',
									widget = wibox.widget.textbox,
									valign = 'center',
									align = 'center',
								},
								bg = '#000000',
								fg = '#ffffff',
								-- forced_height = 12,
								-- forced_width = 8,
								opacity = 0.6,
								widget = wibox.container.background,
							},
							widget = wibox.container.place,
							valign = 'bottom',
							halign = 'left',
						},
						layout = wibox.layout.stack,
					},
					-- left  = 1,
					-- right = 1,
					widget = wibox.container.margin
				},
				id     = 'background_role',
				widget = wibox.container.background,
				forced_height = 32,
				forced_width = 32,
			},
		})
	else
		-- secondary screens
		s.mytaglist = awful.widget.taglist(s, taglist_filter, taglist_buttons)
	end

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
		s.promptbox = widgets_custom.promptbox

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
				widgets_custom.musicicon,
				widgets_custom.mocbarwidget,
				widgets_custom.mocwidget,
				widgets_custom.spaceseparator,
				widgets_custom.vpnicon,
				widgets_custom.vpn,
				widgets_custom.neticon,
				widgets_custom.netwidget,
				widgets_custom.cpuicon,
				widgets_custom.cpuwidget,
				widgets_custom.memicon,
				widgets_custom.memwidget,
				widgets_custom.baticon,
				widgets_custom.batwidget,
				widgets_custom.volicon,
				widgets_custom.volumewidget,
				widgets_custom.spaceseparator,
				widgets_custom.systraykeyboardlayout,
				widgets_custom.systray,
				widgets_custom.spaceseparator,
				widgets_custom.spaceseparator,
				widgets_custom.clockicon,
				widgets_custom.spaceseparator,
				widgets_custom.textclock,
				widgets_custom.spaceseparator,
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
				widgets_custom.spaceseparator,
				s.layoutbox,
			},
		}
	end
end)
-- }}}

-- ---------------------------------------------------------------------
-- Keybindings
-- ---------------------------------------------------------------------

-- Used to launch programms on tag fist navigation
local is_launched_editor = false

-- Global keys
-- ----------------------------------------------------------------------------
local globalkeys = awful.util.table.join(
	awful.key({ modkey, }, "h", hotkeys_popup.show_help, {description="show help", group="awesome"}),

	-- Next/previous tag
	awful.key({ modkey, }, "Page_Up",   awful.tag.viewprev, {description = "view previous", group = "tag"}),
	awful.key({ modkey, }, "Page_Down", awful.tag.viewnext, {description = "view next", group = "tag"}),
	-- Go back to previous tag
	awful.key({ modkey, }, "Tab", awful.tag.history.restore, {description = "go back", group = "tag"}),

	-- awful.key({ modkey, }, "Right", function ()
	--		 awful.client.focus.byidx(1)
	-- 		end, {description = "focus next by index", group = "client"}
	-- ),
	-- awful.key({ modkey, }, "Left", function ()
	--		 awful.gclient.focus.byidx(-1)
	--	 end, {description = "focus previous by index", group = "client"}
	-- ),
	-- awful.key({ modkey,		   }, "w", function()
	--		mymainmenu:show()
	--	end, {description = "show main menu", group = "awesome"}),

	-- On the fly useless gaps change
	-- awful.key({ altkey, "Control" }, "+", function () lain.util.useless_gaps_resize(1) end),
	-- awful.key({ altkey, "Control" }, "-", function () lain.util.useless_gaps_resize(-1) end),

	 -- By direction client focus
	awful.key({ modkey }, "Down", function()
			awful.client.focus.global_bydirection("down")
			if client.focus then client.focus:raise() end
		end, {description = "change client focus", group = "client"}),
	awful.key({ modkey }, "Up", function()
			awful.client.focus.global_bydirection("up")
			if client.focus then client.focus:raise() end
		end, {description = "change client focus", group = "client"}),
	awful.key({ modkey }, "Left", function()
			awful.client.focus.global_bydirection("left")
			if client.focus then client.focus:raise() end
		end, {description = "change client focus", group = "client"}),
	awful.key({ modkey }, "Right", function()
			awful.client.focus.global_bydirection("right")
			if client.focus then client.focus:raise() end
		end, {description = "change client focus", group = "client"}),

	-- Layout manipulation
	awful.key({ modkey,	"Control" }, "space", function()
			awful.layout.inc(1)
		end, {description = "select next", group = "layout"}),
	awful.key({ modkey, "Shift" }, "space", function()
			awful.layout.inc(-1)
		end, {description = "select previous", group = "layout"}),
	awful.key({ modkey, "Shift" }, "Right", function()
			awful.client.swap.global_bydirection('right')
		end, {description = "swap with next client by index", group = "client"}),
	awful.key({ modkey, "Shift" }, "Left", function()
			awful.client.swap.global_bydirection('left')
		end, {description = "swap with previous client by index", group = "client"}),
	awful.key({ modkey, "Shift" }, "Up", function()
			awful.client.swap.global_bydirection('up')
		end, {description = "swap with next client by index", group = "client"}),
	awful.key({ modkey, "Shift" }, "Down", function()
			awful.client.swap.global_bydirection('down')
		end, {description = "swap with previous client by index", group = "client"}),
	awful.key({ modkey, "Control" }, "Right", function()
			awful.screen.focus_relative(1)
		end, {description = "focus the next screen", group = "screen"}),
	awful.key({ modkey, "Control" }, "Left", function()
			awful.screen.focus_relative(-1)
		end, {description = "focus the previous screen", group = "screen"}),
	awful.key({ modkey, }, "u", function()
			awful.client.urgent.jumpto()
		end, {description = "jump to urgent client", group = "client"}),
	awful.key({ modkey,	}, "Tab", function()
			awful.client.focus.history.previous()
			if client.focus then
				client.focus:raise()
			end
		end, {description = "go back", group = "client"}),

	-- Terminal
	awful.key({ modkey, }, "Return", function()
			awful.spawn("rxvt-unicode -title terminal -e " .. os.getenv("HOME") .. "/.dotfiles/bin/tmuxdev")
		end, {description = "open a terminal", group = "launcher"}),
	-- awful.key({}, "²", function () awful.spawn(os.getenv("HOME") .. "/.dotfiles/bin/guakify 'rxvt-unicode.URxvt' '" .. terminal .. " -e " .. os.getenv("HOME") .. "/.dotfiles/bin/tmuxdev'") end, {description = "open a terminal", group = "launcher"}),

	-- Prompt
	awful.key({ modkey, }, "x", function()
			screens.get_primary().promptbox:run()
		end, {description = "run prompt", group = "launcher"}),

	-- Menubar
	awful.key({ modkey, }, "d", function()
			menubar.show(screens.get_primary())
		end, {description = "show the menubar", group = "launcher"}),
	awful.key({ modkey, "Shift" }, "d", function()
			menubar.refresh()
		end, {description = "refresh the menubar", group = "launcher"}),

	-- Clients menu
	-- awful.key({ modkey,	}, "e", function()
	-- 		-- "coords" doesn't work: https://github.com/awesomeWM/awesome/issues/2349
	-- 		-- Default to mouse.coords()
	-- 		-- awful.menu.clients({theme = { width = 500 }}, { keygrabber=true, coords={x=525, y=330} })
	-- 		awful.menu.clients({theme = { width = 500} })
	-- 	end, {description="client menu", group="launcher"}),

	-- Rofi
	awful.key({ modkey,	}, "e", function()
		-- "coords" doesn't work: https://github.com/awesomeWM/awesome/issues/2349
		-- Default to mouse.coords()
		-- awful.menu.clients({theme = { width = 500 }}, { keygrabber=true, coords={x=525, y=330} })
			awful.util.spawn("rofi -show drun")
		end, {description="Rofi launch", group="launcher"}),

	-- Quake-like terminal (²)
	awful.key({}, "#49", function ()
			quake.term:toggle()
		end, {description = "Toggle guake like terminal", group = "launcher"}),

	-- Volume Keys
	awful.key({}, "XF86AudioLowerVolume", function ()
			awful.util.spawn("amixer -q sset Master 5%-", false)
			-- trigger widget update
			widgets_custom.volume.update()
		end, {description = "Volume UP", group = "audio"}),
	awful.key({}, "XF86AudioRaiseVolume", function ()
			awful.util.spawn("amixer -q sset Master 5%+", false)
			widgets_custom.volume.update()
		end, {description = "Volume down", group = "audio"}),
	awful.key({}, "XF86AudioMute", function ()
			awful.util.spawn("amixer set Master 1+ toggle", false)
			widgets_custom.volume.update()
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

	-- Print screen
	awful.key({}, "Print", function()
			awful.util.spawn("ksnip", false)
		end, {description = "make a printscreen with ksnip", group = "launcher"}),

	-- Reload
	awful.key({ modkey, "Shift" }, "r", awesome.restart, {description = "reload awesome", group = "awesome"}),
	-- awful.key({ modkey, "Shift" }, "s", awesome.quit, {description = "quit awesome", group = "awesome"}),

	-- Lock
	awful.key({ modkey, "Shift" }, "l", function()
			awful.util.spawn("i3lock --color 001905 --show-failed-attempts --ignore-empty-password", false)
		end, {description = "lock screen", group = "launcher"}),

	-- Shutdown or restart
	awful.key({ modkey, "Shift" }, "s", function()
			awful.util.spawn(os.getenv("HOME") .. "/.dotfiles/bin/dmenu_shutdown", false)
		end, {description = "shutdown", group = "launcher"}),

	-- VPN
	awful.key({ modkey, "Shift" }, "v", function()
			awful.util.spawn(os.getenv("HOME") .. "/.dotfiles/bin/dmenu_vpn", false)
		end, {description = "launch vpn", group = "launcher"}),

	-- Toggle scratchpad tag (²)
	awful.key({ modkey }, "#49", function ()
			local screen = screens.get_primary()
			local tag_next = screen.tags[10]
			local tag_current = awful.screen.focused().selected_tag
			if tag_next then
				if tag_next == tag_current then
					awful.tag.history.restore()
				else
					tag_next:view_only()
				end
			end
		end, {description = "toggle scratchpad tag", group = "tag"}),
	-- Toggle client to scratchpad tag
	awful.key({ modkey, "Shift" }, "#49", function ()
			if client.focus then
				local screen = awful.screen.focused()
				local tag_next = screen.tags[10]
				if tag_next then
					client.focus:move_to_tag(tag_next)
					tag_next:view_only()
				end
			end
		end, {description = "move focused client to scratchpad", group = "tag"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
-- for i = 1, 9 do
for i, v in pairs(desktops.tags_names) do
	globalkeys = awful.util.table.join(globalkeys,
		-- View tag only.
		awful.key({ modkey }, "#" .. i + 9, function ()
				local screen = awful.screen.focused()
				local tag_next = screen.tags[i]
				local tag_current = awful.screen.focused().selected_tag

				if tag_next then
					if tag_next == tag_current then
						-- If already on focused screen, go to previous one
						awful.tag.history.restore()
					else
						-- Just go to the screen
						tag_next:view_only()
					end
				end
			end, {description = "view tag", group = "tag"}),
		-- Toggle tag display.
		awful.key({ modkey, "Control" }, "#" .. i + 9, function ()
				local screen = awful.screen.focused()
				local tag_next = screen.tags[i]
				if tag_next then
					awful.tag.viewtoggle(tag_next)
				end
			end, {description = "toggle tag", group = "tag"}),
		-- Move client to tag.
		awful.key({ modkey, "Shift" }, "#" .. i + 9, function ()
				if client.focus then
					local screen = awful.screen.focused()
					local tag_next = client.focus.screen.tags[i]
					if tag_next then
						client.focus:move_to_tag(tag_next)
						tag_next:view_only()
					end
				end
			end, {description = "move focused client to tag", group = "tag"}),
		-- Toggle tag on focused client.
		awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9, function ()
				if client.focus then
					local tag_next = client.focus.screen.tags[i]
					if tag_next then
						client.focus:toggle_tag(tag_next)
					end
				end
			end, {description = "toggle focused client on tag", group = "tag"})
	)
end
-- Set keys
root.keys(globalkeys)

-- Client keys
-- ----------------------------------------------------------------------------
local clientkeys = awful.util.table.join(
	awful.key({ modkey, }, "f", function(c)
			c.fullscreen = not c.fullscreen
			c:raise()
		end, {description = "toggle fullscreen", group = "client"}),
	awful.key({ modkey, }, "c", function(c)
		   -- toggle titlebar
		   awful.titlebar.toggle(c)
	   end, {description = "toggle titlebar", group = "client"}),
	awful.key({ modkey, "Shift" }, "q", function(c)
			c:kill()
		end, {description = "close", group = "client"}),
	awful.key({ modkey, }, "space", function(c)
			awful.client.floating.toggle()
		end, {description = "toggle floating", group = "client"}),
	awful.key({ modkey, "Control" }, "Return", function(c)
			c:swap(awful.client.getmaster())
		end, {description = "move to master", group = "client"}),
	awful.key({ modkey, }, "o", function(c)
			c:move_to_screen()
		end, {description = "move to screen", group = "client"}),
	awful.key({ modkey,	}, "t", function(c)
			c.ontop = not c.ontop
		end, {description = "toggle keep on top", group = "client"}),
	awful.key({ modkey, }, "n", function(c)
			-- The client currently has the input focus, so it cannot be
			-- minimized, since minimized clients can't have the focus.
			c.minimized = true
		end, {description = "minimize", group = "client"}),
	awful.key({ modkey, }, "z", function(c)
			c.maximized = not c.maximized
			c:raise()
		end, {description = "maximize", group = "client"}),
	awful.key({ modkey, }, "m", function(c)
			c.maximized = not c.maximized
			c:raise()
		end, {description = "maximize", group = "client"})
)

-- Client button
-- ----------------------------------------------------------------------------
local clientbuttons = awful.util.table.join(
	awful.button({ }, 1, function(c) client.focus = c; c:raise() end),
	awful.button({ modkey }, 1, awful.mouse.client.move),
	awful.button({ modkey }, 3, awful.mouse.client.resize))


-- ---------------------------------------------------------------------
-- Rules
-- ---------------------------------------------------------------------

-- Rules to apply to new clients (through the "manage" signal).
local rules = {
	-- All clients will match this rule
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

	-- Floating clients
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

	-- Default normal client rules
	{ rule_any = { type = { "normal" } },
		properties = {
			titlebars_enabled = true,
			floating = false,
			maximized_vertical = false,
			maximized_horizontal = false,
			screen = screens.get_primary(),
		}
	},

	-- Web
	{ rule = { class = "Firefox" },
		except = { type = "dialog" },
		properties = {
			tag = desktops.tags_names[1],
		}
	},
	-- Dev
	{ rule_any = { class = { "VSCodium", "Zim" }},
		properties = {
			tag = desktops.tags_names[2],
			floating = false,
		}
	},

	{ rule_any = { class = { "jetbrains-phpstorm" }},
		except = { type = "dialog" },
		properties = {
			tag = desktops.tags_names[1],
			floating = false, -- Task list is too small in popup
		}
	},
	-- Mail
	{ rule = { class = "Thunderbird" },
		properties = {
			tag = desktops.tags_names[3],
		}
	},
	-- Files explorer
	{ rule = { class = "Pcmanfm" },
		properties = {
			tag = desktops.tags_names[4],
		}
	},
	-- Mixed
	{ rule_any = { class = { "Godot", "Keybase", "balena-etcher-electron", "GParted", "Transmission" }},
		properties = {
			tag = desktops.tags_names[5],
		}
	},
	-- Graphics
	{ rule_any = { class = { "Gimp", "Krita", }},
		properties = {
			tag = desktops.tags_names[6],
		}
	},
	-- Office
	{ rule_any = { class = { "libreoffice-writer", "libreoffice-calc", "Evince", "Simple-scan" }},
		properties = {
			tag = desktops.tags_names[7],
		}
	},
	-- Games
	{ rule = { class = "Steam" },
		properties = {
			tag = desktops.tags_names[8],
		}
	},

	-- Floating on top and sticky
	{ rule = { class = "ksnip" },
		properties = {
			floating = true,
			sticky = true,
			ontop = true,
			screen = screens.count(),
			placement = awful.placement.no_offscreen + awful.placement.top,
		}
	},
	{ rule_any = { class = { "mpv" }, instance = { "www.netflix.com__browse" }},
		properties = {
			-- Sticky in corner on main screen
			focus = false,
			sticky = true,
			fullscreen = false,
			floating = true,
			ontop = true, -- Not compatible with fullscreen
			screen = screens.get_primary(), -- On primary screen
			callback = function(c)
				-- 2/5 bottom right of primary screen
				sreen_geometry = screens.get_primary().geometry
				c:geometry( { width = sreen_geometry.width * 2 / 5 , height = sreen_geometry.height * 2 / 5 } )
				awful.placement.bottom_right(c)
			end
		}
	},
}

-- set rules
awful.rules.rules = rules

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

-- Force ontop on client fullscreen exit (fullscreen unsets ontop)
client.connect_signal("property::fullscreen", function(c) if not c.fullscreen then c.ontop = true end end)

-- ---------------------------------------------------------------------
-- Auto start
-- ---------------------------------------------------------------------
awful.spawn.with_shell("~/.dotfiles/bin/autostart")
