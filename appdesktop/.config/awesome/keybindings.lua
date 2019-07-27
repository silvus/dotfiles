local awful = require("awful")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup").widget
local screens = require('screens')
local wallpaper = require("wallpaper")

-- Quake like terminal (single instance for all screens)
local quake = require("quake")

local keybindings = {
	globalkeys = {},
	clientkeys = {},
	clientbuttons = {},
}

-- Used to launch programms on tag fist navigation
local is_launched_editor = false
local is_launched_mail = false

-- Global keys
-- ----------------------------------------------------------------------------
keybindings.globalkeys = awful.util.table.join(
	awful.key({ modkey, }, "h", hotkeys_popup.show_help, {description="show help", group="awesome"}),

	-- Next/previous tag
	-- PageUp doesn't work: https://github.com/awesomeWM/awesome/issues/2147
	-- awful.key({ modkey, }, "PageUp",   awful.tag.viewprev ), {description = "view previous", group = "tag"}),
	-- awful.key({ modkey, }, "PageDown", awful.tag.viewnext ), {description = "view next", group = "tag"}),
	-- awful.key({ modkey, }, "Escape", awful.tag.history.restore, {description = "go back", group = "tag"}),

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
	awful.key({ modkey,	}, "e", function()
			-- "coords" doesn't work: https://github.com/awesomeWM/awesome/issues/2349
			-- Default to mouse.coords()
		    -- awful.menu.clients({theme = { width = 500 }}, { keygrabber=true, coords={x=525, y=330} })
		    awful.menu.clients({theme = { width = 500} })
		end, {description="client menu", group="launcher"}),

	-- Quake-like terminal (²)
	awful.key({}, "#49", function ()
			quake.term:toggle()
		end, {description = "Toggle guake like terminal", group = "launcher"}),

	-- Volume Keys
	awful.key({}, "XF86AudioLowerVolume", function ()
			awful.util.spawn("amixer -q sset Master 5%-", false)
			-- TODO: trigger widget update from here ?
			--volume.update()
		end, {description = "Volume UP", group = "audio"}),
	awful.key({}, "XF86AudioRaiseVolume", function ()
			awful.util.spawn("amixer -q sset Master 5%+", false)
			--volume.update()
		end, {description = "Volume down", group = "audio"}),
	awful.key({}, "XF86AudioMute", function ()
			awful.util.spawn("amixer set Master 1+ toggle", false)
			--volume.update()
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

	-- Toggle mail special tag
	awful.key({}, "F1", function()
		if is_launched_mail == false then
			-- Launch thunderbird the first time on the tag
			is_launched_mail = true
			awful.util.spawn("thunderbird", false)
		end
			toggle_special_tag("M")
		end, {description = "toggle mail tag", group = "tag"}),

	-- Toggle scratchpad special tag (²)
	awful.key({ modkey }, "#49", function ()
			if is_launched_editor == false then
				-- Launch editor the first time on the tag
				is_launched_editor = true
				awful.util.spawn("vscodium", false)
			end
			toggle_special_tag("S")
		end, {description = "toggle scratchpad tag", group = "tag"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
	keybindings.globalkeys = awful.util.table.join(keybindings.globalkeys,
		-- View tag only.
		awful.key({ modkey }, "#" .. i + 9, function ()
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
					wallpaper.update(screen)
				end
			end, {description = "view tag", group = "tag"}),
		-- Toggle tag display.
		awful.key({ modkey, "Control" }, "#" .. i + 9, function ()
				local screen = awful.screen.focused()
				local tag = screen.tags[i]
				if tag then
					awful.tag.viewtoggle(tag)
				end
			end, {description = "toggle tag", group = "tag"}),
		-- Move client to tag.
		awful.key({ modkey, "Shift" }, "#" .. i + 9, function ()
				if client.focus then
					local screen = awful.screen.focused()
					local tag = client.focus.screen.tags[i]
					if tag then
						client.focus:move_to_tag(tag)
						tag:view_only()
						wallpaper.update(screen)
					end
				end
			end, {description = "move focused client to tag", group = "tag"}),
		-- Toggle tag on focused client.
		awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9, function ()
				if client.focus then
					local tag = client.focus.screen.tags[i]
					if tag then
						client.focus:toggle_tag(tag)
					end
				end
			end, {description = "toggle focused client on tag", group = "tag"})
	)
end


-- Client keys
-- ----------------------------------------------------------------------------
keybindings.clientkeys = awful.util.table.join(
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
keybindings.clientbuttons = awful.util.table.join(
	awful.button({ }, 1, function(c) client.focus = c; c:raise() end),
	awful.button({ modkey }, 1, awful.mouse.client.move),
	awful.button({ modkey }, 3, awful.mouse.client.resize))


-- Toggle Special tag utility functiuon
-- ----------------------------------------------------------------------------
function toggle_special_tag(tagname)
	local screen = screens.get_primary()
	local tag_special = awful.tag.find_by_name(screen, tagname)
	local tag_current = screen.selected_tag

	if tag_special then
		if tag_special == tag_current then
			-- On scratchpad, go to previous tag
			awful.tag.history.restore(screen)
		else
			-- Update history (need for fist move, when history is empty)
			 awful.tag.history.update(screen)
			-- Go to the scratchpad
			tag_special:view_only()
			-- Focus primary screen
			-- awful.screen.focus(screen)
		end
		-- change wallpaper
		wallpaper.update(screen)
	end
end


return keybindings
