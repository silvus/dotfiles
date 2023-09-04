local awful = require("awful")
local naughty = require("naughty")
local beautiful = require("beautiful")
local hotkeys_popup = require("awful.hotkeys_popup").widget
-- local menubar = require("menubar")
local screens = require("screens")
local desktops = require("desktops")
local config = require("config")
-- Quake like terminal (single instance for all screens)
local quake = require("utils.quake")
local widget_volume = require("widgets.volume")
local widget_notifications = require("widgets.notifications")

-- Set a global variable, a local one
local globalclient = client

local io = require("io")
local string = require("string")

local keys = {}


modkey = config.modkey

-- Panic buttons actions
function panic_key()
	-- for all screens
	for s in screen do
		-- Go to second tag to fake history
		local tag_next = s.tags[2]
		if tag_next then
			tag_next:view_only()
		end
		-- Go to first tag
		local tag_next = s.tags[1]
		if tag_next then
			tag_next:view_only()
		end
	end

	-- Mute sound
	awful.util.spawn("amixer -D pulse sset Master mute", false)
	widget_volume.volume.update()

	-- Clear all notifications
	naughty.destroy_all_notifications()

	-- Mute notifications
	naughty.suspend()
	widget_notifications.update()

	-- Focus on first window in tmux
	awful.util.spawn("tmux select-window -t 1", false)

	-- Clients loop
	for _, c in ipairs(globalclient.get()) do
		-- Close MPV
		if c.class == "mpv" then
			-- c:kill()  -- kill doesn't preserve video position
			-- Simulate "q" keypress
			awful.util.spawn("xdotool key --window " .. c.window .. " q")
		end

		-- Unpin and minimize all sticky clients
		-- if c.sticky then
		-- 	c.ontop = false
		-- 	c.above = false
		-- 	c.sticky = false
		-- 	c.minimized = true
		-- end

		-- Re-apply rules
		-- awful.rules.apply(c)
	end

	-- Focus primary screen
	awful.screen.focus(screens.get_primary())
end

-- Unpanic button
function unpanic_key(restaure_tag)
	-- Unmute notifications
	naughty.resume()
	widget_notifications.update()

	-- Unmute sound
	awful.util.spawn("amixer -D pulse sset Master unmute", false)
	widget_volume.volume.update()

	-- Restaure previous tag
	if restaure_tag then
		local screen = screens.get_primary()
		awful.tag.history.restore(screen, 2)
	end
end

-- Focus client in direction
function focus_client(direction)
	local c = client.focus
	if c and c.maximized then
		-- Focused on a maximized client, maximized next client
		c.maximized = false
		if direction == "left" or direction == "up" then
			awful.client.focus.byidx(-1, c)
		else
			awful.client.focus.byidx(1, c)
		end
		if client.focus then
			client.focus.maximized = true
			client.focus:raise()
		end
	else
		-- Focus by direction
		awful.client.focus.global_bydirection(direction, c, max)
		if client.focus then client.focus:raise() end
	end
end

keys.global = awful.util.table.join(
	awful.key({ modkey }, "h", hotkeys_popup.show_help, { description = "show help", group = "awesome" }),

	-- Next/previous tag
	awful.key({ modkey }, "<", awful.tag.viewprev, { description = "view previous tag", group = "tag" }),
	awful.key({ modkey }, ">", awful.tag.viewnext, { description = "view next tag", group = "tag" }),
	-- Go back to previous tag
	awful.key({ modkey }, "Tab", function()
		local screen = screens.get_primary()
		awful.tag.history.restore(screen)
	end, { description = "go back to previous tag", group = "tag" }),

	awful.key({ modkey }, "w", function()
		-- Toggle all bars
		for s in screen do
			s.bar.visible = not s.bar.visible

			-- Toggle titlebars
			-- for _, c in ipairs(globalclient.get()) do
			-- 	awful.titlebar.toggle(c)
			-- end
		end
	end, { description = "toggle Wibar", group = "awesome" }),

	-- By direction client focus
	awful.key({ modkey }, "Down", function()
		focus_client("down")
	end, { description = "change client focus", group = "client" }),
	awful.key({ modkey }, "Up", function()
		focus_client("up")
	end, { description = "change client focus", group = "client" }),
	awful.key({ modkey }, "Left", function()
		focus_client("left")
	end, { description = "change client focus", group = "client" }),
	awful.key({ modkey }, "Right", function()
		focus_client("right")
	end, { description = "change client focus", group = "client" }),

	awful.key({ modkey }, "s", function()
		focus_client("down")
	end, { description = "change client focus", group = "client" }),
	awful.key({ modkey }, "z", function()
		focus_client("up")
	end, { description = "change client focus", group = "client" }),
	awful.key({ modkey }, "q", function()
		focus_client("left")
	end, { description = "change client focus", group = "client" }),
	awful.key({ modkey }, "d", function()
		focus_client("right")
	end, { description = "change client focus", group = "client" }),

	-- Layout manipulation
	awful.key({ modkey, "Control" }, "space", function()
		awful.layout.inc(1)
	end, { description = "select next", group = "layout" }),
	awful.key({ modkey, "Shift" }, "space", function()
		awful.layout.inc(-1)
	end, { description = "select previous", group = "layout" }),

	awful.key({ modkey, "Shift" }, "Right", function()
		awful.client.swap.global_bydirection('right')
	end, { description = "swap with right client", group = "client" }),
	awful.key({ modkey, "Shift" }, "Left", function()
		awful.client.swap.global_bydirection('left')
	end, { description = "swap with left client", group = "client" }),
	awful.key({ modkey, "Shift" }, "Up", function()
		awful.client.swap.global_bydirection('up')
	end, { description = "swap with top client", group = "client" }),
	awful.key({ modkey, "Shift" }, "Down", function()
		awful.client.swap.global_bydirection('down')
	end, { description = "swap with bottom client", group = "client" }),

	awful.key({ modkey, "Shift" }, "Up", function()
		awful.tag.incmwfact(0.05)
	end, { description = "Increase master width", group = "client" }),
	awful.key({ modkey, "Shift" }, "Down", function()
		awful.tag.incmwfact(-0.05)
	end, { description = "Decrease master width", group = "client" }),
	awful.key({ modkey, "Control" }, "Right", function()
		awful.screen.focus_bydirection('right')
	end, { description = "focus the right screen", group = "screen" }),
	awful.key({ modkey, "Control" }, "Left", function()
		awful.screen.focus_bydirection('left')
	end, { description = "focus the left screen", group = "screen" }),
	awful.key({ modkey, "Control" }, "Up", function()
		awful.screen.focus_bydirection('up')
	end, { description = "focus the top screen", group = "screen" }),
	awful.key({ modkey, "Control" }, "down", function()
		awful.screen.focus_bydirection('down')
	end, { description = "focus the bottom screen", group = "screen" }),

	awful.key({ modkey }, "u", function()
		awful.client.urgent.jumpto()
	end, { description = "jump to urgent client", group = "client" }),

	-- Terminal
	awful.key({ modkey }, "Return", function()
		awful.spawn(config.terminal .. " -title terminal -e " .. config.home .. "/.dotfiles/bin/tmuxdev")
	end, { description = "open a terminal", group = "launcher" }),
	-- awful.key({}, "²", function () awful.spawn(config.home .. "/.dotfiles/bin/guakify 'rxvt-unicode.URxvt' '" .. terminal .. " -e " .. config.home .. "/.dotfiles/bin/tmuxdev'") end, {description = "open a terminal", group = "launcher"}),

	-- Prompt
	-- awful.key({ modkey }, "x", function()
	-- 		screens.get_primary().promptbox:run()
	-- 	end, {description = "run prompt", group = "launcher"}),

	-- Clients menu
	-- awful.key({ modkey, "Shift"}, "e", function()
	-- 		-- "coords" doesn't work: https://github.com/awesomeWM/awesome/issues/2349
	-- 		-- Default to mouse.coords()
	-- 		-- awful.menu.clients({theme = { width = 500 }}, { keygrabber=true, coords={x=525, y=330} })
	-- 		awful.menu.clients({theme = { width = 500} })
	-- 	end, {description="client menu", group="launcher"}),

	-- Rofi
	awful.key({ modkey }, "e", function()
		-- "coords" doesn't work: https://github.com/awesomeWM/awesome/issues/2349
		-- Default to mouse.coords()
		-- awful.menu.clients({theme = { width = 500 }}, { keygrabber=true, coords={x=525, y=330} })
		awful.util.spawn("rofi -show drun")
	end, { description = "Rofi launch", group = "launcher" }),
	-- Menubar
	-- awful.key({ modkey, "Shift"}, "e", function()
	-- 	menubar.show(screens.get_primary())
	-- end, {description = "show the menubar", group = "launcher"}),

	-- Editor
	awful.key({ modkey, "Shift" }, "e", function()
		awful.util.spawn("codium")
	end, { description = "open editor", group = "launcher" }),
	-- File Manager
	awful.key({ modkey, "Shift" }, "f", function()
		awful.util.spawn("thunar")
	end, { description = "open file manager", group = "launcher" }),

	-- Quake-like terminal (²)
	awful.key({}, "#49", function()
		quake.term:toggle()
	end, { description = "Toggle guake like terminal", group = "launcher" }),
	awful.key({ modkey }, "Escape", function()
		quake.term:toggle()
	end, { description = "Toggle guake like terminal", group = "launcher" }),

	-- Volume Keys
	awful.key({}, "XF86AudioLowerVolume", function()
		awful.util.spawn("amixer -q sset Master 5%-", false)
		-- trigger widget update
		widget_volume.volume.update()
	end, { description = "Volume up", group = "audio" }),
	awful.key({}, "XF86AudioRaiseVolume", function()
		awful.util.spawn("amixer -q sset Master 5%+", false)
		widget_volume.volume.update()
	end, { description = "Volume down", group = "audio" }),
	awful.key({}, "XF86AudioMute", function()
		awful.util.spawn("amixer -q sset Master toggle", false)
		widget_volume.volume.update()
	end, { description = "volume mute", group = "audio" }),
	-- Simulated volume keys
	awful.key({ modkey }, "Page_Down", function()
		awful.util.spawn("amixer -q sset Master 5%-", false)
		widget_volume.volume.update()
	end, { description = "Volume up", group = "audio" }),
	awful.key({ modkey }, "Page_Up", function()
		awful.util.spawn("amixer -q sset Master 5%+", false)
		widget_volume.volume.update()
	end, { description = "Volume down", group = "audio" }),
	awful.key({ modkey }, "End", function()
		awful.util.spawn("amixer -q sset Master toggle", false)
		widget_volume.volume.update()
	end, { description = "volume mute toggle", group = "audio" }),

	-- Media Keys
	awful.key({}, "XF86AudioPlay", function()
		awful.util.spawn("playerctl play-pause", false)
	end, { description = "audio toggle play/pause", group = "audio" }),
	awful.key({}, "XF86AudioStop", function()
		awful.util.spawn("playerctl stop", false)
	end, { description = "music stop", group = "audio" }),
	awful.key({}, "XF86AudioNext", function()
		awful.util.spawn("playerctl next", false)
	end, { description = "music next", group = "audio" }),
	awful.key({}, "XF86AudioPrev", function()
		awful.util.spawn("playerctl previous", false)
	end, { description = "music previous", group = "audio" }),
	-- Simulate Media Keys
	awful.key({}, "Pause", function()
		awful.util.spawn("playerctl play-pause", false)
	end, { description = "audio toggle play/pause", group = "audio" }),
	awful.key({ modkey }, "Home", function()
		awful.util.spawn("playerctl next", false)
	end, { description = "music next", group = "audio" }),
	awful.key({ modkey }, "Insert", function()
		awful.util.spawn("playerctl previous", false)
	end, { description = "music previous", group = "audio" }),

	-- Notifications toogle
	awful.key({ modkey }, "n", function()
		naughty.toggle()
		widget_notifications.update()
	end, { description = "notification toggle", group = "awesome" }),

	-- Touchpad Toggle
	awful.key({}, "XF86TouchpadToggle", function()
		awful.util.spawn_with_shell("synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')")
	end, { description = "toggle touchpad", group = "launcher" }),

	-- Print screen
	awful.key({}, "Print", function()
		awful.util.spawn("ksnip", false)
	end, { description = "make a printscreen with ksnip", group = "launcher" }),

	-- Reload
	awful.key({ modkey, "Shift" }, "r", awesome.restart, { description = "reload awesome", group = "awesome" }),
	-- awful.key({ modkey, "Shift" }, "s", awesome.quit, {description = "quit awesome", group = "awesome"}),

	-- Lock
	awful.key({ modkey, "Shift" }, "l", function()
		-- awful.util.spawn("i3lock --color 001905 --show-failed-attempts --ignore-empty-password", false)
		awful.util.spawn(
			"i3lock --color 000305 -t -i " ..
			config.home ..
			"/.dotfiles/appdesktop/.config/awesome/wallpapers/lock_1.png --show-failed-attempts --ignore-empty-password",
			false)
	end, { description = "lock screen", group = "launcher" }),

	-- Shutdown or restart
	awful.key({ modkey, "Shift" }, "s", function()
		awful.util.spawn(config.home .. "/.dotfiles/bin/dmenu_shutdown", false)
	end, { description = "shutdown", group = "launcher" }),

	-- VPN
	-- awful.key({ modkey, "Shift" }, "v", function()
	-- 		awful.util.spawn(config.home .. "/.dotfiles/bin/dmenu_vpn", false)
	-- 	end, {description = "launch vpn", group = "launcher"}),

	-- Firefox refresh
	awful.key({ modkey }, "r", function()
		awful.util.spawn(config.home .. "/.dotfiles/bin/refresh_firefox", false)
	end, { description = "refresh firefox", group = "launcher" }),

	-- Toggle scratchpad tag (²)
	awful.key({ modkey }, "#49", function()
		local screen = screens.get_primary()
		local tag_next = screen.tags[10]
		local tag_current = screen.selected_tag
		if tag_next then
			if tag_next == tag_current then
				awful.tag.history.restore(screen)
			else
				tag_next:view_only()
			end
		end
	end, { description = "toggle scratchpad tag", group = "tag" }),
	-- Move client to scratchpad tag
	awful.key({ modkey, "Shift" }, "#49", function()
		if client.focus then
			local screen = awful.screen.focused()
			local tag_next = screen.tags[10]
			if tag_next then
				client.focus:move_to_tag(tag_next)
				tag_next:view_only()
			end
		end
	end, { description = "move focused client to scratchpad", group = "tag" })

-- -- Panic buttons
-- awful.key({ modkey }, "Pause", function ()
-- 		panic_key()
-- 	end, {description = "Panic button", group = "tag"}),
-- -- Unpanic buttons
-- awful.key({ modkey, "Shift" }, "Pause", function ()
-- 		unpanic_key(false)
-- 	end, {description = "Unpanic button", group = "tag"}),
-- -- Toggle Panic buttons
-- awful.key({modkey}, "a", function ()
-- 		if naughty.is_suspended() then
-- 			unpanic_key(true)
-- 		else
-- 			panic_key()
-- 		end
-- 	end, {description = "Toggle Panic button", group = "tag"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
-- for i = 1, 9 do
for i, v in pairs(desktops.tags_names) do
	keys.global = awful.util.table.join(keys.global,
		-- View tag only.
		awful.key({ modkey }, "#" .. i + 9, function()
			local screen = screens.get_primary()
			local tag_next = screen.tags[i]
			local tag_current = screen.selected_tag

			if tag_next then
				if tag_next == tag_current then
					-- If already on focused screen, go to previous one
					awful.tag.history.restore(screen)
				else
					-- Just go to the screen
					tag_next:view_only()
				end
			end
		end, { description = "view tag", group = "tag" }),
		-- Toggle tag display.
		awful.key({ modkey, "Control" }, "#" .. i + 9, function()
			local screen = screens.get_primary()
			local tag_next = screen.tags[i]
			if tag_next then
				awful.tag.viewtoggle(tag_next)
			end
		end, { description = "toggle tag", group = "tag" }),
		-- Move client to tag.
		awful.key({ modkey, "Shift" }, "#" .. i + 9, function()
			if client.focus then
				local screen = screens.get_primary()
				local tag_next = client.focus.screen.tags[i]
				if tag_next then
					client.focus:move_to_tag(tag_next)
					tag_next:view_only()
				end
			end
		end, { description = "move focused client to tag", group = "tag" }),
		-- Toggle tag on focused client.
		awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9, function()
			if client.focus then
				local tag_next = client.focus.screen.tags[i]
				if tag_next then
					client.focus:toggle_tag(tag_next)
				end
			end
		end, { description = "toggle focused client on tag", group = "tag" })
	)
end

return keys
