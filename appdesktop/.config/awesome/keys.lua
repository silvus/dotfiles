local awful = require("awful")
local naughty = require("naughty")
local beautiful = require("beautiful")
local hotkeys_popup = require("awful.hotkeys_popup").widget
local menubar = require("menubar")
local screens = require("screens")
local desktops = require("desktops")
local config = require("config")
-- Quake like terminal (single instance for all screens)
local quake = require("utils.quake")
-- Dashboard
local dashboard = require("utils.dashboard")
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
function unpanic_key()
	-- Unmute notifications
	naughty.resume()
	widget_notifications.update()

	-- Unmute sound
	awful.util.spawn("amixer -D pulse sset Master unmute", false)
	widget_volume.volume.update()

	-- Primary screen - Go to last tag
	local screen_primary = screens.get_primary()
	local tag_next = screen_primary.tags[10]
	if tag_next then
		tag_next:view_only()
	end
end


keys.global = awful.util.table.join(
	awful.key({ modkey }, "h", hotkeys_popup.show_help, {description="show help", group="awesome"}),

	awful.key({ modkey, "Shift"}, "h", function()
			-- Utility function to trim a string
			local function trim(s)
				if s == nil then return nil end
				return (s:gsub("^%s*(.-)%s*$", "%1"))
			end

			-- parse current layout from setxkbmap
			local file = assert(io.popen('setxkbmap -query', 'r'))
			local status = file:read('*all')
			file:close()
			naughty.notify({
				title = 'Keymap',
				text = trim(status),
				icon = beautiful.paragraph,
				preset = naughty.config.presets.success
			})

			local layout = trim(string.match(status, "layout:([^\n]*)"))
			local variant = trim(string.match(status, "variant:([^\n]*)"))

			-- Launch keyboard visualizer (dep: gkbd-capplet)
			-- or xkeycaps (and use gucharmap)
			awful.util.spawn("gkbd-keyboard-display -l " .. layout .. " " .. variant)
		end, {description="Keyboard keys help", group="awesome"}),

	-- Next/previous tag
	awful.key({ modkey }, "<",   awful.tag.viewprev, {description = "view previous tag", group = "tag"}),
	awful.key({ modkey }, ">", awful.tag.viewnext, {description = "view next tag", group = "tag"}),
	-- Go back to previous tag
	awful.key({ modkey }, "Tab", function()
			local screen = screens.get_primary()
			awful.tag.history.restore(screen)
		end, {description = "go back to previous tag", group = "tag"}),

	-- awful.key({ modkey }, "Right", function ()
	--		 awful.client.focus.byidx(1)
	-- 		end, {description = "focus next by index", group = "client"}
	-- ),
	-- awful.key({ modkey }, "Left", function ()
	--		 awful.gclient.focus.byidx(-1)
	--	 end, {description = "focus previous by index", group = "client"}
	-- ),
	-- awful.key({ modkey,		   }, "w", function()
	--		mymainmenu:show()
	--	end, {description = "show main menu", group = "awesome"}),

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

	awful.key({ modkey }, "s", function()
			awful.client.focus.global_bydirection("down")
			if client.focus then client.focus:raise() end
		end, {description = "change client focus", group = "client"}),
	awful.key({ modkey }, "z", function()
			awful.client.focus.global_bydirection("up")
			if client.focus then client.focus:raise() end
		end, {description = "change client focus", group = "client"}),
	awful.key({ modkey }, "q", function()
			awful.client.focus.global_bydirection("left")
			if client.focus then client.focus:raise() end
		end, {description = "change client focus", group = "client"}),
	awful.key({ modkey }, "d", function()
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
	awful.key({ modkey }, "u", function()
			awful.client.urgent.jumpto()
		end, {description = "jump to urgent client", group = "client"}),
	-- awful.key({ modkey,	}, "Tab", function()
	-- 		awful.client.focus.history.previous()
	-- 		if client.focus then
	-- 			client.focus:raise()
	-- 		end
	-- 	end, {description = "go back", group = "client"}),

	-- Terminal
	awful.key({ modkey }, "Return", function()
			awful.spawn( config.terminal .. " -title terminal -e " .. config.home .. "/.dotfiles/bin/tmuxdev")
		end, {description = "open a terminal", group = "launcher"}),
	-- awful.key({}, "²", function () awful.spawn(config.home .. "/.dotfiles/bin/guakify 'rxvt-unicode.URxvt' '" .. terminal .. " -e " .. config.home .. "/.dotfiles/bin/tmuxdev'") end, {description = "open a terminal", group = "launcher"}),

	-- Prompt
	awful.key({ modkey }, "x", function()
			screens.get_primary().promptbox:run()
		end, {description = "run prompt", group = "launcher"}),

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
		end, {description="Rofi launch", group="launcher"}),
	-- Menubar
	awful.key({ modkey, "Shift"}, "e", function()
		menubar.show(screens.get_primary())
	end, {description = "show the menubar", group = "launcher"}),

	-- Quake-like terminal (²)
	awful.key({}, "#49", function ()
			quake.term:toggle()
		end, {description = "Toggle guake like terminal", group = "launcher"}),

	-- Volume Keys
	awful.key({}, "XF86AudioLowerVolume", function ()
			awful.util.spawn("amixer -q sset Master 5%-", false)
			-- trigger widget update
			widget_volume.volume.update()
		end, {description = "Volume up", group = "audio"}),
	awful.key({}, "XF86AudioRaiseVolume", function ()
			awful.util.spawn("amixer -q sset Master 5%+", false)
			widget_volume.volume.update()
		end, {description = "Volume down", group = "audio"}),
	awful.key({}, "XF86AudioMute", function ()
			awful.util.spawn("amixer -D pulse sset Master toggle", false)
			widget_volume.volume.update()
		end, {description = "volume mute", group = "audio"}),
	-- Simulated volume keys
	awful.key({ modkey }, "Page_Down", function ()
			awful.util.spawn("amixer -q sset Master 5%-", false)
			widget_volume.volume.update()
		end, {description = "Volume up", group = "audio"}),
	awful.key({ modkey }, "Page_Up", function ()
			awful.util.spawn("amixer -q sset Master 5%+", false)
			widget_volume.volume.update()
		end, {description = "Volume down", group = "audio"}),
	awful.key({ modkey }, "Scroll_Lock", function ()
			awful.util.spawn("amixer -D pulse sset Master toggle", false)
			widget_volume.volume.update()
		end, {description = "volume mute", group = "audio"}),
	-- Media Keys
	awful.key({}, "XF86Tools", function()
			awful.util.spawn("/data/doc/.bin/sport", false)
		end, {description = "Sport launcher", group = "audio"}),
	awful.key({}, "XF86AudioPlay", function()
			awful.util.spawn(config.home .. "/.dotfiles/bin/musicplay", false)
		end, {description = "audio toggle play/pause", group = "audio"}),
	awful.key({}, "XF86AudioStop", function()
		awful.util.spawn(config.home .. "/.dotfiles/bin/musicstop", false)
		end, {description = "music stop", group = "audio"}),
	awful.key({}, "XF86AudioNext", function()
			awful.util.spawn(config.home .. "/.dotfiles/bin/musicnext", false)
		end, {description = "music next", group = "audio"}),
	awful.key({}, "XF86AudioPrev", function()
			awful.util.spawn(config.home .. "/.dotfiles/bin/musicprevious", false)
		end, {description = "music previous", group = "audio"}),

	-- Notifications toogle
	awful.key({ modkey }, "n", function ()
			naughty.toggle()
			widget_notifications.update()
		end, {description = "notification toggle", group = "awesome"}),

	-- Touchpad Toggle
	awful.key({}, "XF86TouchpadToggle", function ()
			awful.util.spawn_with_shell("synclient TouchpadOff=$(synclient -l | grep -c 'TouchpadOff.*=.*0')")
		end, {description = "toggle touchpad", group = "launcher"}),

	-- Print screen
	awful.key({}, "Print", function()
			awful.util.spawn("ksnip", false)
		end, {description = "make a printscreen with ksnip", group = "launcher"}),

	-- Reload
	awful.key({ modkey, "Shift" }, "r", awesome.restart, {description = "reload awesome", group = "awesome"}),
	-- awful.key({ modkey, "Shift" }, "s", awesome.quit, {description = "quit awesome", group = "awesome"}),

	-- Lock
	awful.key({ modkey, "Shift" }, "l", function()
			-- awful.util.spawn("i3lock --color 001905 --show-failed-attempts --ignore-empty-password", false)
			awful.util.spawn("i3lock --color 000305 -t -i " .. config.home .. "/.dotfiles/appdesktop/.config/awesome/wallpapers/lock_1.png --show-failed-attempts --ignore-empty-password", false)
		end, {description = "lock screen", group = "launcher"}),

	-- Shutdown or restart
	awful.key({ modkey, "Shift" }, "s", function()
			awful.util.spawn(config.home .. "/.dotfiles/bin/dmenu_shutdown", false)
		end, {description = "shutdown", group = "launcher"}),

	-- Dashboard
	awful.key({ modkey }, "a", function()
			dashboard.show()
		end, {description = "dashboard", group = "custom"}),

	-- VPN
	awful.key({ modkey, "Shift" }, "v", function()
			awful.util.spawn(config.home .. "/.dotfiles/bin/dmenu_vpn", false)
		end, {description = "launch vpn", group = "launcher"}),

	-- Firefox refresh
	awful.key({ modkey }, "r", function()
		awful.util.spawn(config.home .. "/.dotfiles/bin/refresh_firefox", false)
	end, {description = "refresh firefox", group = "launcher"}),

	-- Toggle scratchpad tag (²)
	awful.key({ modkey }, "#49", function ()
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
		end, {description = "toggle scratchpad tag", group = "tag"}),
	-- Move client to scratchpad tag
	awful.key({ modkey, "Shift" }, "#49", function ()
			if client.focus then
				local screen = awful.screen.focused()
				local tag_next = screen.tags[10]
				if tag_next then
					client.focus:move_to_tag(tag_next)
					tag_next:view_only()
				end
			end
		end, {description = "move focused client to scratchpad", group = "tag"}),
	
	-- Panic buttons
	awful.key({ }, "Pause", function ()
			panic_key()
		end, {description = "Panic button", group = "tag"}),
	awful.key({modkey}, "Escape", function ()
			panic_key()
		end, {description = "Panic button", group = "tag"}),
	-- Unpanic buttons
	awful.key({ modkey }, "Pause", function ()
			unpanic_key()
		end, {description = "Unpanic button", group = "tag"}),
	-- Move client to X tag
	awful.key({ modkey, "Shift" }, "Pause", function ()
			if client.focus then
				local s_primary = screens.get_primary()
				local tag_next = s_primary.tags[11]
				if tag_next then
					client.focus:move_to_tag(tag_next)
					tag_next:view_only()
				end
			end
		end, {description = "move focused client to X tag", group = "tag"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
-- for i = 1, 9 do
for i, v in pairs(desktops.tags_names) do
	keys.global = awful.util.table.join(keys.global,
	-- View tag only.
	awful.key({ modkey }, "#" .. i + 9, function ()
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
		end, {description = "view tag", group = "tag"}),
	-- Toggle tag display.
	awful.key({ modkey, "Control" }, "#" .. i + 9, function ()
			local screen = screens.get_primary()
			local tag_next = screen.tags[i]
			if tag_next then
				awful.tag.viewtoggle(tag_next)
			end
		end, {description = "toggle tag", group = "tag"}),
	-- Move client to tag.
	awful.key({ modkey, "Shift" }, "#" .. i + 9, function ()
			if client.focus then
				local screen = screens.get_primary()
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

return keys
