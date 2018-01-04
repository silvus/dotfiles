-- ---------------------------------------------------------------------
-- Init
-- ---------------------------------------------------------------------

-- init random
math.randomseed(os.time());

-- Standard lua
local string = require("string")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local lain    = require("lain")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup").widget
local os      = { getenv = os.getenv, setlocale = os.setlocale }

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
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
-- Include personal config
-- ---------------------------------------------------------------------
function isModuleAvailable(name)
  if package.loaded[name] then
    return true
  else
    for _, searcher in ipairs(package.searchers or package.loaders) do
      local loader = searcher(name)
      if type(loader) == 'function' then
        package.preload[name] = loader
        return true
      end
    end
    return false
  end
end

function requireSafe(lib)
	if isModuleAvailable(lib) then
		require(lib)
	end
end

-- require("mail")
-- only for mars
if awesome.hostname == 'mars' then
	requireSafe('mail')
end

-- ---------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
-- beautiful.init(awful.util.get_themes_dir() .. "default/theme.lua")
beautiful.init("~/.config/awesome/themes/thetheme/theme.lua")

-- This is used later as the default terminal and editor to run.
-- terminal = "x-terminal-emulator"
-- terminal = "urxvt"
terminal = "rxvt-unicode"
editor = os.getenv("EDITOR") or "editor"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Notifications
naughty.config.defaults.timeout = 0
-- naughty.config.defaults.screen = 1
naughty.config.defaults.position = "top_right"
naughty.config.defaults.margin = 10
naughty.config.defaults.gap = 35
naughty.config.defaults.ontop = true
-- naughty.config.defaults.font = "terminus 12"
-- naughty.config.defaults.icon = nil
-- naughty.config.defaults.icon_size = 256
naughty.config.defaults.fg = beautiful.fg_focus
naughty.config.defaults.bg = beautiful.bg_focus
naughty.config.defaults.border_color = beautiful.border_focus
naughty.config.defaults.border_width = beautiful.border_width
naughty.config.defaults.hover_timeout = nil

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    -- awful.layout.suit.tile.bottom,
    -- awful.layout.suit.tile.top,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
    --awful.layout.suit.fair,
    -- awful.layout.suit.fair.horizontal,
    awful.layout.suit.max,
    -- awful.layout.suit.max.fullscreen,
    -- awful.layout.suit.magnifier,
    -- awful.layout.suit.corner.nw,
    awful.layout.suit.floating,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
}
-- }}}

-- Get the list of files from a directory
function scanDir(directory)
	local i, fileList, popen = 0, {}, io.popen
	for filename in popen("find " .. directory .. " -type f | sort"):lines() do
		i = i + 1
		fileList[i] = filename
	end
	return fileList
end

-- Wallpaper
local function set_wallpaper(s)
	if awful.util.file_readable(os.getenv("HOME") .. '/.wallpaper') then
		-- if ~/.wallpaper is a file, use it
		local wallpaper = os.getenv("HOME") .. '/.wallpaper'
		gears.wallpaper.maximized(wallpaper, s, true)
	elseif awful.util. dir_readable (os.getenv("HOME") .. '/.wallpaper') then
		-- if ~/.wallpaper is a directory, pick one into it
		local wallpapers = scanDir(os.getenv("HOME") .. '/.wallpaper')
		-- If we got a tag and an associated wallpaper
		local tag = awful.screen.focused().selected_tag
		if tag and wallpapers[tag.index] then
			-- if on a tag, use his index to find a wallpaper
			local wallpaper = wallpapers[tag.index]
			gears.wallpaper.maximized(wallpaper, s, true)
		else
			-- Fallback to random one
			-- local wallpaper = wallpapers[math.random(#wallpapers)]
			-- Fallback to first one
			local wallpaper = wallpapers[1]
			gears.wallpaper.maximized(wallpaper, s, true)
		end

	else
		-- Fallback to beautiful.wallpaper
		if beautiful.wallpaper then
	        local wallpaper = beautiful.wallpaper
	        -- If wallpaper is a function, call it with the screen
	        if type(wallpaper) == "function" then
	            wallpaper = wallpaper(s)
	        end
	        gears.wallpaper.maximized(wallpaper, s, true)
		end
	end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

-- Quake like (single instance for all screens)
-- local quake = lain.util.quake({
-- 	settings = function(c)
-- 		-- c.sticky = true
-- 	end,
-- 	-- name = "",
-- 	-- app = "subl",
-- 	app = "gvim",
-- 	followtag = true,
-- 	height = 1,
-- 	width = 1,
-- 	vert = "top",
-- 	horiz = "center"
-- })

-- ---------------------------------------------------------------------
-- Status bar
-- ---------------------------------------------------------------------

-- {{{ Helper functions
local function client_menu_toggle_fn()
    local instance = nil

    return function ()
        if instance and instance.wibox.visible then
            instance:hide()
            instance = nil
        else
            instance = awful.menu.clients({ theme = { width = 250 } })
        end
    end
end
-- }}}

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

-- {{{ Wibar
-- Create a wibox for each screen and add it
local taglist_buttons = awful.util.table.join(
                    awful.button({ }, 1, function(t) t:view_only() end),
                    awful.button({ modkey }, 1, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, function(t)
                                              if client.focus then
                                                  client.focus:toggle_tag(t)
                                              end
                                          end),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
                )

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
                                          end),
                     awful.button({ }, 3, client_menu_toggle_fn()))
                    -- awful.button({ }, 4, function ()
                    --                          awful.client.focus.byidx(1)
                    --                      end),
                    -- awful.button({ }, 5, function ()
                    --                          awful.client.focus.byidx(-1)
                    --                      end))

-- Separator
myspaceseparator = wibox.widget.textbox(' ')

-- Textclock widget with calendar
mytextclock = wibox.widget.textclock("%a %d %b <span color='#ffffff'>%H:%M:%S</span>", 1)
-- lain.widget.calendar({
--    attach_to = { mytextclock  }
--})

-- Disks bar
-- local fsicon = wibox.widget.imagebox(beautiful.hdd)
-- fsbar = wibox.widget {
--     forced_height    = 1,
--     forced_width     = 100,
--     margins          = 1,
--     paddings         = 1,
--     ticks            = true,
--     ticks_size       = 6,
--     max_value 		 = 100,
--     value            = 0,
--     color 			 = beautiful.success,
--     background_color = beautiful.info,
--     border_color     = beautiful.info,
--     widget           = wibox.widget.progressbar
-- }
-- fs = lain.widget.fs({
--     partition = "/",
--     -- options = "--exclude-type=tmpfs",
--     settings  = function()
--         if tonumber(fs_now.used) < 90 then
--             fsbar:set_color(beautiful.success)
--         else
--             fsbar:set_color(beautiful.error)
--         end
--         fsbar:set_value(fs_now.used)
--     end
-- })
-- local fsbg = wibox.container.background(fsbar, beautiful.info, gears.shape.rectangle)
-- local myfswidget = wibox.container.margin(fsbg, 2, 7, 4, 4)

-- Net bar
local neticon = wibox.widget.imagebox(beautiful.net)
local netbar = wibox.widget {
    forced_height    = 1,
    forced_width     = 100,
    margins          = 1,
    paddings         = 1,
    ticks            = true,
    ticks_size       = 10,
    max_value        = 1000,
    value            = 0,
    color            = beautiful.success,
    background_color = beautiful.info,
    border_color     = beautiful.info,
    widget           = wibox.widget.progressbar
}
local net = lain.widget.net({
    width = 100, border_width = 0, ticks = true, ticks_size = 100,
    settings = function()
        netbar:set_value(net_now.received)
    end
})
local netbg = wibox.container.background(netbar, beautiful.info, gears.shape.rectangle)
local mynetwidget = wibox.container.margin(netbg, 2, 7, 4, 4)

-- CPU bar
local cpuicon = wibox.widget.imagebox(beautiful.cpu)
local cpubar = wibox.widget {
    forced_height    = 1,
    forced_width     = 100,
    margins          = 1,
    paddings         = 1,
    ticks            = true,
    ticks_size       = 10,
    max_value        = 100,
    value            = 0,
    color            = beautiful.success,
    background_color = beautiful.info,
    border_color     = beautiful.info,
    widget           = wibox.widget.progressbar
}
local cpu = lain.widget.cpu({
    width = 100, border_width = 0, ticks = true, ticks_size = 10,
    settings = function()
        cpubar:set_value(cpu_now.usage)
        -- cpubar:set_value(cpu_now.used)
    end
})
local cpubg = wibox.container.background(cpubar, beautiful.info, gears.shape.rectangle)
local mycpuwidget = wibox.container.margin(cpubg, 2, 7, 4, 4)

-- Ram bar
local memicon = wibox.widget.imagebox(beautiful.mem)
local membar = wibox.widget {
    forced_height    = 1,
    forced_width     = 100,
    margins          = 1,
    paddings         = 1,
    ticks            = true,
    ticks_size       = 10,
    max_value 		 = 100,
    value            = 0,
    color 			 = beautiful.success,
    background_color = beautiful.info,
    border_color     = beautiful.info,
    widget           = wibox.widget.progressbar
}
local mem = lain.widget.mem({
    width = 100, border_width = 0, ticks = true, ticks_size = 10,
    settings = function()
        membar:set_value(mem_now.perc)
        -- membar:set_value(mem_now.used)
    end
})
local membg = wibox.container.background(membar, beautiful.info, gears.shape.rectangle)
local mymemwidget = wibox.container.margin(membg, 2, 7, 4, 4)

-- ALSA volume bar
local volicon = wibox.widget.imagebox(beautiful.vol)
local volume = lain.widget.alsabar({
    width = 100, border_width = 0, ticks = true, ticks_size = 10,
    timeout= 2,
    notification_preset = { font = beautiful.font },
    --togglechannel = "IEC958,3",
    settings = function()
        if volume_now.status == "off" then
            volicon:set_image(beautiful.vol_mute)
        elseif volume_now.level == 0 then
            volicon:set_image(beautiful.vol_no)
        elseif volume_now.level <= 50 then
            volicon:set_image(beautiful.vol_low)
        else
            volicon:set_image(beautiful.vol)
        end
    end,
    colors = {
        background   = beautiful.info,
        mute         = beautiful.error,
        unmute       = beautiful.success
    }
})
volume.tooltip.wibox.fg = beautiful.fg_focus
volume.bar:buttons(awful.util.table.join (
          awful.button({}, 1, function()
          	awful.spawn.with_shell(string.format("%s -e alsamixer", terminal))
          end),
          awful.button({}, 2, function()
            awful.spawn(string.format("%s set %s 100%%", volume.cmd, volume.channel))
            volume.update()
          end),
          awful.button({}, 3, function()
            awful.spawn(string.format("%s set %s toggle", volume.cmd, volume.togglechannel or volume.channel))
            volume.update()
          end),
          awful.button({}, 4, function()
            awful.spawn(string.format("%s set %s 5%%+", volume.cmd, volume.channel))
            volume.update()
          end),
          awful.button({}, 5, function()
            awful.spawn(string.format("%s set %s 5%%-", volume.cmd, volume.channel))
            volume.update()
          end)
))
local volumebg = wibox.container.background(volume.bar, beautiful.info, gears.shape.rectangle)
local myvolumewidget = wibox.container.margin(volumebg, 2, 7, 4, 4)

-- Moc
local musicicon = wibox.widget.imagebox(beautiful.music)
musicicon.visible = false
local moc = lain.widget.contrib.moc({
    music_dir = "/data/silvus/music",
    settings  = function()
    	if moc_now.state == 'PLAY' or moc_now.state == 'PAUSE' then
    		widget:set_markup("<span color='#ffffff'>" .. string.sub(moc_now.file:match( "([^/]+)$" ), 0 , 30) .. ' | ' .. moc_now.elapsed .. ' / ' .. moc_now.total .. "</span>")
    		musicicon.visible = true
    	else
			widget:set_markup("")
			musicicon.visible = false
    	end
    end
})
local mocbg = wibox.container.background(moc.widget, beautiful.bg_normal, gears.shape.rectangle)
local mymoc = wibox.container.margin(mocbg, 2, 7, 4, 4)

-- VPN
-- TODO: Use icons
local vpn = awful.widget.watch(
    "ip addr show tun0",
    5,
    function(widget, stdout, stderr, exitreason, exitcode)
		if exitcode == 0 then
	        widget:set_markup("<span color='" .. beautiful.success .. "'>VPN: ON</span>")
	    else
	    	widget:set_markup("VPN: OFF")
	    end
    end
)
local myvpn = wibox.container.margin(vpn, 2, 7, 4, 4)

-- Crypto
-- TODO: Use icons
-- local crypto = awful.widget.watch(
--     'curl -m10 -s "https://min-api.cryptocompare.com/data/price?fsym=XMR&tsyms=USD"',
--     10800, -- 3 heures
--     function(widget, stdout, stderr, exitreason, exitcode)
-- 		local xmr, pos, err = require("lain.util").dkjson.decode(stdout, 1, nil)
-- 		local xmr_price = 'XMR: $'.. (not err and xmr and xmr["USD"]) or "N/A"
--         widget:set_text(xmr_price)
--     end
-- )
-- local mycrypto = wibox.container.margin(crypto, 2, 7, 4, 4)


-- -- Mail - in mail.lua
-- local mailicondev = wibox.widget.imagebox()
-- mailicondev:buttons(awful.util.table.join(awful.button({ }, 1, function () awful.util.spawn(mail) end)))
-- local myimapcheckdev = lain.widget.imap({
-- 	timeout  = 180,
--  	is_plain = true,
--  	password = "nope",
--  	server = "mail.nope.net",
--  	mail = "nope@nope.fr",
--  	icon = beautiful.mail,
--  	settings = function()
--  		if mailcount > 0 then
--  			widget:set_markup("Dev <span color='#ffffff'>" .. mailcount .. '</span>')
--  			mailicondev:set_image(beautiful.mail_on)
--  		else
--  			widget:set_markup("")
--  			mailicondev:set_image(beautiful.mail)
--  		end
--  	end
-- })

awful.screen.connect_for_each_screen(function(s)
    -- Wallpaper
    set_wallpaper(s)

    -- Each screen has its own tag table.
	-- local names = { "main", "www", "skype", "gimp", "office", "im", "7", "8", "9" }
	-- local l = awful.layout.suit  -- Just to save some typing: use an alias.
	-- local layouts = { l.floating, l.tile, l.floating, l.fair, l.max, l.floating, l.tile.left, l.floating, l.floating }
	-- awful.tag(names, s, layouts)
    awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.noempty, taglist_buttons)

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, tasklist_buttons)

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            s.mytaglist,
            s.mypromptbox,
        },
        s.mytasklist, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            musicicon,
            mymoc,
            myspaceseparator,
            myvpn,
            neticon,
            mynetwidget,
            -- fsicon,
            -- myfswidget,
            cpuicon,
            mycpuwidget,
            memicon,
            mymemwidget,
            volicon,
            myvolumewidget,
            myspaceseparator,
            myimapcheckdev,
            mailicondev,
            myimapcheckpers,
            mailiconpers,
            -- myspaceseparator,
            -- mycrypto,
            myspaceseparator,
            mykeyboardlayout,
            wibox.widget.systray(),
            myspaceseparator,
            mytextclock,
            myspaceseparator,
            s.mylayoutbox,
        },
    }
end)
-- }}}

-- ---------------------------------------------------------------------
-- Key bindings
-- ---------------------------------------------------------------------

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "h",      hotkeys_popup.show_help,
              {description="show help", group="awesome"}),
    -- awful.key({ modkey, "Shift"   }, "Left",   awful.tag.viewprev,
    --           {description = "view previous", group = "tag"}),
    -- awful.key({ modkey, "Shift"   }, "Right",  awful.tag.viewnext,
    --           {description = "view next", group = "tag"}),
    -- awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
    --           {description = "go back", group = "tag"}),

    -- awful.key({ modkey,           }, "Right",
    --     function ()
    --         awful.client.focus.byidx( 1)
    --     end,
    --     {description = "focus next by index", group = "client"}
    -- ),
    -- awful.key({ modkey,           }, "Left",
    --     function ()
    --         awful.client.focus.byidx(-1)
    --     end,
    --     {description = "focus previous by index", group = "client"}
    -- ),

     -- By direction client focus
    awful.key({ modkey }, "Down",
        function()
            awful.client.focus.bydirection("down")
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey }, "Up",
        function()
            awful.client.focus.bydirection("up")
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey }, "Left",
        function()
            awful.client.focus.bydirection("left")
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey }, "Right",
        function()
            awful.client.focus.bydirection("right")
            if client.focus then client.focus:raise() end
        end),

    -- awful.key({ modkey,           }, "w", function () mymainmenu:show() end,
    --          {description = "show main menu", group = "awesome"}),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "Right", function () awful.client.swap.byidx(  1)    end,
              {description = "swap with next client by index", group = "client"}),
    awful.key({ modkey, "Shift"   }, "Left", function () awful.client.swap.byidx( -1)    end,
              {description = "swap with previous client by index", group = "client"}),
    awful.key({ modkey, "Control" }, "Right", function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "screen"}),
    awful.key({ modkey, "Control" }, "Left", function () awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "screen"}),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              {description = "jump to urgent client", group = "client"}),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}),

    -- On the fly useless gaps change
    -- awful.key({ altkey, "Control" }, "+", function () lain.util.useless_gaps_resize(1) end),
    -- awful.key({ altkey, "Control" }, "-", function () lain.util.useless_gaps_resize(-1) end),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.spawn(terminal .. " -title terminal -e " .. os.getenv("HOME") .. "/.dotfiles/bin/tmuxdev") end,
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, "Shift"   }, "r", awesome.restart,
              {description = "reload awesome", group = "awesome"}),
    -- awful.key({ modkey, "Shift"   }, "s", awesome.quit,
    --          {description = "quit awesome", group = "awesome"}),

    -- awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
    --           {description = "increase master width factor", group = "layout"}),
    -- awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
    --          {description = "decrease master width factor", group = "layout"}),
    -- awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
    --           {description = "increase the number of master clients", group = "layout"}),
    -- awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
    --           {description = "decrease the number of master clients", group = "layout"}),
    -- awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
    --           {description = "increase the number of columns", group = "layout"}),
    -- awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
    --           {description = "decrease the number of columns", group = "layout"}),
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
              {description = "select next", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
              {description = "select previous", group = "layout"}),

    -- awful.key({ modkey, "Control" }, "n",
    --           function ()
    --               local c = awful.client.restore()
    --               -- Focus restored client
    --               if c then
    --                   client.focus = c
    --                   c:raise()
    --               end
    --           end,
    --           {description = "restore minimized", group = "client"}),

    -- Prompt
    awful.key({ modkey },            "x",     function () awful.screen.focused().mypromptbox:run() end,
              {description = "run prompt", group = "launcher"}),

    -- awful.key({ modkey }, "x",
    --           function ()
    --               awful.prompt.run {
    --                 prompt       = "Run Lua code: ",
    --                 textbox      = awful.screen.focused().mypromptbox.widget,
    --                 exe_callback = awful.util.eval,
    --                 history_path = awful.util.get_cache_dir() .. "/history_eval"
    --               }
    --           end,
    --           {description = "lua execute prompt", group = "awesome"}),
    -- Menubar
    awful.key({ modkey }, "d", function() menubar.show() end,
              {description = "show the menubar", group = "launcher"}),

    -- awful.key({ modkey }, "e", function()
	-- 	awful.util.spawn("subl", false)
	-- end),
	-- Editor in quake
	-- awful.key({ modkey, }, "e", function ()
	--	quake:toggle()
	-- end),

	-- Text Editor
	awful.key({ modkey }, "e", function()
		awful.util.spawn("subl", false)
	end),

	-- Volume Keys
	awful.key({}, "XF86AudioLowerVolume", function ()
		awful.util.spawn("amixer -q sset Master 5%-", false)
	end),
	awful.key({}, "XF86AudioRaiseVolume", function ()
		awful.util.spawn("amixer -q sset Master 5%+", false)
	end),
	awful.key({}, "XF86AudioMute", function ()
		awful.util.spawn("amixer set Master 1+ toggle", false)
	end),

	-- Media Keys
	awful.key({}, "XF86AudioPlay", function()
		awful.util.spawn("mocp --toggle-pause", false)
	end),
	awful.key({}, "XF86AudioStop", function()
		awful.util.spawn("mocp --stop", false)
	end),
	awful.key({}, "XF86AudioNext", function()
		awful.util.spawn("mocp --next", false)
	end),
	awful.key({}, "XF86AudioPrev", function()
		awful.util.spawn("mocp --previous", false)
	end),

	-- lock
	awful.key({ modkey, "Shift" }, "l", function()
		-- awful.util.spawn("i3lock --color 001912 --show-failed-attempts --ignore-empty-password", false)
		awful.util.spawn("i3lock --color 001905 --show-failed-attempts --ignore-empty-password", false)
	end),
	-- shutdown or restart
	awful.key({ modkey, "Shift" }, "s", function()
		awful.util.spawn(os.getenv("HOME") .. "/.dotfiles/bin/dmenu_shutdown", false)
	end),
	-- manage VPN
	awful.key({ modkey, "Shift" }, "v", function()
		awful.util.spawn(os.getenv("HOME") .. "/.dotfiles/bin/dmenu_vpn", false)
	end)
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey,           }, "c",
	   function (c)
	       -- toggle titlebar
	       awful.titlebar.toggle(c)
	   end,
	   {description = "toggle titlebar", group = "client"}),
    awful.key({ modkey, "Shift"   }, "q",      function (c) c:kill()                         end,
              {description = "close", group = "client"}),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
              {description = "toggle floating", group = "client"}),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
              {description = "move to master", group = "client"}),
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
              {description = "move to screen", group = "client"}),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
              {description = "toggle keep on top", group = "client"}),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        {description = "minimize", group = "client"}),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "maximize", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
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
							-- TODO: Should be during tag creation
							-- TODO: here, it doesn't handle mouse event
							set_wallpaper(screen)
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                              tag:view_only()
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- ---------------------------------------------------------------------
-- Rules
-- ---------------------------------------------------------------------

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
		properties = { border_width = beautiful.border_width,
		             border_color = beautiful.border_normal,
		             focus = awful.client.focus.filter,
		             raise = true,
		             keys = clientkeys,
		             buttons = clientbuttons,
		             screen = awful.screen.preferred,
		             placement = awful.placement.no_overlap+awful.placement.no_offscreen
		}
    },

    -- Floating clients.
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
        role = {
          "AlarmWindow",  -- Thunderbird's calendar.
          -- "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { floating = true }
    },

    -- Add titlebars to normal clients and dialogs
    { rule_any = {type = { "normal", "dialog" }
		}, properties = { titlebars_enabled = true }
    },

    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = "2" } },

    { rule = { class = "mpv" },
		properties = {
			floating = false,
			-- titlebars_enabled = false,
			sticky = true,
			ontop = true,
		}
    },
    { rule = { class = "Firefox" },
		properties = {
			tag = "1"
			--switchtotag = true
		}
	},
    { rule = { class = "Thunderbird" },
		properties = {
			tag = "3"
			--switchtotag = true
		}
	},
    { rule = { class = "Steam" },
		properties = {
			tag = "8"
			--switchtotag = true
		}
	}
}
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

	if (c.class == "Firefox") then
		-- if it's a Firefox we will connect a signal which will call if 'name' changing
		c:connect_signal("property::name", function(c)
			if (string.find(c.name, "(Private Browsing)")) then
				-- if "(Private Browsing)" is part of 'c.name' then 'c' goes to tags[1][9]
				local tags = root.tags()
				c:tags({tags[9]})
			end
		end)
	end

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
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
-- client.connect_signal("mouse::enter", function(c)
--     if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
--         and awful.client.focus.filter(c) then
--         client.focus = c
--     end
-- end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

-- ---------------------------------------------------------------------
-- Auto start
-- ---------------------------------------------------------------------

awful.spawn.with_shell("setxkbmap -model pc105 -layout fr,us -variant oss")
awful.spawn.with_shell("~/.dotfiles/bin/autostart")
