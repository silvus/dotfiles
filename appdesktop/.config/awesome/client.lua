local awful = require("awful")
local config = require("config")

modkey = config.modkey
modkey = "Mod4"

local client = {}

-- Client keys
-- ----------------------------------------------------------------------------
client.keys = awful.util.table.join(
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
client.buttons = awful.util.table.join(
	awful.button({ }, 1, function(c) client.focus = c; c:raise() end),
	awful.button({ modkey }, 1, awful.mouse.client.move),
	awful.button({ modkey }, 3, awful.mouse.client.resize))


return client