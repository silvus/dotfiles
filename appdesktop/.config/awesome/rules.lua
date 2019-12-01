
local awful = require("awful")
local beautiful = require("beautiful")
local desktops = require("desktops")
local screens = require("screens")
local globalclient = client
local clients = require("clients")


-- Rules to apply to new clients (through the "manage" signal).
local rules = {
	-- All clients will match this rule
	{ rule = { },
		properties = {
			border_width = beautiful.border_width,
			border_color = beautiful.border_normal,
			focus = awful.client.focus.filter,
			raise = true,
			keys = clients.keys,
			buttons = clients.buttons,
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
			screen = screens.count(),
			floating = false, -- Task list is too small in popup
		}
	},
	-- Mail
	{ rule = { class = "thunderbird" },
		properties = {
			tag = desktops.tags_names[3],
		}
	},
	{ rule = { class = "thunderbird", instance = "Msgcompose" },
		properties = {
			tag = desktops.tags_names[3],
			floating = true,
			ontop = true,
			callback = function(c)
				-- left 50%
				sreen_geometry = screens.get_primary().geometry
				c:geometry( { width = sreen_geometry.width / 2 , height = sreen_geometry.height } )
				awful.placement.bottom_left(c)
			end
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

return rules
