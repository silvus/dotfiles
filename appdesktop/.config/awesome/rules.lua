
local awful = require("awful")
local beautiful = require("beautiful")
local desktops = require("desktops")
local screens = require("screens")
local globalclient = client
local clients = require("clients")


-- Rules to apply to new clients (through the "manage" signal).
local rules = {
	-- Generics rules
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
			placement = awful.placement.no_overlap + awful.placement.no_offscreen + awful.placement.top,
			-- floating = false,
			-- maximized_vertical = false,
			-- maximized_horizontal = false,
		}
	},

	-- Dialogs client
	-- { rule_any = { type = { "dialog" } },
	-- 	properties = {
	-- 		placement = awful.placement.no_offscreen + awful.placement.centered
	-- 	}
	-- },

	-- Normal client
	{ rule_any = { type = { "normal" } },
		properties = {
			screen = screens.get_primary(),
		}
	},

	-- Floating clients
	{
		rule_any = {
			instance = {
				"DTA",  -- Firefox addon DownThemAll.
				"copyq",  -- Includes session name in class.
				"pinentry",
			},
			class = {
				"Arandr",
				"Blueman-manager",
				"Gpick",
				"Kruler",
				"MessageWin",  -- kalarm.
				"Sxiv",
				"Wpa_gui",
				"pinentry",
				"veromix",
				"xtightvncviewer",
				"Gcr-prompter",  -- Gnome password prompt
			},
			-- Note that the name property shown in xprop might be set slightly after creation of the client
			-- and the name shown there might not match defined rules here.
			name = {
				"Event Tester",  -- xev
			},
			title = {
				"Event Tester",  -- xev
			},
			type = {
				"dialog"
			},
			role = {
				"AlarmWindow",  -- Thunderbird's calendar.
				"ConfigManager",  -- Thunderbird's about:config.
				-- "pop-up",  -- e.g. Google Chrome's (detached) Developer Tools.
			}
		},
		properties = {
			floating = true,
			placement = awful.placement.no_offscreen + awful.placement.centered
		}
	},

	-- Specifics rules

	-- Web
	{ rule_any = { class = {"Firefox"} },
		except = { type = "dialog" },
		properties = {
			tag = desktops.tags_names[1],
		}
	},
	-- Dev
	{ rule_any = { class = { "VSCodium", "Zim" }},
		properties = {
			tag = desktops.tags_names[2],
		}
	},
	{ rule_any = { class = { "jetbrains-phpstorm" }},
		except = { type = "dialog" },
		properties = {
			tag = desktops.tags_names[3],
			-- screen = screens.count(),
			floating = false, -- Task list is too small in popup
		}
	},
	-- Mail
	{ rule_any = { class = {"thunderbird", "Thunderbird"} },
		properties = {
			tag = desktops.tags_names[4],
		}
	},
	{ rule_any = { instance = {"Msgcompose"} },
		properties = {
			tag = desktops.tags_names[4],
			floating = true,
			ontop = true,
			callback = function(c)
				-- snap left 50%
				local f = awful.placement.scale
					+ awful.placement.top_left
					+ awful.placement.maximize_vertically
				f(c, {honor_workarea=true, to_percent = 0.5})
			end
		}
	},
	-- Files explorer
	{ rule_any = { class = {"Pcmanfm"} },
		properties = {
			tag = desktops.tags_names[5],
		}
	},
	-- Mixed
	{ rule_any = { class = { "Godot", "Keybase", "balena-etcher-electron", "GParted", "Transmission" }},
		properties = {
			tag = desktops.tags_names[6],
		}
	},
	-- Graphics
	{ rule_any = { class = { "Gimp", "Krita", }},
		properties = {
			tag = desktops.tags_names[7],
		}
	},
	-- Office
	{ rule_any = { class = { "libreoffice-startcenter","libreoffice-writer", "libreoffice-calc", "Evince", "Simple-scan" }, instance = {"libreoffice"}},
		properties = {
			tag = desktops.tags_names[7],
		}
	},
	-- Games
	{ rule_any = { class = {"Steam"} },
		properties = {
			tag = desktops.tags_names[8],
		}
	},
	-- Ksnip (screenshots)
	{ rule_any = { class = {"ksnip"} },
		properties = {
			-- Floating on top and sticky
			floating = true,
			sticky = true,
			ontop = true,
			screen = screens.count(),
			placement = awful.placement.no_offscreen + awful.placement.top,
		}
	},
	-- MPV
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
				-- -- 2/5 bottom right of primary screen
				-- sreen_geometry = screens.get_primary().geometry
				-- c:geometry( { width = sreen_geometry.width * 2 / 5 , height = sreen_geometry.height * 2 / 5 } )
				-- awful.placement.bottom_right(c)
				-- snap left 50%
				local f = awful.placement.scale
					+ awful.placement.bottom_right
				f(c, {honor_workarea=true, to_percent = 0.4})
			end
		}
	},
}

return rules
