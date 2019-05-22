local awful = require("awful")
local beautiful = require("beautiful")
local screens = require("screens")
local keybindings = require("keybindings")


local rules = {
	-- All clients will match this rule
	{ rule = { },
		properties = {
			border_width = beautiful.border_width,
			border_color = beautiful.border_normal,
			focus = awful.client.focus.filter,
			raise = true,
			keys = keybindings.clientkeys,
			buttons = keybindings.clientbuttons,
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
			maximized_horizontal = false
		}
	},

	-- Specifics rules
	{ rule = { class = "Firefox" },
		properties = {
			tag = "1",
			screen = screens.get_primary(),
		}
	},
	{ rule = { class = "Steam" },
		properties = {
			tag = "8",
			screen = screens.get_primary(),
		}
	},
	-- Mail special tag
	{ rule = { class = "Thunderbird" },
		properties = {
			tag = "M",
			screen = screens.get_primary(),
		}
	},
	-- Scratchpad
	{ rule = { class = "Zim" },
		properties = {
			tag = "S",
			screen = screens.get_primary(),
			floating = false,  -- Task list is too small in popup
		}
	},
}
-- }}}

-- Specific for multi screens
if screens.count() > 1 then
	-- On dual screens
	table.insert(rules, { rule = { class = "mpv" },
		properties = {
			-- Fullscreen on secondary screen
			focus = false,
			floating = false,
			fullscreen = true,
			-- maximized_vertical = true,
			-- maximized_horizontal = true,
			screen = screens.count(),
			-- switchtotag = true,
			-- titlebars_enabled = false,
			-- sticky = true,
		}
	})
	table.insert(rules, { rule_any = { class = {
			"Code",
			"VSCodium",
			"krita",
			"Sublime_text",
			"jetbrains-phpstorm" }
		},
		except = { type = "dialog" },
		properties = {
			tag = "1",
			screen = screens.get_vertical(),
		}
	})
	table.insert(rules, { rule = { instance = "www.netflix.com__browse" },
		properties = {
			-- Fullscreen on secondary screen
			focus = false,
			floating = false,
			fullscreen = true,
			screen = screens.count()
		}
	})
else
	-- Single screen
	table.insert(rules, { rule = { class = "mpv" },
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
	})
	table.insert(rules, { rule_any = { class = {
			"Code",
			"VSCodium",
			"krita",
			"Sublime_text",
			"jetbrains-phpstorm" }
		},
		except = { type = "dialog" },
		properties = {
			tag = "2",
			screen = screens.get_vertical(),
		}
	})
	table.insert(rules, { rule = { instance = "www.netflix.com__browse" },
		properties = {
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
	})
end

return rules
