local awful = require("awful")
local beautiful = require("beautiful")
local desktops = require("desktops")
local config = require("config")
local screens = require("screens")
local globalclient = client
local keys = require("keys")


local function get_video_rule(c)
    if (config.rules_videos == "auto" and screens.count() > 1) or config.rules_videos == "fullscreen" then
        -- Auto or fullscreen modes. Auto is full screen with more than 1 screen

        -- Full screen on last screen
        c.sticky = false
        c.fullscreen = true
        c.floating = false
        c.ontop = false            -- Not compatible with fullscreen
        c.screen = screens.count() -- On last screen
    elseif config.rules_videos == "tiled" then
        -- Tiled but sticky
        c.sticky = true
        c.fullscreen = false
        c.floating = false
        c.ontop = true                   -- Not compatible with fullscreen
        c.screen = screens.get_primary() -- On primary screen
    else
        -- Défault to float mode

        -- Sticky in corner on main screen
        c.sticky = true
        c.fullscreen = false
        c.floating = true
        c.ontop = true                   -- Not compatible with fullscreen
        c.screen = screens.get_primary() -- On primary screen
        -- -- 2/5 bottom right of primary screen
        -- sreen_geometry = screens.get_primary().geometry
        -- c:geometry( { width = sreen_geometry.width * 2 / 5 , height = sreen_geometry.height * 2 / 5 } )
        -- awful.placement.bottom_right(c)
        -- snap left 50%
        local f = awful.placement.scale + awful.placement.bottom_right
        f(c, { honor_workarea = true, to_percent = 0.4 })
    end

    return c
end

-- Rules to apply to new clients (through the "manage" signal).
local rules = {
    -- Generics rules
    {
        rule = {},
        properties = {
            border_width = beautiful.border_width,
            border_color = beautiful.border_normal,
            focus = awful.client.focus.filter,
            raise = true,
            keys = keys.clients.keys,
            buttons = keys.clients.buttons,
            screen = awful.screen.preferred,
            titlebars_enabled = false,
            placement = awful.placement.no_overlap + awful.placement.no_offscreen + awful.placement.top,
            -- floating = false,
            -- maximized = false,
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
    {
        rule_any = { type = { "normal" } },
        properties = {
            screen = screens.get_primary(),
        }
    },

    -- Floating clients
    {
        rule_any = {
            instance = {
                "DTA",   -- Firefox addon DownThemAll.
                "copyq", -- Includes session name in class.
                "pinentry",
            },
            class = {
                "Arandr",
                "Blueman-manager",
                "Gpick",
                "Kruler",
                "MessageWin", -- kalarm.
                "Sxiv",
                "Wpa_gui",
                "pinentry",
                "veromix",
                "xtightvncviewer",
                "Gcr-prompter", -- Gnome password prompt
            },
            -- Note that the name property shown in xprop might be set slightly after creation of the client
            -- and the name shown there might not match defined rules here.
            name = {
                "Event Tester", -- xev
            },
            title = {
                "Event Tester", -- xev
            },
            type = {
                "dialog"
            },
            role = {
                "AlarmWindow",   -- Thunderbird's calendar.
                "ConfigManager", -- Thunderbird's about:config.
                -- "pop-up",  -- e.g. Google Chrome's (detached) Developer Tools.
            }
        },
        properties = {
            floating = true,
            sticky = true,
            ontop = true,
            modal = true,
            placement = awful.placement.no_overlap + awful.placement.no_offscreen + awful.placement.top,
        }
    },

    -- Specifics rules

    -- Web
    {
        rule_any = { class = { "firefox", "Firefox" } },
        -- except = { type = "dialog" },
        properties = {
            tag = desktops.tags_names[1],
            titlebars_enabled = false,
        }
    },
    -- Notes
    {
        rule_any = { class = { "obsidian", "Zim", "VNote" } },
        properties = {
            tag = desktops.tags_names[2],
            titlebars_enabled = false,
        }
    },
    -- Dev
    {
        rule_any = { class = { "VSCodium", "dev.zed.Zed", "lite-xl" } },
        properties = {
            tag = desktops.tags_names[3],
            titlebars_enabled = false,
        }
    },
    {
        rule = { class = "jetbrains-phpstorm" },
        properties = {
            tag = desktops.tags_names[3],
            titlebars_enabled = false,
            -- screen = screens.count(),
        }
    },
    -- Guake like terminal
    {
        rule_any = { instance = { "guaketerm", "tdropterm" }, class = { "guaketerm", "tdropterm" } },
        properties = {
            -- Only property needed on rules, other are set in quake.lua
            titlebars_enabled = false,
        }
    },
    -- Files explorer
    {
        rule_any = { class = { "Pcmanfm", "pcmanfm-qt", "Thunar", "Nemo" } },
        properties = {
            tag = desktops.tags_names[4],
        }
    },
    -- Mail
    {
        rule_any = { class = { "thunderbird", "Thunderbird" } },
        properties = {
            tag = desktops.tags_names[5],
            titlebars_enabled = false,
        }
    },
    {
        rule_any = { instance = { "Msgcompose" } },
        properties = {
            tag = desktops.tags_names[5],
            floating = true,
            ontop = true,
            callback = function(c)
                -- snap left 50%
                local f = awful.placement.scale
                    + awful.placement.top_left
                    + awful.placement.maximize_vertically
                f(c, { honor_workarea = true, to_percent = 0.5 })
            end
        }
    },

    -- Mixed
    {
        rule_any = {
            class =
            "Godot",
            "Keybase",
            "balena-etcher-electron",
            "GParted",
            "Transmission",
            "Slack",
            "JoalDesktop",
            "Pavucontrol",
            "Lutris",
            "discord",
        },
        properties = {
            tag = desktops.tags_names[6],
        }
    },
    -- Games
    {
        rule_any = {
            class = {
                "steam",
                "Steam",
            }
        },
        properties = {
            tag = desktops.tags_names[6],
        }
    },
    -- Office
    {
        rule_any = {
            instance = {
                "libreoffice",
            },
            class = {
                "libreoffice-startcenter",
                "libreoffice-writer",
                "libreoffice-calc",
                "libreoffice-draw",
                "libreoffice-impress",
                "Simple-scan",
                "Gnumeric",
            }
        },
        properties = {
            tag = desktops.tags_names[7],
        }
    },
    -- Graphics
    {
        rule_any = { class = { "Gimp", "Krita", "FreeCAD"} },
        properties = {
            tag = desktops.tags_names[8],
        }
    },
    -- Privacy
    {
        rule_any = { class = { "Tor Browser", "firefox-aurora" } },
        properties = {
            tag = desktops.tags_names[9],
            titlebars_enabled = false,
        }
    },
    -- Containers / VM
    {
        rule_any = { class = { "Remote-viewer" } },
        properties = {
            screen = screens.count(),
        }
    },
    -- Ksnip (screenshots)
    {
        rule_any = { class = { "ksnip" } },
        properties = {
            -- Floating on top and sticky
            floating = true,
            sticky = true,
            ontop = true,
            modal = true,
            placement = awful.placement.no_offscreen + awful.placement.top_right,
        }
    },
    -- Sonata (mpd client)
    {
        rule_any = { class = { "Sonata" } },
        properties = {
            -- Floating on top (no sticky)
            floating = true,
            ontop = true,
            placement = awful.placement.no_offscreen + awful.placement.left,
        }
    },
    -- MPV and streaming services
    {
        rule_any = {
            class = { "mpv", "Emby Theater" },
            instance = { "www.netflix.com__browse", "www.primevideo.com" },
            role = { "PictureInPicture" },
        },
        properties = {
            titlebars_enabled = true,
            -- focus = false,
            placement = awful.placement.no_offscreen + awful.placement.bottom_right,
            -- Floating on top and sticky or full screen on secondary
            callback = function(c)
                get_video_rule(c)
            end
        }
    },
}

-- Connect a signal on new client appears
-- https://superuser.com/questions/585058/how-to-move-just-opened-new-window-of-an-already-started-client-to-a-tag-automat?rq=1
-- client.connect_signal("manage",function(c,startup)
-- 	if (c.class == "Firefox") then
-- 		-- if it's a Firefox we will connect a signal which will call if 'name' changing
-- 		c:connect_signal("property::name",function(c)
-- 			-- if "(Private Browsing)" is part of 'c.name' then 'c' goes to tag 9
-- 			if (string.find(c.name,"(Private Browsing)")) then
-- 				local t = awful.tag.find_by_name(awful.screen.focused(), desktops.tags_names[9])
-- 				c:tags({t})
-- 			end
-- 		end)

-- 	elseif (c.class == "VirtualBox Machine") then
-- 		-- if it's a VirtualBox we will connect a signal which will call if 'name' changing
-- 		c:connect_signal("property::name",function(c)
-- 			-- if "devus" is part of 'c.name' then 'c' goes to tag x
-- 			if (string.find(c.name,"devus")) then
-- 				local t = awful.tag.find_by_name(awful.screen.focused(), 'x')
-- 				c:tags({t})
-- 			end
-- 		end)
-- 	end
-- end)


return rules
