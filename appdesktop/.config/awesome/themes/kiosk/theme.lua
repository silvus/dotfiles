-- Minimal theme for single-purpose kiosk boxes (fullscreen media player).
-- See the "Keybindings override" and "Rules override" sections below for
-- how this theme can diverge from the default profile without touching
-- keys.lua / rules.lua.

-- ---------------------------------------------------------------------
-- Init
-- ---------------------------------------------------------------------

local awful  = require("awful")
local gears  = require("gears")
local wibox = require("wibox")
local screens = require("screens")
local config = require('config')

local theme = {}

-- ---------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------

theme.name = "kiosk"

-- Catppuccin Mocha: https://github.com/catppuccin/catppuccin
theme.info                                      = "#89b4fa" -- blue
theme.error                                     = "#f38ba8" -- red
theme.success                                   = "#a6e3a1" -- green
theme.primary                                   = "#cba6f7" -- mauve

theme.notification_position                     = "bottom_right"
theme.notification_max_width                    = 500
theme.notification_icon_size                    = 50

theme.dir                                       = gears.filesystem.get_configuration_dir() .. "themes/" .. theme.name
theme.wallpaper                                 = theme.dir .. "/wallpaper.jpg"
theme.font                                      = "DejaVu Sans Mono 9"

theme.fg_normal                                 = theme.info
theme.bg_normal                                 = "#1e1e2e" -- base

theme.fg_focus                                  = "#cdd6f4" -- text
theme.bg_focus                                  = "#313244" -- surface0

theme.fg_urgent                                 = "#FFFFFF"
theme.bg_urgent                                 = theme.error

theme.taglist_fg_normal                         = theme.fg_normal
theme.taglist_fg_focus                          = "#FFFFFF"
theme.taglist_bg_normal                         = theme.bg_normal
theme.taglist_bg_focus                          = theme.success

theme.tasklist_fg_focus                         = theme.fg_normal
theme.tasklist_bg_focus                         = theme.bg_normal
theme.tasklist_spacing                          = 15
theme.tasklist_fg_normal                        = "#FFFFFF"

-- Height/width rotated
theme.graph_height                              = 15
theme.graph_width                               = 30

theme.titlebar_fg_normal                        = "#333333"
theme.titlebar_bg_normal                        = "#181825" -- mantle
theme.titlebar_bg_focus                         = "#45475a" -- surface1

theme.snap_bg                                   = theme.primary
theme.snap_border_width                         = 5

theme.border_normal                             = "#11111b" -- crust
theme.border_focus                              = theme.primary
theme.border_width                              = 2

-- Kiosk: apps run fullscreen, no need for gaps
theme.gap_single_client                         = false
theme.useless_gap                               = 0

theme.menu_height                               = 16
theme.menu_width                                = 250

theme.ac                                        = theme.dir .. "/icons/ac.png"
theme.arrow_left                                = theme.dir .. "/icons/arrow_left.png"
theme.battery                                   = theme.dir .. "/icons/battery.png"
theme.battery_empty                             = theme.dir .. "/icons/battery_empty.png"
theme.battery_low                               = theme.dir .. "/icons/battery_low.png"
theme.bolt                                      = theme.dir .. "/icons/bolt.png"
theme.clock                                     = theme.dir .. "/icons/clock.png"
theme.code                                      = theme.dir .. "/icons/code.png"
theme.cpu                                       = theme.dir .. "/icons/cpu.png"
theme.fire                                      = theme.dir .. "/icons/fire.png"
theme.firefox                                   = theme.dir .. "/icons/firefox.png"
theme.folder                                    = theme.dir .. "/icons/folder.png"
theme.gamepad                                   = theme.dir .. "/icons/gamepad.png"
theme.hdd                                       = theme.dir .. "/icons/hdd.png"
theme.lock                                      = theme.dir .. "/icons/lock.png"
theme.mail                                      = theme.dir .. "/icons/mail.png"
theme.mail_on                                   = theme.dir .. "/icons/mail_on.png"
theme.mem                                       = theme.dir .. "/icons/mem.png"
theme.microphone                                = theme.dir .. "/icons/microphone.png"
theme.microphone_off                            = theme.dir .. "/icons/microphone_off.png"
theme.music                                     = theme.dir .. "/icons/music.png"
theme.music_note                                = theme.dir .. "/icons/music_note.png"
theme.music_note_on                             = theme.dir .. "/icons/music_note_on.png"
theme.note                                      = theme.dir .. "/icons/note.png"
theme.net                                       = theme.dir .. "/icons/net.png"
theme.paint                                     = theme.dir .. "/icons/paint.png"
theme.paragraph                                 = theme.dir .. "/icons/paragraph.png"
theme.shield                                    = theme.dir .. "/icons/shield.png"
theme.terminal                                  = theme.dir .. "/icons/terminal.png"
theme.bell                                      = theme.dir .. "/icons/bell.png"
theme.bell_slash                                = theme.dir .. "/icons/bell-slash.png"
theme.vol                                       = theme.dir .. "/icons/vol.png"
theme.vol_low                                   = theme.dir .. "/icons/vol_low.png"
theme.vol_mute                                  = theme.dir .. "/icons/vol_mute.png"
theme.vol_no                                    = theme.dir .. "/icons/vol_no.png"
theme.volcapture                                = theme.dir .. "/icons/volcapture.png"
theme.volcapture_low                            = theme.dir .. "/icons/volcapture_low.png"
theme.volcapture_mute                           = theme.dir .. "/icons/volcapture_mute.png"
theme.volcapture_no                             = theme.dir .. "/icons/volcapture_no.png"

theme.titlebar_close_button_focus               = theme.dir .. "/icons/titlebar/close_focus.png"
theme.titlebar_close_button_normal              = theme.dir .. "/icons/titlebar/close_normal.png"
theme.titlebar_floating_button_focus_active     = theme.dir .. "/icons/titlebar/floating_focus_active.png"
theme.titlebar_floating_button_focus_inactive   = theme.dir .. "/icons/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active    = theme.dir .. "/icons/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_normal_inactive  = theme.dir .. "/icons/titlebar/floating_normal_inactive.png"
theme.titlebar_maximized_button_focus_active    = theme.dir .. "/icons/titlebar/maximized_focus_active.png"
theme.titlebar_maximized_button_focus_inactive  = theme.dir .. "/icons/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active   = theme.dir .. "/icons/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_normal_inactive = theme.dir .. "/icons/titlebar/maximized_normal_inactive.png"
theme.titlebar_minimize_button_focus            = theme.dir .. "/icons/titlebar/minimize_focus.png"
theme.titlebar_minimize_button_normal           = theme.dir .. "/icons/titlebar/minimize_normal.png"
theme.titlebar_ontop_button_focus_active        = theme.dir .. "/icons/titlebar/ontop_focus_active.png"
theme.titlebar_ontop_button_focus_inactive      = theme.dir .. "/icons/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active       = theme.dir .. "/icons/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_normal_inactive     = theme.dir .. "/icons/titlebar/ontop_normal_inactive.png"
theme.titlebar_sticky_button_focus_active       = theme.dir .. "/icons/titlebar/sticky_focus_active.png"
theme.titlebar_sticky_button_focus_inactive     = theme.dir .. "/icons/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active      = theme.dir .. "/icons/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_normal_inactive    = theme.dir .. "/icons/titlebar/sticky_normal_inactive.png"

theme.layout_centerfair                         = theme.dir .. "/icons/layout/centerfair.png"
theme.layout_centerwork                         = theme.dir .. "/icons/layout/centerwork.png"
theme.layout_dwindle                            = theme.dir .. "/icons/layout/dwindle.png"
theme.layout_fairh                              = theme.dir .. "/icons/layout/fairh.png"
theme.layout_fairv                              = theme.dir .. "/icons/layout/fairv.png"
theme.layout_floating                           = theme.dir .. "/icons/layout/floating.png"
theme.layout_fullscreen                         = theme.dir .. "/icons/layout/fullscreen.png"
theme.layout_magnifier                          = theme.dir .. "/icons/layout/magnifier.png"
theme.layout_max                                = theme.dir .. "/icons/layout/max.png"
theme.layout_spiral                             = theme.dir .. "/icons/layout/spiral.png"
theme.layout_termfair                           = theme.dir .. "/icons/layout/termfair.png"
theme.layout_tile                               = theme.dir .. "/icons/layout/tile.png"
theme.layout_tilebottom                         = theme.dir .. "/icons/layout/tilebottom.png"
theme.layout_tileleft                           = theme.dir .. "/icons/layout/tileleft.png"
theme.layout_tiletop                            = theme.dir .. "/icons/layout/tiletop.png"

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
if awful.util.is_dir("/usr/share/icons/Numix") then
	theme.icon_theme = "Numix"
elseif awful.util.is_dir("/usr/share/icons/Papirus") then
	theme.icon_theme = "Papirus"
elseif awful.util.is_dir("/usr/share/icons/Faba") then
	theme.icon_theme = "Faba"
elseif awful.util.is_dir("/usr/share/icons/menta") then
	theme.icon_theme = "menta"
else
	-- Default for NixOs
	theme.icon_theme = "Numix"
end

-- Disable the tasklist client titles.
theme.tasklist_disable_task_name = true
--  Disable the extra tasklist client property notification icons.
theme.tasklist_plain_task_name = false

theme.master_width_factor = config.layouts_master_width

-- Kiosk: every desktop starts fullscreen (single app, no tiling).
-- desktops.lua uses this as the default for any tag that doesn't set its
-- own `layout` explicitly.
theme.tags_default_layout = awful.layout.suit.max

-- Kiosk: plain numbered tags, no icons. Overrides the default icon-based
-- tag list defined in desktops.lua (same 10 tags, same names/order, so
-- keybindings that index screen.tags[i] -- e.g. the scratchpad at [10] --
-- still work unchanged).
theme.tags = {
	{ name = "1" },
	{ name = "2" },
	{ name = "3" },
	{ name = "4" },
	{ name = "5" },
	{ name = "6" },
	{ name = "7" },
	{ name = "8" },
	{ name = "9" },
	{ name = "0" },
}

-- ---------------------------------------------------------------------
-- Rules override (optional)
-- ---------------------------------------------------------------------
-- rules.lua appends theme.rules (if set) after its own rules, so entries
-- here can add new rules or override the tag/properties of a class that
-- the shared rules.lua already matches (later rule wins).
--
-- Here: the "movies" media explorer normally lands on tag 8 (see the
-- "Mixed" rule in rules.lua) but on this kiosk it should open on tag 1.
theme.rules = {
	{
		rule_any = { class = { "movies" } },
		properties = {
			tag = "1",
		}
	},
}

-- ---------------------------------------------------------------------
-- Keybindings override (optional, not used yet)
-- ---------------------------------------------------------------------
-- This theme currently keeps the default keybindings from keys.lua.
-- To fully replace them for this profile, set both of these before
-- `return theme` (keys.lua checks for them once beautiful.init has run):
--
-- Use config.modkey here, not the bare `modkey` global -- that global is
-- only set once keys.lua itself runs, which happens *after* beautiful.init
-- (and thus after this file), so it would still be nil at this point.
--
-- theme.keys_global = awful.util.table.join(
-- 	awful.key({ config.modkey }, "q", function()
-- 		awful.spawn("systemctl --user restart mediaplayer")
-- 	end, { description = "restart media player", group = "kiosk" })
-- )
-- theme.keys_clients = {
-- 	keys = awful.util.table.join(
-- 		awful.key({ config.modkey }, "F4", function(c)
-- 			c:kill()
-- 		end, { description = "close", group = "client" })
-- 	),
-- 	buttons = awful.util.table.join(
-- 		awful.button({}, 1, function(c)
-- 			client.focus = c
-- 			c:raise()
-- 		end)
-- 	),
-- }

-- ---------------------------------------------------------------------
-- Bar (Wibar) management
-- ---------------------------------------------------------------------
local function bar(s)
	local widget_rotate = require("widgets.rotate")
	local widget_tags_vertical = require("widgets.tags_vertical")
	local widget_systray = require("widgets.systray")

	local wibox_custom = awful.wibar({
		position = "left",
		screen = s,
		width = 20,
		visible = (s == screens.get_primary()) and config.show_bar or false,
		bg = theme.bg_normal .. "bf" -- add the alpha value to the color (where "00" would be completely transparent and "ff" would be no transparency
	})

	wibox_custom:setup {
		layout = wibox.layout.align.vertical,
		{ -- Top widget
			layout = wibox.layout.fixed.vertical,
			widget_tags_vertical.widget(s),
		},
		nil, -- Middle (unused)
		{ -- Bottom widget
			layout = wibox.layout.fixed.vertical,
			widget_rotate(widget_systray.widget),
		},
	}

	return wibox_custom
end


theme.bar = bar


return theme
