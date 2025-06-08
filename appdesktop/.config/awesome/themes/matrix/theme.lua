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

theme.name = "matrix"

theme.info                                      = "#074d0b"
theme.error                                     = "#b01c09"
theme.success                                   = "#009914"
theme.primary                                   = "#074f00"

theme.notification_position                     = "bottom_right"
theme.notification_max_width                    = 500
theme.notification_icon_size                    = 50

theme.dir                                       = gears.filesystem.get_configuration_dir() .. "themes/" .. theme.name
theme.wallpaper                                 = theme.dir .. "/wallpaper.png"
theme.font                                      = "DejaVu Sans Mono 9"

theme.fg_normal                                 = theme.info
theme.bg_normal                                 = "#111111"

theme.fg_focus                                  = "#BBBBBB"
theme.bg_focus                                  = theme.primary

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

theme.titlebar_fg_normal                        = "#333333"
theme.titlebar_bg_normal                        = "#1c2b25"
theme.titlebar_bg_focus                         = "#022603"

theme.snap_bg                                   = theme.primary
theme.snap_border_width                         = 5

theme.border_normal                             = "#141414"
theme.border_focus                              = theme.success
theme.border_width                              = 2

-- theme.useless_gap                               = 5
-- theme.gap_single_client                         = true
theme.gap_single_client                         = false
theme.useless_gap                               = 4

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
	theme.icon_theme = nil
end

-- Disable the tasklist client titles.
theme.tasklist_disable_task_name = true
--  Disable the extra tasklist client property notification icons.
theme.tasklist_plain_task_name = false
-- theme.tasklist_disable_icon = true
-- theme.maximized_hide_border = true
-- theme.fullscreen_hide_border = true
theme.master_width_factor = config.layouts_master_width

-- Bar (Wibar) management

function widget_rotate(w, inverse)
	local direction = 'east'
	if inverse then
		direction = 'west'
	end

	return wibox.container {
		w,
		direction = direction,
		widget = wibox.container.rotate
	}
end

-- Build a bar
function bar(s)

	-- Customs widgets definitions
	-- Import need to be done after beautiful init or colors are not defined
	local widget_separator = require("widgets.separator")
	local widget_separator_vertical = require("widgets.separator_vertical")
	local widget_layout = require("widgets.layout")
	local widget_tags = require("widgets.tags")
	local widget_tags_vertical = require("widgets.tags_vertical")
	local widget_tasks = require("widgets.tasks")
	local widget_tasks_vertical = require("widgets.tasks_vertical")
	local widget_clock = require("widgets.clock")
	local widget_clock_vertical = require("widgets.clock_vertical")
	local widget_volumecapture = require("widgets.volumecapture")
	local widget_volume = require("widgets.volume")
	local widget_notifications = require("widgets.notifications")
	local widget_cpu = require("widgets.cpu")
	local widget_ram = require("widgets.ram")
	local widget_net = require("widgets.net")
	local widget_vpn = require("widgets.vpn")
	local widget_moc = require("widgets.moc")
	local widget_systray = require("widgets.systray")
	-- local widget_prompt = require("widgets.prompt")
	local widget_keyboardlayout = require("widgets.keyboardlayout")
	local widget_battery = require("widgets.battery")

	-- Create an imagebox widget which will contains an icon indicating which layout we're using. One layoutbox per screen.
	local layoutbox = widget_layout.widget(s)

	local wibox_custom = nil

	-- Widget for main screen only
	if s == screens.get_primary() then
		-- Create a promptbox (on screen object to trigger in keys bindings)
		-- s.promptbox = widget_prompt.widget

		-- Create a vertical wibox
		wibox_custom = awful.wibar({
			position = "left",
			screen = s,
			visible = config.show_bar,
			bg = theme.bg_normal .. "bf" -- add the alpha value to the color (where "00" would be completely transparent and "ff" would be no transparency
		})

		-- Add widgets to the wibox
		wibox_custom:setup {
			layout = wibox.layout.align.vertical,
			{ -- Left widgets
				layout = wibox.layout.fixed.vertical,
				widget_tags_vertical.widget(s),
				widget_separator_vertical.widget,
				-- s.promptbox,
			},
			{ -- Middle widget
				layout = wibox.layout.fixed.vertical,
				widget_tasks_vertical.widget(s),
			},
			{ -- Right widgets
				layout = wibox.layout.fixed.vertical,
				widget_moc.icon,
				widget_moc.widgetbar,
				-- widget_rotate(widget_moc.widget),
				widget_vpn.icon,
				-- widget_vpn.widget,
				widget_battery.icon,
				widget_rotate(widget_battery.widget),
				widget_separator_vertical.widget,
				widget_net.icon,
				widget_rotate(widget_net.widget, true),
				widget_separator_vertical.widget,
				widget_cpu.icon,
				widget_rotate(widget_cpu.widget, true),
				widget_separator_vertical.widget,
				widget_ram.icon,
				widget_rotate(widget_ram.widget),
				widget_rotate(widget_keyboardlayout.widget),
				widget_separator_vertical.widget,
				widget_notifications.widget,
				widget_volumecapture.widget,
				widget_volume.widget,
				widget_separator_vertical.widget,
				widget_rotate(widget_systray.widget),
				widget_separator_vertical.widget,
				widget_clock.icon,
				widget_clock_vertical.widget,
				widget_separator_vertical.widget,
				{
					layoutbox,
					layout = wibox.container.margin(layoutbox ,0 ,0 ,0 ,-5)
				},
			},
		}

	else
		-- secondary screen (always horizontal)
		wibox_custom = awful.wibar({
			position = "top",
			screen = s,
			visible = false,
			--height = 25
		})
		wibox_custom:setup {
			layout = wibox.layout.align.horizontal,
			{ -- Left widgets
				layout = wibox.layout.fixed.horizontal,
				widget_tags.widget(s),
			},
			{ -- Middle widget
				layout = wibox.layout.fixed.horizontal,
				widget_tasks.widget(s),
			},
			{ -- Right widgets
				layout = wibox.layout.fixed.horizontal,
				widget_separator.widget,
				layoutbox,
			},
		}
	end

	return wibox_custom
end


theme.bar = bar


return theme

