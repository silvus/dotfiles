-- ---------------------------------------------------------------------
-- Init
-- ---------------------------------------------------------------------

local os      = { getenv = os.getenv, setlocale = os.setlocale }
local theme   = {}

-- ---------------------------------------------------------------------
-- Config
-- ---------------------------------------------------------------------

theme.info   									= "#494B4F"
theme.error   									= "#D64937"
-- theme.error   									= "#C10004"
-- theme.success  									= "#8bdd58"
theme.success  									= "#00A5FF"

theme.dir                                       = os.getenv("HOME") .. "/.config/awesome/themes/thetheme"
theme.wallpaper                                 = theme.dir .. "/wallpaper.png"
theme.font                                      = "Monospace 9"

theme.fg_normal                                 = theme.info
theme.bg_normal                                 = "#111111"

theme.fg_focus                                  = "#BBBBBB"
theme.bg_focus                                  = "#00141a"

theme.fg_urgent                                 = "#FFFFFF"
theme.bg_urgent                                 = theme.error

theme.taglist_fg_normal                         = theme.fg_normal
theme.taglist_fg_focus                          = "#FFFFFF"
theme.taglist_bg_normal                         = theme.bg_normal
theme.taglist_bg_focus                          = theme.success

theme.tasklist_spacing 							= 10

theme.titlebar_bg_normal                        = "#333333"
theme.titlebar_bg_focus                         = "#00141a"

theme.border_normal                             = "#141414"
theme.border_focus                              = theme.success
theme.border_width                              = 2

-- theme.useless_gap                               = 5
-- theme.gap_single_client                         = false
theme.useless_gap                               = 0
theme.gap_single_client                         = true

theme.menu_height                               = 16
theme.menu_width                                = 250

theme.ac                                        = theme.dir .. "/icons/ac.png"
theme.awesome_icon                              = theme.dir .. "/icons/awesome.png"
-- theme.awesome_icon                              = theme.dir .. "/icons/awesome_icon_white.png"
theme.awesome_icon_launcher                     = theme.dir .. "/icons/awesome_icon.png"
theme.bar                                       = theme.dir .. "/icons/bar.png"
theme.bat                                       = theme.dir .. "/icons/bat.png"
theme.bat_low                                   = theme.dir .. "/icons/bat_low.png"
theme.bat_no                                    = theme.dir .. "/icons/bat_no.png"
theme.battery                                   = theme.dir .. "/icons/battery.png"
theme.battery_empty                             = theme.dir .. "/icons/battery_empty.png"
theme.battery_low                               = theme.dir .. "/icons/battery_low.png"
theme.bottom_bar                                = theme.dir .. "/icons/bottom_bar.png"
theme.calendar                                  = theme.dir .. "/icons/cal.png"
theme.clock                                     = theme.dir .. "/icons/clock.png"
theme.cpu                                       = theme.dir .. "/icons/cpu.png"
theme.disk                                      = theme.dir .. "/icons/disk.png"
theme.hdd                                       = theme.dir .. "/icons/hdd.png"
theme.layout_dwindle                            = theme.dir .. "/icons/dwindle.png"
theme.layout_fairh                              = theme.dir .. "/icons/fairh.png"
theme.layout_fairv                              = theme.dir .. "/icons/fairv.png"
theme.layout_floating                           = theme.dir .. "/icons/floating.png"
theme.layout_fullscreen                         = theme.dir .. "/icons/fullscreen.png"
theme.layout_magnifier                          = theme.dir .. "/icons/magnifier.png"
theme.layout_max                                = theme.dir .. "/icons/max.png"
theme.layout_spiral                             = theme.dir .. "/icons/spiral.png"
theme.layout_tile                               = theme.dir .. "/icons/tile.png"
theme.layout_tilebottom                         = theme.dir .. "/icons/tilebottom.png"
theme.layout_tileleft                           = theme.dir .. "/icons/tileleft.png"
theme.layout_tiletop                            = theme.dir .. "/icons/tiletop.png"
theme.mail                                      = theme.dir .. "/icons/mail.png"
theme.mail_on                                   = theme.dir .. "/icons/mail_on.png"
theme.mem                                       = theme.dir .. "/icons/mem.png"
theme.menu_submenu_icon                         = theme.dir .. "/icons/submenu.png"
theme.mpd_on                                    = theme.dir .. "/icons/mpd_on.png"
theme.mpdl                                      = theme.dir .. "/icons/mpd.png"
theme.music                                     = theme.dir .. "/icons/note.png"
theme.music_on                                  = theme.dir .. "/icons/note_on.png"
theme.net                                       = theme.dir .. "/icons/net.png"
theme.net_down                                  = theme.dir .. "/icons/net_down.png"
theme.net_up                                    = theme.dir .. "/icons/net_up.png"
theme.net_wired                                 = theme.dir .. "/icons/net_wired.png"
theme.next                                       = theme.dir .. "/icons/next.png"
theme.pause                                     = theme.dir .. "/icons/pause.png"
theme.play                                      = theme.dir .. "/icons/play.png"
theme.prev                                      = theme.dir .. "/icons/prev.png"
theme.spr_bottom_right                          = theme.dir .. "/icons/spr_bottom_right.png"
theme.spr_left                                  = theme.dir .. "/icons/spr_left.png"
theme.spr_right                                 = theme.dir .. "/icons/spr_right.png"
theme.spr_small                                 = theme.dir .. "/icons/spr_small.png"
theme.spr_very_small                            = theme.dir .. "/icons/spr_very_small.png"
theme.stop                                      = theme.dir .. "/icons/stop.png"
theme.taglist_squares_sel                       = theme.dir .. "/icons/square_sel.png"
theme.taglist_squares_unsel                     = theme.dir .. "/icons/square_unsel.png"
theme.temp                                      = theme.dir .. "/icons/temp.png"
theme.titlebar_close_button_focus               = theme.dir .. "/icons/close_focus.png"
theme.titlebar_close_button_normal              = theme.dir .. "/icons/close_normal.png"
theme.titlebar_floating_button_focus_active     = theme.dir .. "/icons/floating_focus_active.png"
theme.titlebar_floating_button_focus_inactive   = theme.dir .. "/icons/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active    = theme.dir .. "/icons/floating_normal_active.png"
theme.titlebar_floating_button_normal_inactive  = theme.dir .. "/icons/floating_normal_inactive.png"
theme.titlebar_maximized_button_focus_active    = theme.dir .. "/icons/maximized_focus_active.png"
theme.titlebar_maximized_button_focus_inactive  = theme.dir .. "/icons/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active   = theme.dir .. "/icons/maximized_normal_active.png"
theme.titlebar_maximized_button_normal_inactive = theme.dir .. "/icons/maximized_normal_inactive.png"
theme.titlebar_minimize_button_focus            = theme.dir .. "/icons/minimize_focus.png"
theme.titlebar_minimize_button_normal           = theme.dir .. "/icons/minimize_normal.png"
theme.titlebar_ontop_button_focus_active        = theme.dir .. "/icons/ontop_focus_active.png"
theme.titlebar_ontop_button_focus_inactive      = theme.dir .. "/icons/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active       = theme.dir .. "/icons/ontop_normal_active.png"
theme.titlebar_ontop_button_normal_inactive     = theme.dir .. "/icons/ontop_normal_inactive.png"
theme.titlebar_sticky_button_focus_active       = theme.dir .. "/icons/sticky_focus_active.png"
theme.titlebar_sticky_button_focus_inactive     = theme.dir .. "/icons/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active      = theme.dir .. "/icons/sticky_normal_active.png"
theme.titlebar_sticky_button_normal_inactive    = theme.dir .. "/icons/sticky_normal_inactive.png"
theme.vol                                       = theme.dir .. "/icons/vol.png"
theme.vol_low                                   = theme.dir .. "/icons/vol_low.png"
theme.vol_mute                                  = theme.dir .. "/icons/vol_mute.png"
theme.vol_no                                    = theme.dir .. "/icons/vol_no.png"

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = nil

-- Awesome >= 4.1
-- theme.tasklist_disable_task_name = true
-- theme.tasklist_plain_task_name = true
--theme.tasklist_disable_icon = true

-- lain related
theme.layout_centerfair                         = theme.dir .. "/icons/centerfair.png"
theme.layout_termfair                           = theme.dir .. "/icons/termfair.png"
theme.layout_centerwork                         = theme.dir .. "/icons/centerwork.png"

return theme
