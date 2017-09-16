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

theme.fg_urgent                                 = theme.error
theme.bg_urgent                                 = "#FFFFFF"

theme.taglist_fg_normal                         = theme.fg_normal
theme.taglist_fg_focus                          = "#FFFFFF"
theme.taglist_bg_normal                         = theme.bg_normal
theme.taglist_bg_focus                          = theme.success

theme.titlebar_bg_normal                        = "#333333"
theme.titlebar_bg_focus                         = "#00141a"

theme.border_normal                             = "#141414"
theme.border_focus                              = theme.success
theme.border_width                              = 2

theme.useless_gap                               = 5
theme.gap_single_client                         = false

theme.menu_height                               = 16
theme.menu_width                                = 250

theme.awesome_icon                              = theme.dir .. "/icons/awesome.png"
theme.menu_submenu_icon                         = theme.dir .. "/icons/submenu.png"
theme.taglist_squares_sel                       = theme.dir .. "/icons/square_unsel.png"
theme.taglist_squares_unsel                     = theme.dir .. "/icons/square_unsel.png"
theme.layout_tile                               = theme.dir .. "/icons/tile.png"
theme.layout_tileleft                           = theme.dir .. "/icons/tileleft.png"
theme.layout_tilebottom                         = theme.dir .. "/icons/tilebottom.png"
theme.layout_tiletop                            = theme.dir .. "/icons/tiletop.png"
theme.layout_fairv                              = theme.dir .. "/icons/fairv.png"
theme.layout_fairh                              = theme.dir .. "/icons/fairh.png"
theme.layout_spiral                             = theme.dir .. "/icons/spiral.png"
theme.layout_dwindle                            = theme.dir .. "/icons/dwindle.png"
theme.layout_max                                = theme.dir .. "/icons/max.png"
theme.layout_fullscreen                         = theme.dir .. "/icons/fullscreen.png"
theme.layout_magnifier                          = theme.dir .. "/icons/magnifier.png"
theme.layout_floating                           = theme.dir .. "/icons/floating.png"
theme.titlebar_close_button_normal              = theme.dir .. "/icons/close_normal.png"
theme.titlebar_close_button_focus               = theme.dir .. "/icons/close_focus.png"
theme.titlebar_minimize_button_normal           = theme.dir .. "/icons/minimize_normal.png"
theme.titlebar_minimize_button_focus            = theme.dir .. "/icons/minimize_focus.png"
theme.titlebar_ontop_button_normal_inactive     = theme.dir .. "/icons/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive      = theme.dir .. "/icons/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active       = theme.dir .. "/icons/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active        = theme.dir .. "/icons/ontop_focus_active.png"
theme.titlebar_sticky_button_normal_inactive    = theme.dir .. "/icons/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive     = theme.dir .. "/icons/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active      = theme.dir .. "/icons/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active       = theme.dir .. "/icons/sticky_focus_active.png"
theme.titlebar_floating_button_normal_inactive  = theme.dir .. "/icons/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive   = theme.dir .. "/icons/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active    = theme.dir .. "/icons/floating_normal_active.png"
theme.titlebar_floating_button_focus_active     = theme.dir .. "/icons/floating_focus_active.png"
theme.titlebar_maximized_button_normal_inactive = theme.dir .. "/icons/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = theme.dir .. "/icons/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active   = theme.dir .. "/icons/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active    = theme.dir .. "/icons/maximized_focus_active.png"

theme.ac                                        = theme.dir .. "/icons/ac.png"
theme.bat                                       = theme.dir .. "/icons/bat.png"
theme.bat_low                                   = theme.dir .. "/icons/bat_low.png"
theme.bat_no                                    = theme.dir .. "/icons/bat_no.png"
theme.battery                                   = theme.dir .. "/icons/battery.png"
theme.battery_empty                             = theme.dir .. "/icons/battery_empty.png"
theme.battery_low                               = theme.dir .. "/icons/battery_low.png"
theme.cpu                                       = theme.dir .. "/icons/cpu.png"
theme.disk                                      = theme.dir .. "/icons/disk.png"
theme.hdd                                       = theme.dir .. "/icons/hdd.png"
theme.mail                                      = theme.dir .. "/icons/mail.png"
theme.mail_on                                   = theme.dir .. "/icons/mail_on.png"
theme.mem                                       = theme.dir .. "/icons/mem.png"
theme.music                                     = theme.dir .. "/icons/note.png"
theme.music_on                                  = theme.dir .. "/icons/note_on.png"
theme.net                                       = theme.dir .. "/icons/net.png"
theme.pause                                     = theme.dir .. "/icons/pause.png"
theme.play                                      = theme.dir .. "/icons/play.png"
theme.stop                                      = theme.dir .. "/icons/stop.png"
theme.temp                                      = theme.dir .. "/icons/temp.png"
theme.vol                                       = theme.dir .. "/icons/vol.png"
theme.vol_low                                   = theme.dir .. "/icons/vol_low.png"
theme.vol_mute                                  = theme.dir .. "/icons/vol_mute.png"
theme.vol_no                                    = theme.dir .. "/icons/vol_no.png"

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = nil

-- Awesome >= 4.1
-- theme.tasklist_disable_task_name = true

-- lain related
theme.layout_centerfair                         = theme.dir .. "/icons/centerfair.png"
theme.layout_termfair                           = theme.dir .. "/icons/termfair.png"
theme.layout_centerwork                         = theme.dir .. "/icons/centerwork.png"

return theme
