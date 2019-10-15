local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")
local naughty = require("naughty")
local lain = require("lain")

local widgets = {}


-- Battery
-- TODO: move in own widget file
-- ----------------------------------------------------------------------------
widgets.baticon = wibox.widget.imagebox(beautiful.battery)
local batbar = wibox.widget {
	forced_height	= 1,
	forced_width	= 100,
	margins			= 1,
	paddings		= 1,
	ticks			= true,
	ticks_size		= 10,
	step_width		= 10,
	max_value		= 100,
	min_value		= 0,
	value			= 0,
	color 			= beautiful.success,
	background_color = beautiful.bg_normal,
	border_color	= beautiful.info,
	widget		    = wibox.widget.progressbar
}
batbar.visible  = false
widgets.baticon.visible = false
local battery = lain.widget.bat({
	settings = function()
		if bat_now.status == "N/A" then
			-- No battery
			batbar.visible  = false
			widgets.baticon.visible = false
		else
			batbar.visible  = true
			widgets.baticon.visible = true
			batbar:set_value(bat_now.perc)

			-- Change icon and color if full or low battery
			if bat_now.perc >= 95 then
				batbar.color = beautiful.success_alt
			elseif bat_now.perc <= 15 then
				widgets.baticon:set_image(beautiful.battery_low)
				batbar.color = beautiful.error
			elseif bat_now.perc <= 5 then
				widgets.baticon:set_image(beautiful.battery_empty)
				batbar.color = beautiful.error
			else
				widgets.baticon:set_image(beautiful.battery)
				if bat_now.status == "Full" then
					batbar.color = beautiful.success_alt
				elseif bat_now.status == "Discharging" then
					batbar.color = beautiful.success
				elseif bat_now.status == "Charging" then
					batbar.color = beautiful.info

				end
			end
		end
	end,

	-- Batterie notifications
	bat_notification_charged_preset = {
		-- title   = "Battery full",
		-- text    = "You can unplug the cable",
		timeout = naughty.config.presets.low.timeout,
		fg      = naughty.config.presets.low.fg,
		bg      = naughty.config.presets.low.bg,
		preset  = naughty.config.presets.low
	},
	bat_notification_low_preset = {
		-- title = "Battery low",
		-- text = "Plug the cable!",
		timeout = naughty.config.presets.critical.timeout,
		fg = naughty.config.presets.critical.fg,
		bg = naughty.config.presets.critical.bg,
		preset = naughty.config.presets.critical
	},
	bat_notification_critical_preset = {
		-- title = "Battery exhausted",
		-- text = "Shutdown imminent",
		timeout = naughty.config.presets.critical.timeout,
		fg = naughty.config.presets.critical.fg,
		bg = naughty.config.presets.critical.bg,
		preset = naughty.config.presets.critical
	},
})
local batbg = wibox.container.background(batbar, beautiful.info, gears.shape.rectangle)
widgets.batwidget = wibox.container.margin(batbg, 2, 7, 4, 4)

-- Disks bar
-- ----------------------------------------------------------------------------
-- local fsicon = wibox.widget.imagebox(beautiful.hdd)
-- fsbar = wibox.widget {
--	 forced_height	= 1,
--	 forced_width	 = 100,
--	 margins		  = 1,
--	 paddings		 = 1,
--	 ticks			= true,
--	 ticks_size	   = 6,
--	 max_value 		 = 100,
--	 value			= 0,
--	 color 			 = beautiful.success,
--	 background_color = beautiful.info,
--	 border_color	 = beautiful.info,
--	 widget		   = wibox.widget.progressbar
-- }
-- fs = lain.widget.fs({
--	 partition = "/",
--	 -- options = "--exclude-type=tmpfs",
--	 settings  = function()
--		 if tonumber(fs_now.used) < 90 then
--			 fsbar:set_color(beautiful.success)
--		 else
--			 fsbar:set_color(beautiful.error)
--		 end
--		 fsbar:set_value(fs_now.used)
--	 end
-- })
-- local fsbg = wibox.container.background(fsbar, beautiful.info, gears.shape.rectangle)
-- local myfswidget = wibox.container.margin(fsbg, 2, 7, 4, 4)


-- Mail
-- in mail.lua
-- ----------------------------------------------------------------------------
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



return widgets