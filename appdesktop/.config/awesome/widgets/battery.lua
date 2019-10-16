local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")
local naughty = require("naughty")
local lain = require("lain")


local customwidget = {}


customwidget.icon = wibox.widget.imagebox(beautiful.battery)
customwidget.icon.visible = false

-- Battery

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
	widget			= wibox.widget.progressbar
}
batbar.visible  = false

local battery = lain.widget.bat({
	settings = function()
		if bat_now.status == "N/A" then
			-- No battery
			batbar.visible  = false
			customwidget.icon.visible = false
		else
			batbar.visible  = true
			customwidget.icon.visible = true
			batbar:set_value(bat_now.perc)

			-- Change icon and color if full or low battery
			if bat_now.perc >= 95 then
				batbar.color = beautiful.primary
			elseif bat_now.perc <= 15 then
				customwidget.icon:set_image(beautiful.battery_low)
				batbar.color = beautiful.error
			elseif bat_now.perc <= 5 then
				customwidget.icon:set_image(beautiful.battery_empty)
				batbar.color = beautiful.error
			else
				customwidget.icon:set_image(beautiful.battery)
				if bat_now.status == "Full" then
					batbar.color = beautiful.primary
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

customwidget.widget = wibox.container.margin(batbg, 2, 7, 4, 4)

return customwidget
