local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")

local customwidget = {}

customwidget.icon = wibox.widget.imagebox(beautiful.net_wired)
customwidget.icon.visible = false

-- VPN

local vpn = awful.widget.watch(
	"ip addr show tun0",
	10,
	function(customwidget, stdout, stderr, exitreason, exitcode)
		if exitcode == 0 then
			customwidget:set_markup("<span color='" .. beautiful.success .. "'>VPN: ON</span>")
			customwidget.icon.visible = true
		else
			customwidget:set_markup("")
			customwidget.icon.visible = false
		end
	end
)
customwidget.widget = wibox.container.margin(vpn, 2, 7, 4, 4)

return customwidget
