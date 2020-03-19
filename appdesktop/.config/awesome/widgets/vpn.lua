local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")

local customwidget = {}

customwidget.vpn_text = wibox.widget.textbox()

customwidget.icon = wibox.widget.imagebox(beautiful.shield)
customwidget.icon.visible = false

-- VPN

awful.widget.watch(
	"ip addr show tun0",
	10,
	function(customwidget, stdout, stderr, exitreason, exitcode)
		if exitcode == 0 then
			customwidget.vpn_text:set_markup("<span font='DejaVu Sans 8' color='" .. beautiful.success .. "'>VPN</span>")
			customwidget.icon.visible = true
		else
			customwidget.vpn_text:set_markup("")
			customwidget.icon.visible = false
		end
		return
	end,
	customwidget
)

customwidget.widget = wibox.container.margin(customwidget.vpn_text, 1, 1, 1, 1)

return customwidget
