local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")

local customwidget = {}

customwidget.icon = wibox.widget.imagebox(beautiful.shield)
customwidget.icon.visible = false

local status_text = ""

-- VPN: any OpenVPN-style tun or WireGuard interface, whatever it's named.
awful.widget.watch(
	"ip -o link show type tun 2>/dev/null; ip -o link show type wireguard 2>/dev/null",
	10,
	function(widget, stdout)
		local names = {}
		for name in stdout:gmatch("%d+:%s*([^:@]+)") do
			table.insert(names, name)
		end

		if #names > 0 then
			widget.icon.visible = true
			status_text = "VPN: " .. table.concat(names, ", ")
		else
			widget.icon.visible = false
			status_text = ""
		end
	end,
	customwidget
)

awful.tooltip {
	objects        = { customwidget.icon },
	timer_function = function() return status_text end,
}

return customwidget
