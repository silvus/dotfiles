local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")
local networkmanager = require("utils.networkmanager")

-- Network widget, replacing nm-applet

local customwidget = {}

local status_text = ""
local open_menu = nil

customwidget.icon = wibox.widget.imagebox(beautiful.network_off)

local function label_for(dev)
	if dev.type == "ethernet" then return "Wired" end
	return dev.connection or "Wi-Fi"
end

local function update()
	networkmanager.primary_connection_async(function(dev)
		if not dev then
			networkmanager.wifi_enabled_async(function(enabled)
				if enabled then
					customwidget.icon:set_image(beautiful.network_disconnected)
					status_text = "No network"
				else
					customwidget.icon:set_image(beautiful.network_off)
					status_text = "Wi-Fi off"
				end
			end)
			return
		end

		if dev.type == "ethernet" then
			customwidget.icon:set_image(beautiful.network_wired)
			status_text = label_for(dev)
			return
		end

		networkmanager.current_signal_async(function(signal)
			if signal and signal >= 67 then
				customwidget.icon:set_image(beautiful.network_wifi_high)
			elseif signal and signal >= 34 then
				customwidget.icon:set_image(beautiful.network_wifi_medium)
			else
				customwidget.icon:set_image(beautiful.network_wifi_low)
			end
			status_text = label_for(dev) .. (signal and (" " .. signal .. "%") or "")
		end)
	end)
end
customwidget.update = update
update()
networkmanager.watch(update)

-- `nmcli monitor` (above) reacts instantly to connect/disconnect, but not to plain signal-strength drift on an already-connected AP.
gears.timer {
	timeout = 2,
	autostart = true,
	call_now = false,
	callback = update,
}

-- pasystray/sound-widget-like menu: toggle Wi-Fi, pick a nearby network
local function signal_bars(signal)
	if signal >= 80 then
		return "\xe2\x96\x82\xe2\x96\x84\xe2\x96\x86\xe2\x96\x88"                   -- ▂▄▆█
	elseif signal >= 55 then
		return "\xe2\x96\x82\xe2\x96\x84\xe2\x96\x86_"                              -- ▂▄▆_
	elseif signal >= 30 then
		return "\xe2\x96\x82\xe2\x96\x84__"                                         -- ▂▄__
	else
		return "\xe2\x96\x82___"
	end                                                                           -- ▂___
end

local function ap_label(ap)
	local mark = ap.in_use and "> " or "  "
	return mark .. signal_bars(ap.signal) .. " " .. ap.ssid
end

local function show_menu(items)
	local menu = awful.menu { items = items }
	-- awful.menu also hides itself on an outside click / Escape
	local original_hide = menu.hide
	menu.hide = function(self, ...)
		open_menu = nil
		return original_hide(self, ...)
	end
	open_menu = menu
	menu:show()
end

local function toggle_menu()
	if open_menu then
		open_menu:hide()
		return
	end
	networkmanager.rescan()

	networkmanager.wifi_enabled_async(function(wifi_on)
		local items = {}

		table.insert(items, {
			wifi_on and "Disable Wi-Fi" or "Enable Wi-Fi",
			function()
				networkmanager.set_wifi(not wifi_on)
				-- Radio state changes asynchronously; give it a moment before refreshing.
				gears.timer.start_new(2, function()
					update()
					return false
				end)
			end,
		})

		local function finish_and_show(aps)
			if aps then
				for _, ap in ipairs(aps) do
					table.insert(items, {
						ap_label(ap),
						function() networkmanager.connect(ap.ssid, update) end,
					})
				end
				table.insert(items, { "Rescan", function() networkmanager.rescan() end })
			end
			table.insert(items, { "Open Connection Editor", function() awful.spawn("nm-connection-editor") end })
			show_menu(items)
		end

		if wifi_on then
			networkmanager.wifi_list_async(finish_and_show)
		else
			finish_and_show(nil)
		end
	end)
end

customwidget.icon:buttons(awful.util.table.join(awful.button({}, 1, toggle_menu)))

awful.tooltip {
	objects        = { customwidget.icon },
	timer_function = function() return status_text end,
}

return customwidget
