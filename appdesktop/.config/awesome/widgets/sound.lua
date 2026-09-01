local string = require("string")
local table = require("table")
local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")
local pulseaudio = require("utils.pulseaudio")

-- Sound widget, replacing pasystray.

local customwidget = {}

local out_value = 0

local pactl = pulseaudio.pactl
local trim = pulseaudio.trim

-- `pactl list sinks`/`list sources` long output -> { {index, name, description}, ... }
local function parse_devices(text)
	local devices = {}
	for block in (text .. "\n\n"):gmatch("(.-)\n\n") do
		local index = block:match("#(%d+)")
		local name = block:match("\n\tName: (.-)\n")
		local description = block:match("\n\tDescription: (.-)\n")
		local card = block:match("\n\tCard: (%d+)")
		if index and name then
			table.insert(devices, { index = index, name = name, description = description or name, card = card })
		end
	end
	return devices
end

-- `pactl list sink-inputs`/`list source-outputs` long output ->
-- { {index, device_index, app}, ... }. device_field is "Sink" or "Source".
local function parse_streams(text, device_field)
	local streams = {}
	for block in (text .. "\n\n"):gmatch("(.-)\n\n") do
		local index = block:match("#(%d+)")
		local device_index = block:match("\n\t" .. device_field .. ": (%d+)")
		local app = block:match('application%.name = "(.-)"') or block:match('media%.name = "(.-)"')
		if index then
			table.insert(streams, { index = index, device_index = device_index, app = app or ("stream #" .. index) })
		end
	end
	return streams
end

local get_volume_and_mute = pulseaudio.get_volume_and_mute

-- Same look as the ram widget's vertical bar
local function make_bar()
	local bar = wibox.widget {
		forced_height    = beautiful.graph_height or 1,
		forced_width     = beautiful.graph_width or 75,
		margins          = 1,
		paddings         = 1,
		ticks            = true,
		ticks_size       = 10,
		step_width       = 10,
		max_value        = 100,
		min_value        = 0,
		value            = 0,
		color            = beautiful.success,
		background_color = beautiful.bg_normal,
		border_color     = beautiful.info,
		widget           = wibox.widget.progressbar,
	}
	local bg = wibox.container.background(bar, beautiful.info, gears.shape.rectangle)
	local widget = wibox.container.margin(bg, 2, 4, 4, 4)
	return bar, widget
end

customwidget.icon_out = wibox.widget.imagebox(beautiful.vol)
customwidget.bar_out, customwidget.widget_out = make_bar()

local function update_out()
	local level, muted = get_volume_and_mute("sink")
	if not level then
		-- pactl gave us nothing (e.g. pulseaudio/pipewire restarting): keep
		-- showing the last known state instead of guessing "normal, unmuted".
		return
	end

	out_value = level
	customwidget.bar_out:set_value(level)

	if muted then
		customwidget.icon_out:set_image(beautiful.vol_mute)
		customwidget.bar_out:set_color(beautiful.error)
		customwidget.bar_out:set_border_color(beautiful.error)
	elseif level == 0 then
		customwidget.icon_out:set_image(beautiful.vol_no)
		customwidget.bar_out:set_color(beautiful.error)
		customwidget.bar_out:set_border_color(beautiful.error)
	elseif level <= 50 then
		customwidget.icon_out:set_image(beautiful.vol_low)
		customwidget.bar_out:set_color(beautiful.success)
		customwidget.bar_out:set_border_color(beautiful.info)
	else
		customwidget.icon_out:set_image(beautiful.vol)
		customwidget.bar_out:set_color(beautiful.success)
		customwidget.bar_out:set_border_color(beautiful.info)
	end
end

-- Public: refresh the bar. Also called by keys.lua after volume keys.
function customwidget.update()
	update_out()
end

customwidget.update()

-- Live updates via `pactl subscribe`, instead of polling.
pulseaudio.watch_events(function(line)
	return line:match("on sink") or line:match("on source") or line:match("on server")
end, customwidget.update)

-- pasystray-like menu: one flat list of devices, pick one to switch to it
local function device_label(dev, current_name)
	local mark = (dev.name == current_name) and "> " or "  "
	return mark .. (dev.description or dev.name)
end

local function guess_source_name(sink_name)
	return (sink_name:gsub("output", "input"))
end

local open_menu = nil

local function build_menu()
	local sinks = parse_devices(pactl("list sinks"))
	local sources_all = parse_devices(pactl("list sources"))
	-- Hide monitor sources (a sink looped back as a source)
	local sources = {}
	for _, s in ipairs(sources_all) do
		if not s.name:match("%.monitor$") then table.insert(sources, s) end
	end
	if #sources == 0 then sources = sources_all end
	local sources_by_name = {}
	local sources_by_card = {}
	for _, d in ipairs(sources) do
		sources_by_name[d.name] = d
		-- First non-monitor source on a card wins
		if d.card and not sources_by_card[d.card] then sources_by_card[d.card] = d end
	end

	local default_sink = trim(pactl("get-default-sink"))

	local sink_inputs = parse_streams(pactl("list sink-inputs"), "Sink")
	local source_outputs = parse_streams(pactl("list source-outputs"), "Source")

	local items = {
		{ "Open Mixer (pavucontrol)", function() awful.spawn("pavucontrol") end },
	}

	-- One entry per output device. Picking it switches both the default output and its matching input
	for _, dev in ipairs(sinks) do
		table.insert(items, {
			device_label(dev, default_sink),
			function()
				awful.spawn("pactl set-default-sink " .. dev.name, false)
				for _, si in ipairs(sink_inputs) do
					awful.spawn("pactl move-sink-input " .. si.index .. " " .. dev.name, false)
				end

				-- Match by card first: works for Bluetooth and USB devices where name substitution doesn't line up.
				-- Fall back to the name guess for devices without a Card.
				local source = (dev.card and sources_by_card[dev.card]) or sources_by_name[guess_source_name(dev.name)]
				if source then
					awful.spawn("pactl set-default-source " .. source.name, false)
					for _, so in ipairs(source_outputs) do
						awful.spawn("pactl move-source-output " .. so.index .. " " .. source.name, false)
					end
				end

				customwidget.update()
			end,
		})
	end

	local menu = awful.menu { items = items }
	-- awful.menu also hides itself on an outside click / Escape
	local original_hide = menu.hide
	menu.hide = function(self, ...)
		open_menu = nil
		return original_hide(self, ...)
	end
	return menu
end

local function toggle_menu()
	if open_menu then
		open_menu:hide()
		return
	end
	open_menu = build_menu()
	open_menu:show()
end

-- Buttons & tooltips
local buttons_out = awful.util.table.join(
	awful.button({}, 1, toggle_menu),
	awful.button({}, 3, function()
		awful.spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle")
		customwidget.update()
	end),
	awful.button({}, 4, function()
		awful.spawn("pactl set-sink-volume @DEFAULT_SINK@ +10%")
		customwidget.update()
	end),
	awful.button({}, 5, function()
		awful.spawn("pactl set-sink-volume @DEFAULT_SINK@ -10%")
		customwidget.update()
	end)
)

customwidget.icon_out:buttons(buttons_out)
customwidget.widget_out:buttons(buttons_out)

awful.tooltip {
	objects        = { customwidget.icon_out, customwidget.widget_out },
	timer_function = function() return string.format("%d%%", out_value) end,
}

return customwidget
