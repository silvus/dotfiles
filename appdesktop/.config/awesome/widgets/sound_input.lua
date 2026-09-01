local string = require("string")
local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local pulseaudio = require("utils.pulseaudio")

local customwidget = {}

local volume_value = 0

customwidget.icon = wibox.widget.imagebox(beautiful.microphone)
customwidget.widget = wibox.container.margin(customwidget.icon, 3, 4, 3, 4)

local function update()
	local level, muted = pulseaudio.get_volume_and_mute("source")
	if not level then
		-- pactl gave us nothing (e.g. pulseaudio/pipewire restarting): keep
		-- showing the last known state instead of guessing.
		return
	end

	volume_value = level

	if muted or level == 0 then
		customwidget.icon:set_image(beautiful.microphone_off)
	else
		customwidget.icon:set_image(beautiful.microphone)
	end
end

customwidget.update = update
update()

-- Live updates via `pactl subscribe`, instead of polling ALSA every 2s.
pulseaudio.watch_events(function(line)
	return line:match("on source") or line:match("on server")
end, update)

-- Tooltip
awful.tooltip {
	objects        = { customwidget.widget },
	timer_function = function()
		return string.format("%d%%", volume_value)
	end,
}

-- events
local buttons_event = awful.util.table.join(
	awful.button({}, 1, function()
		awful.spawn("pavucontrol")
	end),
	awful.button({}, 2, function()
		awful.spawn("pactl set-source-volume @DEFAULT_SOURCE@ 100%")
		update()
	end),
	awful.button({}, 3, function()
		awful.spawn("pactl set-source-mute @DEFAULT_SOURCE@ toggle")
		update()
	end),
	awful.button({}, 4, function()
		awful.spawn("pactl set-source-volume @DEFAULT_SOURCE@ +10%")
		update()
	end),
	awful.button({}, 5, function()
		awful.spawn("pactl set-source-volume @DEFAULT_SOURCE@ -10%")
		update()
	end)
)

customwidget.widget:buttons(buttons_event)

return customwidget
