local awful = require("awful")
local gears = require("gears")

-- Small shared helpers around `pactl`

local pulseaudio = {}

function pulseaudio.pactl(args)
	local h = io.popen("pactl " .. args .. " 2>/dev/null")
	if not h then return "" end
	local out = h:read("*a") or ""
	h:close()
	return out
end

function pulseaudio.trim(s)
	return (s or ""):gsub("^%s+", ""):gsub("%s+$", "")
end

-- kind = "sink" or "source". Returns level (0-100, or nil if pactl gave nothing) and muted (boolean).
function pulseaudio.get_volume_and_mute(kind)
	local vol_out = pulseaudio.pactl("get-" .. kind .. "-volume @DEFAULT_" .. kind:upper() .. "@")
	local level = tonumber(vol_out:match("(%d+)%%"))
	local mute_out = pulseaudio.trim(pulseaudio.pactl("get-" .. kind .. "-mute @DEFAULT_" .. kind:upper() .. "@"))
	return level, mute_out:match("yes") ~= nil
end

-- Debounced live-update watcher: subscribes to `pactl subscribe` and calls
-- `update()` (after a short debounce, since pactl emits several events per
-- change) whenever `is_relevant(line)` returns true for an event line.
-- Auto-retries the subscription if pulseaudio/pipewire restarts.
function pulseaudio.watch_events(is_relevant, update)
	local debounce_pending = false
	local function debounced_update()
		if debounce_pending then return end
		debounce_pending = true
		gears.timer.start_new(0.1, function()
			debounce_pending = false
			update()
			return false
		end)
	end

	local function watch()
		awful.spawn.with_line_callback("pactl subscribe", {
			stdout = function(line)
				if is_relevant(line) then
					debounced_update()
				end
			end,
			exit = function()
				-- pulseaudio/pipewire restarted or `pactl` isn't there yet: retry.
				gears.timer.start_new(2, function()
					watch()
					return false
				end)
			end,
		})
	end

	watch()
end

return pulseaudio
