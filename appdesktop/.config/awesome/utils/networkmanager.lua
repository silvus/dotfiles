local awful = require("awful")
local gears = require("gears")

-- Small shared helpers around `nmcli`.
-- Everything here that shells out is async (awful.spawn), never io.popen

local nm = {}

-- Run `nmcli <args>`, invoke callback(stdout) once it exits.
function nm.cli_async(args, callback)
	awful.spawn.easy_async_with_shell("nmcli " .. args .. " 2>/dev/null", function(stdout)
		callback(stdout or "")
	end)
end

function nm.trim(s)
	return (s or ""):gsub("^%s+", ""):gsub("%s+$", "")
end

-- Single-quote a string for safe use in a shell command.
function nm.shell_quote(s)
	return "'" .. (s or ""):gsub("'", "'\\''") .. "'"
end

-- Parse `nmcli -t -f ...` colon-separated output into rows of fields.
-- nmcli backslash-escapes ":" (and "\") inside a field, so a naive gmatch("[^:]+") split would break on SSIDs containing a colon.
function nm.parse_terse(text)
	local rows = {}
	for line in text:gmatch("[^\n]+") do
		local fields = {}
		local field = {}
		local i = 1
		while i <= #line do
			local c = line:sub(i, i)
			if c == "\\" and i < #line then
				table.insert(field, line:sub(i + 1, i + 1))
				i = i + 2
			elseif c == ":" then
				table.insert(fields, table.concat(field))
				field = {}
				i = i + 1
			else
				table.insert(field, c)
				i = i + 1
			end
		end
		table.insert(fields, table.concat(field))
		table.insert(rows, fields)
	end
	return rows
end

function nm.wifi_enabled_async(callback)
	nm.cli_async("radio wifi", function(out)
		callback(nm.trim(out) == "enabled")
	end)
end

function nm.set_wifi(enabled)
	awful.spawn("nmcli radio wifi " .. (enabled and "on" or "off"), false)
end

function nm.rescan()
	awful.spawn("nmcli device wifi rescan", false)
end

-- The device NetworkManager considers "primary" (i.e. the one carrying the default route), if any: { type = "wifi"|"ethernet"|..., connection = name }.
function nm.primary_connection_async(callback)
	nm.cli_async("-t -f DEVICE,TYPE,STATE,CONNECTION device status", function(out)
		local rows = nm.parse_terse(out)
		local fallback = nil
		for _, r in ipairs(rows) do
			local dtype, state, conn = r[2], r[3], r[4]
			if state == "connected" then
				if dtype == "ethernet" then
					callback({ type = dtype, connection = conn })
					return
				elseif dtype == "wifi" then
					fallback = fallback or { type = dtype, connection = conn }
				end
			end
		end
		callback(fallback)
	end)
end

-- Access points in range: { {ssid=, signal=, security=, in_use=}, ... }, de-duplicated by SSID (keep the strongest signal), sorted strongest first.
function nm.wifi_list_async(callback)
	nm.cli_async("-t -f IN-USE,SSID,SIGNAL,SECURITY device wifi list", function(out)
		local rows = nm.parse_terse(out)
		local by_ssid = {}
		local order = {}
		for _, r in ipairs(rows) do
			local in_use, ssid, signal, security = r[1], r[2], tonumber(r[3]) or 0, r[4]
			if ssid and ssid ~= "" then
				local existing = by_ssid[ssid]
				if not existing or signal > existing.signal then
					if not existing then table.insert(order, ssid) end
					by_ssid[ssid] = { ssid = ssid, signal = signal, security = security, in_use = (in_use == "*") }
				end
			end
		end
		local list = {}
		for _, ssid in ipairs(order) do table.insert(list, by_ssid[ssid]) end
		table.sort(list, function(a, b) return a.signal > b.signal end)
		callback(list)
	end)
end

-- Signal strength (0-100) of the access point currently in use, or nil.
function nm.current_signal_async(callback)
	nm.wifi_list_async(function(list)
		for _, ap in ipairs(list) do
			if ap.in_use then
				callback(ap.signal)
				return
			end
		end
		callback(nil)
	end)
end

-- Connect to `ssid`. Tries without a password first (covers open networks and networks already known to NetworkManager).
-- If that fails, asks for a password via a `zenity` dialog and retries. `callback(ok)` runs at the end.
function nm.connect(ssid, callback)
	local quoted = nm.shell_quote(ssid)
	awful.spawn.easy_async_with_shell("nmcli device wifi connect " .. quoted, function(_, _, _, code)
		if code == 0 then
			if callback then callback(true) end
			return
		end
		awful.spawn.easy_async_with_shell(
			"pw=$(zenity --password --title=" .. quoted .. ") && nmcli device wifi connect " .. quoted .. " password \"$pw\"",
			function(_, _, _, code2)
				if callback then callback(code2 == 0) end
			end
		)
	end)
end

-- Debounced live-update watcher: subscribes to `nmcli monitor` and calls `update()` (after a short debounce, since several events fire per change)
function nm.watch(update)
	local debounce_pending = false
	local function debounced_update()
		if debounce_pending then return end
		debounce_pending = true
		gears.timer.start_new(0.5, function()
			debounce_pending = false
			update()
			return false
		end)
	end

	local function watch()
		awful.spawn.with_line_callback("nmcli monitor", {
			stdout = function() debounced_update() end,
			exit = function()
				gears.timer.start_new(2, function()
					watch()
					return false
				end)
			end,
		})
	end

	watch()
end

return nm
