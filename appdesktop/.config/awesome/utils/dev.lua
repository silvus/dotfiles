local naughty = require("naughty")

local devlib = {}

-- Debug lib

-- Usage :
-- local dev = require("utils.dev")
-- dev.debug_log('here')

-- String notification
local function debug_log(text)
	naughty.notify({
		title = 'Debug',
		text = text,
		ontop = true,
		preset = naughty.config.presets.critical
	})
	-- local log = io.open('/tmp/awesomewm_debug.log', 'aw')
	-- log:write(text)
	-- log:flush()
	-- log:close()
end

local function debug_table(t)
	local str = ""
	for k, v in pairs(t) do
		str = str .. tostring(k) .. " " .. tostring(v) .. "\n"
	end
	naughty.notify({
		title = 'Debug',
		text = str,
		ontop = true,
		preset = naughty.config.presets.critical
	})
end


devlib.debug_table = debug_table
devlib.debug_log = debug_log

return devlib
