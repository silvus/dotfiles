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


devlib.debug_log = debug_log

return devlib