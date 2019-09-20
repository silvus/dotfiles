local screens = {}

local awful = require("awful")
local io = require("io")

-- Set global screen variable to a local one
local screen = screen

-- Xrandr commands based on current hostname
local screens_configuration = {}
-- screens_configuration['mars'] = {}
-- Triple Screen
-- screens_configuration['mars']['DVI-D-0'] = '--primary --pos 0x0'
-- screens_configuration['mars']['HDMI-0'] = '--rotate left --pos 1920x-600'
-- screens_configuration['mars']['DP-0'] = '--above DVI-D-0'
-- Screen vertical
-- xrandr --output DVI-D-0 --auto --primary --pos 0x0 --output HDMI-0 --auto --rotate left --pos 1920x-600 --output HDMI-1 --off
-- Triple Screen
-- xrandr --output DVI-D-0 --auto --primary --pos 0x0 --output HDMI-0 --auto --rotate left --pos 1920x-600 --output DP-0 --auto --above DVI-D-0 --output HDMI-1 --off
-- Simple big screen
-- screens_configuration['mars']['DP-0'] = '--primary'

-- With projector
-- xrandr --output HDMI-1 --auto --same-as DVI-D-0
-- Without projector
-- xrandr --output HDMI-1 --off

-- Dual screen - VGA (vertical) on right
-- screens_configuration['orcus'] = {}
-- screens_configuration['orcus']['DVI-D-0'] = '--primary --pos 0x0'
-- screens_configuration['orcus']['HDMI-0'] = '--rotate left --pos 1920x-600'
-- screens_configuration['orcus']['HDMI-0'] = '--above DVI-D-0'

-- Dual screen - VGA (vertical) on right
screens_configuration['pc190701'] = {}
screens_configuration['pc190701']['HDMI-2'] = '--primary --pos 0x500'
screens_configuration['pc190701']['DP-1'] = '--rotate left --pos 1920x0 --right-of HDMI-2'


-- Keep screens orders (lua doesn't keep array declaration order)
local screens_index = {
	'DVI-D0',
	'DVI-D-0',
	'HDMI0',
	'HDMI-0',
	'HDMI1',
	'HDMI-1',
	'HDMI2',
	'HDMI-2',
	'VGA0',
	'VGA-0',
	'VGA1',
	'VGA-1',
	'DP0',
	'DP-0',
	'DP1',
	'DP-1',
}

-- Setup screens from config
local function update()
	hostname = io.popen("hostname --short"):read()
	-- debug_log(hostname)

	if screens_configuration[hostname] then
		command = ''
		for _, output in ipairs(screens_index) do
			display = screens_configuration[hostname][output]
			if display then
				command = command .. ' --output ' .. output .. ' --auto ' .. display
			end
		end

		-- debug_log(command)
		if command then
			os.execute('xrandr ' .. command)
			return true
		end
	end

	-- Fallback
	awful.spawn.with_shell("~/.dotfiles/bin/autostart_screen")
end

-- Count numbers of screens
local function count()
	if screen.count() then
		return screen:count()
	end

	-- Fallback to at least one screen
	return 1
end

-- Get primary screen
local function get_primary()
	return screen.primary
end


screens.update = update
screens.count = count
screens.get_primary = get_primary

return screens
