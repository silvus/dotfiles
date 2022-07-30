local os = require("os")
local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")
local screens = require("screens")
local config = require("config")
-- local naughty = require('naughty')

-- Wallpapers definitions
local wallpaper = {}


-- scan directory, and optionally filter outputs
function scandir(directory, filter)
	local i, t, popen = 0, {}, io.popen
	if not filter then
		filter = function(s) return true end
	end
	for filename in popen('ls -a "'..directory..'"'):lines() do
		if filter(filename) then
			i = i + 1
			t[i] = filename
		end
	end
	return t
end

-- Get wallpaper path
-- First search in ~/.wallpaper.png
-- Then search in ~/.wallpaper.jpg
-- Then if ~/.wallpapers exist, use a different wallpaper by screen and tag index, then by tag index only, then from default in ~/.config/awesome/wallpapers
-- It can be customize with ~/.wallpapers/wallpaper_{screen_index}_{tag_index}.{jpg|png} , if option is on, else a random wallpaper_* is picked
-- Final fallback to beautiful.wallpaper
local function _get_wallpaper_path(s, tag_based)
	local wallpaper_next = nil

	if awful.util.file_readable(config.home .. '/.wallpaper.png') then
		-- if ~/.wallpaper is a file, use it
		wallpaper_next = config.home .. '/.wallpaper.png'
	elseif awful.util.file_readable(config.home .. '/.wallpaper.jpg') then
		-- if ~/.wallpaper is a file, use it
		wallpaper_next = config.home .. '/.wallpaper.jpg'

	elseif awful.util.dir_readable (config.home .. '/.wallpapers') then
		-- if ~/.wallpapers is a directory, pick one into it (if name match pattern wallpaper_tagindex.ext)
		local wallpapers_dir = config.home .. '/.wallpapers'
		-- else fallback to default wallpapers directory
		local wallpaper_default_dir = config.home .. '/.config/awesome/wallpapers'

		if tag_based then
			-- Based on current tag (if arg)
			local tag = s.selected_tag
			local tag_name = "1"
			if tag then
				tag_name = tag.name
			end

			-- get screen index on multi screen
			local screen_index = 1
			if screens.count() > 1 and s.index then
				screen_index = s.index
			end

			local wallpapers_possibilities = {
				-- Search in home with a screen index
				wallpapers_dir .. '/wallpaper_' .. screen_index .. '_' .. tag_name .. '.jpg',
				wallpapers_dir .. '/wallpaper_' .. screen_index .. '_' .. tag_name .. '.png',
				-- Search in home
				wallpapers_dir .. '/wallpaper_' .. tag_name .. '.jpg',
				wallpapers_dir .. '/wallpaper_' .. tag_name .. '.png',
				-- Search in default directory
				wallpaper_default_dir .. '/wallpaper_' .. tag_name .. '.jpg',
				wallpaper_default_dir .. '/wallpaper_' .. tag_name .. '.png'
			}
			-- take the first existing wallpaper
			for _, wp in ipairs(wallpapers_possibilities) do
				if awful.util.file_readable(wp) then
					wallpaper_next = wp
					break
				end
			end
		else 
			-- Take a random wallpaper from ~/.wallpapers
			local wallpaper_filter = function(s) 
				-- image only and exclude hiddens files
				return (string.match(s,"%.png$") or string.match(s,"%.jpg$") or string.match(s,"%.jpeg$")) and string.sub(s, 1, 1) ~= '.'
			end
			wallpapers_files = scandir(wallpapers_dir, wallpaper_filter)
			wallpapers_random_index = math.random(1, #wallpapers_files)
			wallpaper_next = wallpapers_dir .. '/' .. wallpapers_files[wallpapers_random_index]
		end
	end

	-- Fallback to beautiful.wallpaper
	if wallpaper_next == nil and beautiful.wallpaper then
		wallpaper_next = beautiful.wallpaper
		-- If wallpaper is a function, call it with the screen
		if type(wallpaper) == "function" then
			wallpaper_next = wallpaper(s)
		end
	end

	return wallpaper_next
end

-- Set wallpaper
local function _set_wallpaper(s, wallpaper_path)
	if wallpaper_path ~= nil then
		-- 	maximized (surf, s, ignore_aspect, offset)
		--	 surf The wallpaper to set. Either a cairo surface or a file name.
		--	 s The screen whose wallpaper should be set. Can be nil, in which case all screens are set.
		--	 ignore_aspect If this is true, the image’s aspect ratio is ignored. The default is to honor the aspect ratio.
		--	 offset This can be set to a table with entries x and y.
		gears.wallpaper.maximized(wallpaper_path, s, false)
	end
end

-- Set wallpaper on screens init or update wallpaper on each tag change (config option)
-- To be used in property::selected event and screens init
local function update(s)
	-- Default to focused screen (no screen passed on tag selected event)
	if not s then
		s = awful.screen.focused()
	end
	local wallpapers_by_tag = config.wallpapers_by_tag
	local wallpaper_next = _get_wallpaper_path(s, wallpapers_by_tag)
	_set_wallpaper(s, wallpaper_next)
end


wallpaper.update = update

return wallpaper
