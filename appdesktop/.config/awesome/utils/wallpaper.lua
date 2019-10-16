local os = require("os")
local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")
local screens = require("screens")

local wallpaper = {}


-- Wallpaper
-- First search in ~/.wallpaper.png
-- Then search in ~/.wallpaper.jpg
-- Then if ~/.wallpapers exist, use a different wallpaper by screen and tag index, then by tag index only, then from default in ~/.config/awesome/wallpapers
-- It can be customize with ~/.wallpapers/wallpaper_{screen_index}_{tag_index}.{jpg|png}
-- Final fallback to beautiful.wallpaper
local function update(s)
	local wallpaper_current = nil

	-- Default to focused screen
	if not s then
		s = awful.screen.focused()
	end

	if awful.util.file_readable(os.getenv("HOME") .. '/.wallpaper.png') then
		-- if ~/.wallpaper is a file, use it
		wallpaper_current = os.getenv("HOME") .. '/.wallpaper.png'
	elseif awful.util.file_readable(os.getenv("HOME") .. '/.wallpaper.jpg') then
		-- if ~/.wallpaper is a file, use it
		wallpaper_current = os.getenv("HOME") .. '/.wallpaper.jpg'

	elseif awful.util.dir_readable (os.getenv("HOME") .. '/.wallpapers') then
		-- if ~/.wallpapers is a directory, pick one into it (if name match pattern wallpaper_tagindex.ext)
		local wallpapers_dir = os.getenv("HOME") .. '/.wallpapers'
		-- else fallback to default wallpapers directory
		local wallpaper_default_dir = os.getenv("HOME") .. '/.config/awesome/wallpapers'
		-- Based on current tag
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

		-- naughty.notify({title = "tag_name", text = tostring(tag_name) })

		local wallpapers_possibilities = {
			-- Search in home with a screen index
			wallpapers_dir .. '/wallpaper_' .. screen_index .. '_' .. tag_name .. '.jpg',
			wallpapers_dir .. '/wallpaper_' .. screen_index .. '_' .. tag_name .. '.png',
			-- Search in home
			wallpapers_dir .. '/wallpaper_' .. tag_name .. '.jpg',
			wallpapers_dir .. '/wallpaper_' .. tag_name .. '.png',
			-- Search in default directory jpg
			wallpaper_default_dir .. '/wallpaper_' .. tag_name .. '.jpg',
			wallpaper_default_dir .. '/wallpaper_' .. tag_name .. '.png'
		}
		-- take the first existing wallpaper
		for _, wp in ipairs(wallpapers_possibilities) do
			if awful.util.file_readable(wp) then
				wallpaper_current = wp
				break
			end
		end
	end

	-- Fallback to beautiful.wallpaper
	if wallpaper_current == nil and beautiful.wallpaper then
		wallpaper_current = beautiful.wallpaper
		-- If wallpaper is a function, call it with the screen
		if type(wallpaper) == "function" then
			wallpaper_current = wallpaper(s)
		end
	end

	-- Set wallpaper
	if wallpaper_current ~= nil then
		gears.wallpaper.maximized(wallpaper_current, s, true)
	end
end


wallpaper.update = update

return wallpaper
