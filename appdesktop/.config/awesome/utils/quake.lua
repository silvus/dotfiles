local os = require("os")
local screens = require("screens")
local config = require("config")
local lain = require("lain")


local quake = {}

-- Quake like terminal (single instance for all screens)
quake.term = lain.util.quake({
	-- client name
	name = "guaketerm",
	-- client to spawn
	app = config.terminal,
	-- extra app arguments
	extra = "-title terminal -e " .. config.home .. "/.dotfiles/bin/tmuxdev",
	-- border width
	border = 0,
	-- initially visible
	-- visible = false,
	-- Overlap the wibox or not
	overlap = true,
	-- always spawn on currently focused screen
	followtag = false,
	-- On primary screen
	screen = screens.get_primary(),
	-- dropdown client height (float in [0,1] or exact pixels number)
	height = 1,
	-- dropdown client width (float in [0,1] or exact pixels number)
	width = 1,
	-- vertical position (string, possible values: "top", "bottom", "center")
	vert = "top",
	-- horizontal position (string, possible values: "left", "right", "center")
	horiz = "center",
	-- settings is a function which takes the client as input, and can be used to customize its properties
	settings = function(c)
		c.fullscreen = true
		-- c.ontop = true -- Not compatible with fullscreen
		-- c.sticky = true
		c.floating = true
		-- c.maximized_vertical = true
		-- c.maximized_horizontal = true
	end
})

-- Quake like editor (single instance for all screens)
-- quake.editor = lain.util.quake({
-- 	-- client name
-- 	name = "zim",
-- 	-- client to spawn
-- 	app = "zim",
-- 	-- how to specify client name
-- 	argname = '',  -- cannot set client name ?
-- 	-- extra app arguments
-- 	extra = "",
-- 	-- border width
-- 	border = 0,
-- 	-- initially visible
-- 	-- visible = false,
-- 	-- Overlap the wibox or not
-- 	overlap = false,
-- 	-- always spawn on currently focused screen
-- 	followtag = false,
-- 	-- On primary screen
-- 	screen = screens.get_primary(),
-- 	-- dropdown client height (float in [0,1] or exact pixels number)
-- 	height = 1,
-- 	-- dropdown client width (float in [0,1] or exact pixels number)
-- 	width = 1,
-- 	-- vertical position (string, possible values: "top", "bottom", "center")
-- 	vert = "top",
-- 	-- horizontal position (string, possible values: "left", "right", "center")
-- 	horiz = "center",
-- 	-- settings is a function which takes the client as input, and can be used to customize its properties
-- 	settings = function(c)
-- 		c.ontop = true -- Not compatible with fullscreen
-- 		c.sticky = true
-- 		-- c.titlebars_enabled = true
-- 		-- c.floating = true
-- 		c.maximized_vertical = true
-- 		c.maximized_horizontal = true
--
-- 		c.titlebars_enabled = true
-- 		c.floating = true
-- 		-- c.fullscreen = true
-- 	end
-- })


return quake
