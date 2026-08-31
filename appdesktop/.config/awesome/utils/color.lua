local colorlib = {}

-- Color helpers for theme files.
--
-- Usage:
-- local color = require("utils.color")
-- color.darken("#009914", 0.4) --> "#00450a" (about 40% of the original brightness)

-- Scale a "#rrggbb" color's channels towards black by `factor` (0..1,
-- where 1 keeps the color unchanged and 0 turns it fully black).
local function darken(hex, factor)
	local r, g, b = hex:match("^#(%x%x)(%x%x)(%x%x)$")
	if not r then
		return hex
	end

	r, g, b = tonumber(r, 16), tonumber(g, 16), tonumber(b, 16)
	r = math.floor(r * factor + 0.5)
	g = math.floor(g * factor + 0.5)
	b = math.floor(b * factor + 0.5)

	return string.format("#%02x%02x%02x", r, g, b)
end

colorlib.darken = darken

return colorlib
