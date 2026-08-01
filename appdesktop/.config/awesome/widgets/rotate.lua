local wibox = require("wibox")

-- Wrap a widget to be laid out vertically (for left/right wibars)
local function widget_rotate(w, inverse)
	local direction = 'east'
	if inverse then
		direction = 'west'
	end

	return wibox.container {
		w,
		direction = direction,
		widget = wibox.container.rotate
	}
end

return widget_rotate
