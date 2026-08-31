local wibox          = require("wibox")
local awful          = require("awful")
local gears          = require("gears")
local beautiful      = require("beautiful")

local customwidget   = {}

-- Apps list

customwidget.buttons = awful.util.table.join(
	awful.button({}, 1, function(c)
		if c == client.focus then
			c.minimized = true
		else
			-- Without this, the following :isvisible() makes no sense
			c.minimized = false
			if not c:isvisible() and c.first_tag then
				c.first_tag:view_only()
			end
			-- This will also un-minimize the client, if needed
			client.focus = c
			c:raise()
		end
	end)
)

-- Show clients from every screen instead of only the screen this widget
local function filter_all_screens(c)
	if c.sticky then
		return true
	end

	if not c.screen then
		return false
	end

	for _, t in ipairs(c:tags()) do
		if t.selected then
			return true
		end
	end

	return false
end

-- Single letter to fall back on when a client has no icon, so it still shows up as a visible tile instead of an empty gap.
local function fallback_letter(c)
	local name = c.class or c.name or "?"
	return name:sub(1, 1):upper()
end

-- Plain-word description of a client's state, used for the tooltip
local function status_text(c)
	local bits = {}
	if c.sticky then
		table.insert(bits, "pinned")
	end
	if c.minimized then
		table.insert(bits, "minimized")
	end
	if c.urgent then
		table.insert(bits, "urgent")
	end
	if c.ontop then
		table.insert(bits, "always on top")
	end
	if c.floating then
		table.insert(bits, "floating")
	end
	if c.maximized then
		table.insert(bits, "maximized")
	end

	if #bits == 0 then
		return ""
	end

	return " (" .. table.concat(bits, ", ") .. ")"
end

-- Fixed colors for the corner status dots. Kept independent of the theme palette
local BADGE_COLOR = {
	pin   = "#4CAF50", -- pinned / sticky
	min   = "#9E9E9E", -- minimized
	ontop = "#42A5F5", -- always on top
	float = "#FFB74D", -- floating
}

-- Show/hide/color one corner badge by id prefix.
local function set_badge(self, prefix, visible, letter)
	local bg_w = self:get_children_by_id(prefix .. "_bg_role")[1]
	bg_w:set_visible(visible)
	if visible then
		bg_w:set_bg(BADGE_COLOR[prefix])
		self:get_children_by_id(prefix .. "_role")[1]:set_markup("<b>" .. letter .. "</b>")
	end
end

-- Apply focus / urgent / minimized / icon styling to one task tile. Called both when the tile is created and whenever the client's state changes.
local function update_task(self, c)
	local icon_w        = self:get_children_by_id("icon_role")[1]
	local fallback_bg_w = self:get_children_by_id("fallback_bg_role")[1]
	local fallback_w    = self:get_children_by_id("fallback_role")[1]

	-- Icon, with a visible letter-tile fallback so apps without one don't just vanish from the list
	if c.icon then
		icon_w:set_image(c.icon)
		icon_w:set_visible(true)
		fallback_bg_w:set_visible(false)
	else
		icon_w:set_visible(false)
		fallback_bg_w:set_visible(true)
		local accent = beautiful.primary or beautiful.success
		fallback_bg_w:set_shape_border_color(accent)
		fallback_w:set_markup("<span color='" ..
			accent .. "'><b>" .. gears.string.xml_escape(fallback_letter(c)) .. "</b></span>")
	end


	-- Status: small dots in the corners of the icon
	set_badge(self, "pin", c.sticky, "P")
	set_badge(self, "min", c.minimized, "M")
	set_badge(self, "ontop", c.ontop, "T")
	set_badge(self, "float", c.floating, "F")

	-- Tooltip with the full picture
	self._task_tooltip = self._task_tooltip or awful.tooltip({ objects = { self } })
	self._task_tooltip.text = (c.name or c.class or "?") .. status_text(c)
end

-- The icon: either the client's own icon, or an outlined letter tile stacked in the same slot so it stays visible when there is no icon.
local icon_and_fallback = {
	{
		id                    = "icon_role",
		widget                = wibox.widget.imagebox,
		resize                = true,
		horizontal_fit_policy = "fit",
		vertical_fit_policy   = "fit",
		halign                = "center",
		valign                = "center",
	},
	{
		{
			id     = "fallback_role",
			widget = wibox.widget.textbox,
			align  = "center",
			valign = "center",
		},
		id                 = "fallback_bg_role",
		shape              = gears.shape.rounded_bar,
		shape_border_width = 2,
		widget             = wibox.container.background,
	},
	layout = wibox.layout.stack,
}

local function badge_widget(id_prefix, halign, valign)
	local lift = (valign == "bottom") and 2 or 0
	return {
		{
			{
				{
					{
						id     = id_prefix .. "_role",
						widget = wibox.widget.textbox,
						align  = "center",
						valign = "center",
						font   = "DejaVu Sans Mono Bold 6",
					},
					id     = id_prefix .. "_bg_role",
					shape  = gears.shape.circle,
					widget = wibox.container.background,
				},
				forced_width  = 8,
				forced_height = 8,
				widget        = wibox.container.constraint,
			},
			margins = 1,
			bottom  = 1 + lift,
			widget  = wibox.container.margin,
		},
		halign = halign,
		valign = valign,
		widget = wibox.container.place,
	}
end

local icon_fixed_size = {
	{
		icon_and_fallback,
		badge_widget("pin", "right", "top"),
		badge_widget("min", "right", "bottom"),
		badge_widget("ontop", "left", "top"),
		badge_widget("float", "left", "bottom"),
		layout = wibox.layout.stack,
	},
	strategy      = "exact",
	forced_width  = 20,
	forced_height = 20,
	widget        = wibox.container.constraint,
}

local icon_area = {
	icon_fixed_size,
	halign = "center",
	valign = "center",
	widget = wibox.container.place,
}

local task_template = {
	{
		icon_area,
		margins = 1,
		widget  = wibox.container.margin,
	},
	id              = "background_role",
	widget          = wibox.container.background,

	create_callback = update_task,
	update_callback = update_task,
}

-- Create a tasklist widget
local function widget(s)
	return awful.widget.tasklist({
		screen          = s,
		-- List clients from all screens, not just this one.
		source          = awful.widget.tasklist.source.all_clients,
		filter          = filter_all_screens,
		buttons         = customwidget.buttons,
		layout          = {
			spacing = 2,
			layout  = wibox.layout.fixed.vertical,
		},
		widget_template = task_template,
	})
end

customwidget.widget = widget

return customwidget
