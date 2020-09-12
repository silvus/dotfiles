local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")

local styles = {}
styles.normal = {
	fg_color	 = "#FFFFFF",
	bg_color	 = "#D3D3D320",
}
styles.month = {
	padding		 = 20,
	fg_color	 = "#FFFFFF",
	bg_color	 = beautiful.bg_normal.."00",
	border_width = 0,
}
styles.focus = {
	fg_color = beautiful.success,
	bg_color = "#00000000",
	-- markup   = function(t) return '<b>' .. t .. '</b>' end,
}
styles.header = {
	fg_color = "#D3D3D3",
	bg_color = "#00000000",
}
styles.weekday = {
	fg_color = "#E3E3E3",
	bg_color = "#00000000",
	padding  = 3,
}
styles.weeknumber = {
	fg_color	 = beautiful.fg_focus,
	bg_color	 = beautiful.bg_focus.."60",
}

local function decorate_cell(widget, flag, date)
		if flag =='monthheader' and not styles.monthheader then
		flag = 'header'
	end
	local props = styles[flag] or {}
	if props.markup and widget.get_text and widget.set_markup then
		widget:set_markup(props.markup(widget:get_text()))
	end
	local default_bg = "#D3D3D320"
	local default_fg = "#ffffff"
	local ret = wibox.widget {
		{
			widget,
			margins = (props.padding or 2) + (props.border_width or 0),
			widget  = wibox.container.margin
		},
		shape			  = props.shape,
		shape_border_color = props.border_color or beautiful.bg_normal,
		shape_border_width = props.border_width or 0,
		fg				 = props.fg_color or default_fg,
		bg				 = props.bg_color or default_bg,
		widget			 = wibox.container.background
	}
	return ret
end

calendar_widget = wibox.widget {
	date	 = os.date('*t'),
	-- font	 = "sans medium 13",
	long_weekdays = false,
	week_numbers = true,
	start_sunday = false,
	spacing  = 3,
	fn_embed = decorate_cell,
	widget   = wibox.widget.calendar.month
}

local current_month = os.date('*t').month
calendar_widget:buttons(gears.table.join(
	-- Left Click - Reset date to current date
	awful.button({}, 1, function ()
		calendar_widget.date = os.date('*t')
	end),
	-- Scroll - Move to previous or next month
	awful.button({}, 4, function ()
		new_calendar_month = calendar_widget.date.month - 1
		if new_calendar_month == current_month then
			calendar_widget.date = os.date('*t')
		else
			calendar_widget.date = {month = new_calendar_month, year = calendar_widget.date.year}
		end
	end),
	awful.button({}, 5, function ()
		new_calendar_month = calendar_widget.date.month + 1
		if new_calendar_month == current_month then
			calendar_widget.date = os.date('*t')
		else
			calendar_widget.date = {month = new_calendar_month, year = calendar_widget.date.year}
		end
	end)
))

return calendar_widget
