-- ~/.config/awesome/shell/bar/widgets/layoutbox.lua
local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")

local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.build(s)
	if not s.mylayoutbox or not s.mylayoutbox.valid then
		s.mylayoutbox = awful.widget.layoutbox(s)
		s.mylayoutbox:buttons(gears.table.join(
			awful.button({}, 1, function()
				awful.layout.inc(1)
			end),
			awful.button({}, 3, function()
				awful.layout.inc(-1)
			end),
			awful.button({}, 4, function()
				awful.layout.inc(1)
			end),
			awful.button({}, 5, function()
				awful.layout.inc(-1)
			end)
		))
	end

	return wibox.widget({
		s.mylayoutbox,
		left = beautiful.layoutbox_pad_h or 0,
		right = beautiful.layoutbox_pad_h or 0,
		top = beautiful.layoutbox_pad_v or 0,
		bottom = beautiful.layoutbox_pad_v or 0,
		widget = wibox.container.margin,
	})
end

return M
