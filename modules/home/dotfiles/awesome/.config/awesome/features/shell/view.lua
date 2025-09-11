-- features/shell/view.lua
-- positioniert die Bar am Screen

local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")

local V = {}

function V.place(s, model, opts)
	opts = opts or {}
	s.mywibox = awful.wibar({
		position = opts.position or "bottom",
		screen = s,
		height = beautiful.wibar_height or 28,
		bg = beautiful.wibar_bg or beautiful.bg_normal,
		fg = beautiful.wibar_fg or beautiful.fg_normal,
	})

	s.mywibox:setup({
		layout = wibox.layout.align.horizontal,
		model.left,
		model.center,
		model.right,
	})
	return s.mywibox
end

return V
