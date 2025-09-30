-- ~/.config/awesome/widgets/bar/systray.lua
local wibox = require("wibox")
local beautiful = require("beautiful")

local M = {}

function M.build(opts)
	opts = opts or {}
	local bar_h = tonumber(beautiful.wibar_height) or 28
	local pad_h = opts.pad_h or (beautiful.systray_pad_h or 4)
	local base_size = opts.base_size or beautiful.systray_base_size or math.max(1, bar_h - 6)

	local tray = wibox.widget.systray()
	tray:set_horizontal(true)
	tray:set_base_size(base_size)

	local centered = wibox.widget({
		tray,
		widget = wibox.container.place,
		halign = "left",
		valign = "center",
	})

	local with_margin = wibox.widget({
		centered,
		widget = wibox.container.margin,
		left = pad_h,
		right = pad_h,
	})

	return wibox.container.constraint(with_margin, "exact", nil, bar_h)
end

return M
