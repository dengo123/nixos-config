-- ~/.config/awesome/widgets/bar/systray.lua
local wibox = require("wibox")
local beautiful = require("beautiful")

local M = {}

function M.build(opts)
	opts = opts or {}

	local bar_h = tonumber(beautiful.wibar_height) or 28
	local pad_h = opts.pad_h or (beautiful.systray_pad_h or 4)
	local pad_v = 0
	local base_size = opts.base_size or beautiful.systray_base_size or math.max(1, bar_h - 6)
	local bg = opts.bg or beautiful.systray_bg -- dunkler als wibar_bg

	local tray = wibox.widget.systray()
	tray:set_horizontal(true)
	tray:set_base_size(base_size)

	local placed = wibox.widget({
		tray,
		widget = wibox.container.place,
		halign = "left",
		valign = "center",
	})

	local with_margin = wibox.widget({
		placed,
		widget = wibox.container.margin,
		left = pad_h,
		right = pad_h,
		top = pad_v,
		bottom = pad_v,
	})

	local boxed = wibox.widget({
		with_margin,
		widget = wibox.container.background,
		bg = bg, -- färbt den gesamten Block (inkl. Padding)
	})

	return wibox.container.constraint(boxed, "exact", nil, bar_h) -- volle Wibar-Höhe
end

return M
