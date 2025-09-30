-- ~/.config/awesome/features/shell/widgets/clock.lua
-- (oder: ~/.config/awesome/shell/bar/widgets/clock.lua)
local wibox = require("wibox")
local awful = require("awful")
local beautiful = require("beautiful")

local M = {}

function M.build(fmt)
	local bar_h = tonumber(beautiful.wibar_height) or 28
	local pad_h = beautiful.clock_pad_h or 6
	local pad_v = beautiful.clock_pad_v or 0
	local bg = beautiful.clock_bg or beautiful.wibar_bg
	local format = fmt or "%H:%M"

	-- Uhr + Kalender
	local clock = wibox.widget.textclock(format)
	local cal = awful.widget.calendar_popup.month({
		start_sunday = false,
		long_weekdays = true,
	})
	cal:attach(clock, "br")

	-- vertikal mittig, keine top/bottom-Margen → Block füllt die Bar-Höhe
	local placed = wibox.widget({
		clock,
		widget = wibox.container.place,
		halign = "center",
		valign = "center",
	})

	local with_margin = wibox.widget({
		placed,
		widget = wibox.container.margin,
		left = pad_h,
		right = pad_h,
		top = 0,
		bottom = 0,
	})

	local bg_block = wibox.widget({
		with_margin,
		widget = wibox.container.background,
		bg = bg, -- gleiche Farbe wie Systray-Block
	})

	-- exakt auf Wibar-Höhe constrainen
	return wibox.container.constraint(bg_block, "exact", nil, bar_h)
end

return M
