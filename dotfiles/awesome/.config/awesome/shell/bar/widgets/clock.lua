-- features/shell/widgets/clock.lua

local wibox = require("wibox")
local awful = require("awful")

local M = {}

function M.build(fmt)
	-- Textclock Widget (nur HH:MM)
	local clock = wibox.widget.textclock(fmt or "%H:%M")

	-- ✅ Monatskalender als Popup, an die Uhr "unten rechts" anheften
	local cal = awful.widget.calendar_popup.month({
		start_sunday = false,
		long_weekdays = true,
	})
	-- Positionskürzel: "tr","tl","br","bl" sind erlaubt
	cal:attach(clock, "br")

	-- Abstand vom Bildschirmrand (10px rechts/unten)
	local margin = wibox.container.margin(clock, 0, 10, 0, 0)
	return margin
end

return M
