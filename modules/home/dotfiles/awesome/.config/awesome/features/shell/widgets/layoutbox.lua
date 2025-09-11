-- ~/.config/awesome/widgets/bar/layoutbox.lua
local awful = require("awful")
local gears = require("gears")

local M = {}

function M.build(s)
	local lb = awful.widget.layoutbox(s)

	lb:buttons(gears.table.join(
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

	return lb
end

return M
