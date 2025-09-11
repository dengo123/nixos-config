-- ~/.config/awesome/widgets/bar/clock.lua
local wibox = require("wibox")
local M = {}

function M.build(fmt)
	return wibox.widget.textclock(fmt or "%a %d.%m.  %H:%M")
end

return M
