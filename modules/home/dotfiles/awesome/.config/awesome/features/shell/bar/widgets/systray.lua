-- ~/.config/awesome/widgets/bar/systray.lua
local wibox = require("wibox")

local M = {}

-- opts.base_size: Pixel-Größe der Tray-Icons (Default 20)
function M.build(opts)
	opts = opts or {}
	local tray = wibox.widget.systray()
	tray:set_base_size(opts.base_size or 20)

	-- Abstand vom oberen Bar-Rand: 3 px
	local margin = wibox.container.margin(tray, 0, 0, 4, 0)
	return margin
end

return M
