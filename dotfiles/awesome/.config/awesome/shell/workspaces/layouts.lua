-- ~/.config/awesome/shell/workspaces/layouts.lua
local awful = require("awful")

local M = {}

-- Globale Reihenfolge (für Widgets/awful.layout.inc)
M.list = {
	awful.layout.suit.max,
	awful.layout.suit.fair,
	awful.layout.suit.tile,
	awful.layout.suit.fair.horizontal,
	awful.layout.suit.tile.top,
}

function M.apply()
	awful.layout.layouts = M.list
end

return M
