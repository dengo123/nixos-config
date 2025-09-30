-- ~/.config/awesome/features/workspaces/layouts.lua
local awful = require("awful")

local M = {
	list = {
		awful.layout.suit.tile,
		awful.layout.suit.tile.top,
	},
}

function M.apply()
	awful.layout.layouts = M.list
end

return M
