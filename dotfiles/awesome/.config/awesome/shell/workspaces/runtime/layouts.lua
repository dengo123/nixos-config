-- ~/.config/awesome/shell/workspaces/runtime/layouts.lua
local awful = require("awful")

local M = {
	list = {
		awful.layout.suit.max,
		awful.layout.suit.fair,
		awful.layout.suit.tile,
		awful.layout.suit.fair.horizontal,
		awful.layout.suit.tile.top,
	},
}

local by_name = {
	max = awful.layout.suit.max,
	fair = awful.layout.suit.fair,
	tile = awful.layout.suit.tile,
	fair_horizontal = awful.layout.suit.fair.horizontal,
	tile_top = awful.layout.suit.tile.top,
}

-- =========================================================================
-- Public API
-- =========================================================================

function M.apply()
	awful.layout.layouts = M.list
end

function M.resolve(name)
	local key = tostring(name or ""):lower()
	local layout = by_name[key]

	assert(layout ~= nil, "workspaces.layouts: layout ungueltig: " .. tostring(name))

	return layout
end

return M
