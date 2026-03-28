-- ~/.config/awesome/shell/menu/ui/layout.lua
local xr = require("beautiful.xresources")

local dpi = xr.apply_dpi

local M = {}

local runtime = {
	layout = nil,
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function default_layout()
	return {
		item_height = dpi(28),
		width = dpi(220),
		gap = dpi(2),
		x_padding = dpi(0),
		align = "left",
		x_offset = 0,
	}
end

local function ensure_layout()
	if type(runtime.layout) ~= "table" then
		runtime.layout = default_layout()
	end

	return runtime.layout
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init()
	runtime.layout = default_layout()
	return M
end

function M.get()
	return ensure_layout()
end

function M.item_height()
	local layout = ensure_layout()
	assert(layout ~= nil, "menu.ui: layout ungültig")

	local item_height = tonumber(layout.item_height)
	assert(item_height ~= nil, "menu.ui.layout: item_height ungültig")

	return item_height
end

function M.total_height(item_count)
	return M.item_height() * (item_count or 0)
end

return M
