-- ~/.config/awesome/shell/menu/layout.lua
local xr = require("beautiful.xresources")

local dpi = xr.apply_dpi

local M = {}

local runtime = {
	ctx = {},
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
	if not runtime.layout then
		runtime.layout = default_layout()
	end

	return runtime.layout
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = (args and (args.ctx or args)) or {}
	runtime.layout = default_layout()
	return M
end

function M.get()
	return ensure_layout()
end

function M.item_height()
	local layout = M.get()
	return assert(tonumber(layout.item_height), "shell.menu: layout.item_height ungültig")
end

function M.total_height(item_count)
	return M.item_height() * (item_count or 0)
end

return M
