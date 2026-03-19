-- ~/.config/awesome/shell/menu/layout.lua
local xr = require("beautiful.xresources")

local dpi = xr.apply_dpi

local M = {}

local runtime = nil

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(_cfg)
	runtime = {
		item_height = dpi(28),
		width = dpi(220),
		gap = dpi(4),
		x_padding = dpi(0),
		align = "left",
		x_offset = 0,
	}
end

function M.get()
	if not runtime then
		M.init({})
	end

	return runtime
end

function M.item_height()
	local layout = M.get()
	return assert(tonumber(layout.item_height), "shell.menu: layout.item_height ungültig")
end

function M.total_height(item_count)
	return M.item_height() * (item_count or 0)
end

return M
