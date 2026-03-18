-- ~/.config/awesome/ui/theme/notify.lua
local beautiful = require("beautiful")
local xr = require("beautiful.xresources")

local dpi = xr.apply_dpi

local M = {}

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(cfg)
	cfg = cfg or {}

	-- ---------------------------------------------------------------------
	-- Colors
	-- ---------------------------------------------------------------------

	local C = cfg.colors or {}
	local H = cfg.helpers or {}

	local border = (H and H.adjust_color and C.creme) and H.adjust_color(C.creme, -14) or (C.black or "#000000")

	-- ---------------------------------------------------------------------
	-- Notify
	-- ---------------------------------------------------------------------

	beautiful.notify = {
		bg = C.creme,
		fg = C.black,
		border = border,

		radius = dpi(8),
		icon_size = dpi(24),
		margin = dpi(10),
		border_w = dpi(1),
	}
end

return M
