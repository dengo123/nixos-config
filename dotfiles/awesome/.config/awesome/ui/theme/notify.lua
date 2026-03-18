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
		border = C.black,
		radius = dpi(12),
		icon_size = dpi(28),
		margin = dpi(12),
		border_w = dpi(1),
	}

	-- ---------------------------------------------------------------------
	-- Notify Center
	-- ---------------------------------------------------------------------

	beautiful.notify.center = {
		panel_radius = dpi(12),
		panel_border_width = dpi(1),
		panel_border = border,
		panel_bg = C.creme,

		header_bg = C.creme,
		header_fg = C.black,

		body_bg = C.creme,
		body_fg = C.black,

		width_factor = 0.28,
		height_factor = 0.45,
		min_width = dpi(320),
		min_height = dpi(220),
		max_width = dpi(520),
		max_height = dpi(720),

		offset_x = dpi(0),
		offset_y = dpi(0),

		margin_top = dpi(0),
		margin_right = dpi(0),
		margin_bottom = dpi(0),
		margin_left = dpi(0),

		padding = dpi(12),
		spacing = dpi(10),

		header_height = dpi(44),
		entry_height = dpi(88),
		empty_height = dpi(56),
	}
end

return M
