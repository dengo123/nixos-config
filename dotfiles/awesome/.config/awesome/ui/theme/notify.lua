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
		width_factor = 0.30,
		min_width = dpi(320),
		max_width = dpi(520),

		min_height = dpi(80),
		max_height = dpi(720),

		offset_x = dpi(0),
		offset_y = dpi(0),

		margin_top = dpi(8),
		margin_right = dpi(8),
		margin_bottom = dpi(0),

		panel_bg = "#00000000",

		entry_bg = C.creme,
		entry_fg = C.black,
		entry_border = C.black,

		entry_radius = dpi(12),
		entry_border_w = dpi(1),
		entry_padding = dpi(6),
		entry_spacing = dpi(6),

		text_inset_top = dpi(4),
		text_inset_bottom = dpi(4),

		list_pad_top = dpi(0),
		list_pad_right = dpi(0),
		list_pad_bottom = dpi(0),
		list_pad_left = dpi(0),
	}
end

return M
