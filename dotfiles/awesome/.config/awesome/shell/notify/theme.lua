local beautiful = require("beautiful")
local xr = require("beautiful.xresources")

local dpi = xr.apply_dpi

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function resolved_theme(args)
	local ui = (args and args.ui) or {}
	return ui.theme or {}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	args = args or {}

	local theme = resolved_theme(args)
	local C = theme.colors or {}

	-- ---------------------------------------------------------------------
	-- Notify
	-- ---------------------------------------------------------------------

	beautiful.notify = {
		bg = C.surface,
		fg = C.text or C.foreground,
		border = C.surface_focus,
		radius = dpi(10),
		icon_size = dpi(28),
		margin = dpi(8),
		border_w = dpi(1),
		width = dpi(360),
	}

	-- ---------------------------------------------------------------------
	-- Notify Center
	-- ---------------------------------------------------------------------

	beautiful.notify.center = {
		width_factor = 0.30,
		min_width = dpi(320),
		max_width = dpi(360),

		min_height = dpi(80),
		max_height = dpi(720),

		offset_x = dpi(0),
		offset_y = dpi(0),

		margin_top = dpi(8),
		margin_right = dpi(8),
		margin_bottom = dpi(0),

		panel_bg = C.transparent,

		entry_bg = C.surface,
		entry_fg = C.text or C.foreground,
		entry_border = C.surface_focus,

		entry_radius = dpi(10),
		entry_border_w = dpi(1),
		entry_padding = dpi(6),
		entry_spacing = dpi(6),

		entry_height = dpi(74),
		entry_height_with_actions = dpi(92),

		text_inset_top = dpi(4),
		text_inset_bottom = dpi(4),

		list_pad_top = dpi(0),
		list_pad_right = dpi(0),
		list_pad_bottom = dpi(0),
		list_pad_left = dpi(0),

		action_bg = C.surface,
		action_fg = C.text or C.foreground,
		action_border = C.surface_focus,
		action_radius = dpi(8),
		action_border_w = dpi(1),
		action_spacing = dpi(6),
		action_padding_h = dpi(8),
		action_padding_v = dpi(4),
	}
end

return M
