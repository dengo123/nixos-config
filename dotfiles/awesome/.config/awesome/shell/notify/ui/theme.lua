-- ~/.config/awesome/shell/notify/theme.lua
-- ~/.config/awesome/shell/notify/ui/theme.lua
local beautiful = require("beautiful")
local xr = require("beautiful.xresources")

local dpi = xr.apply_dpi

local M = {}

local runtime = {
	ctx = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function ctx()
	return runtime.ctx or {}
end

local function resolved_theme(args)
	local c = (args and (args.ctx or args)) or ctx()
	local ui = (args and args.ui) or c.ui or {}
	return ui.theme or {}
end

-- =========================================================================
-- Public API
-- =========================================================================

function M.init(args)
	runtime.ctx = (args and (args.ctx or args)) or {}
	args = args or {}

	local theme = resolved_theme(args)
	local C = theme.colors or {}
	local F = theme.fonts or {}
	local U = theme.utils or {}

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
		padding = dpi(8),
		spacing = dpi(8),
		border_w = dpi(1),
	}

	-- ---------------------------------------------------------------------
	-- Notify Center
	-- ---------------------------------------------------------------------

	beautiful.notify.center = {
		width_factor = 0.30,
		min_width = dpi(320),
		max_width = dpi(360),

		offset_x = dpi(0),
		offset_y = dpi(0),

		margin_top = dpi(8),
		margin_right = dpi(8),
		margin_bottom = dpi(10),

		panel_bg = U.transparent,

		entry_bg = C.surface,
		entry_fg = C.text or C.foreground,
		entry_border = C.surface_focus,

		entry_bg_hover = C.surface_focus,
		entry_fg_hover = C.text or C.foreground,
		entry_border_hover = C.surface_focus,

		entry_bg_focus = C.surface_focus,
		entry_fg_focus = C.text or C.foreground,
		entry_border_focus = C.surface_focus,

		entry_radius = dpi(10),
		entry_border_w = dpi(1),
		entry_padding = dpi(6),
		entry_spacing = dpi(6),

		text_inset_top = dpi(4),
		text_inset_bottom = dpi(4),

		list_pad_top = dpi(8),
		list_pad_right = dpi(0),
		list_pad_bottom = dpi(0),
		list_pad_left = dpi(0),

		title_font = F.ui_bold or F.ui,
		message_font = F.ui,

		action_fg = C.text or C.foreground,
		action_fg_hover = C.text_focus,
		action_spacing = dpi(8),
		action_row_spacing = dpi(6),
		action_icon = "▪",
		action_icon_font = F.ui_bold or F.ui,
		action_font = F.ui,
	}

	return M
end

return M
