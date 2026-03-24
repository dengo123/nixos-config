-- ~/.config/awesome/shell/windowing/ui/theme.lua
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
-- Theme
-- =========================================================================

function M.init(args)
	args = args or {}

	local theme = resolved_theme(args)
	local C = theme.colors or {}

	beautiful.border_width = dpi(3)
	beautiful.border_radius = dpi(10)
	beautiful.border_normal = C.secondary
	beautiful.border_focus = C.primary

	beautiful.titlebar_position = "top"
	beautiful.titlebar_height = dpi(28)
	beautiful.titlebar_bg_normal = C.secondary
	beautiful.titlebar_bg_focus = C.primary
	beautiful.titlebar_fg_normal = C.text_invert_focus
	beautiful.titlebar_fg_focus = C.text_invert or C.background
end

-- =========================================================================
-- Button Style
-- =========================================================================

function M.button_style(args)
	args = args or {}

	local theme = resolved_theme(args)
	local C = theme.colors or {}

	return {
		size = beautiful.titlebar_height,
		spacing = dpi(4),
		fg = C.text_invert or C.background,
		fg_hover = C.text_invert_focus,
		close = C.close,
		close_hover = C.close_focus,
	}
end

return M
