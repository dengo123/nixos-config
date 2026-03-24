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

	assert(type(C.secondary) == "string" and C.secondary ~= "", "windowing.theme: theme.colors.secondary fehlt")
	assert(type(C.primary) == "string" and C.primary ~= "", "windowing.theme: theme.colors.primary fehlt")
	assert(type(C.gray) == "string" and C.gray ~= "", "windowing.theme: theme.colors.gray fehlt")
	assert(type(C.white) == "string" and C.white ~= "", "windowing.theme: theme.colors.white fehlt")

	beautiful.border_width = dpi(3)
	beautiful.border_radius = dpi(10)
	beautiful.border_normal = C.secondary
	beautiful.border_focus = C.primary

	beautiful.titlebar_position = "top"
	beautiful.titlebar_height = dpi(28)
	beautiful.titlebar_bg_normal = C.secondary
	beautiful.titlebar_bg_focus = C.primary
	beautiful.titlebar_fg_normal = C.gray
	beautiful.titlebar_fg_focus = C.white
end

-- =========================================================================
-- Button Style
-- =========================================================================

function M.button_style(args)
	args = args or {}

	local theme = resolved_theme(args)
	local C = theme.colors or {}

	assert(type(C.white) == "string" and C.white ~= "", "windowing.theme: theme.colors.white fehlt")
	assert(type(C.gray) == "string" and C.gray ~= "", "windowing.theme: theme.colors.gray fehlt")
	assert(type(C.close) == "string" and C.close ~= "", "windowing.theme: theme.colors.close fehlt")
	assert(type(C.close_focus) == "string" and C.close_focus ~= "", "windowing.theme: theme.colors.close_focus fehlt")

	return {
		size = beautiful.titlebar_height,
		spacing = dpi(4),
		fg = C.white,
		fg_hover = C.gray,
		close = C.close,
		close_hover = C.close_focus,
	}
end

return M
