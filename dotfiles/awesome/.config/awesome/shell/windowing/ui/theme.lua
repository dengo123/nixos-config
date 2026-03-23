-- ~/.config/awesome/shell/windowing/ui/theme.lua
local beautiful = require("beautiful")
local xr = require("beautiful.xresources")

local dpi = xr.apply_dpi

local M = {}

-- =========================================================================
-- Helpers
-- =========================================================================

local function resolve_colors(cfg)
	local Colors = require("ui.colors")

	if type(Colors.set_runtime_cfg) == "function" then
		Colors.set_runtime_cfg(cfg or {})
	end

	return Colors.get()
end

-- =========================================================================
-- Theme
-- =========================================================================

function M.init(cfg)
	local C = resolve_colors(cfg)

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

function M.button_style(cfg)
	local C = resolve_colors(cfg)

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
