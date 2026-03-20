-- ~/.config/awesome/shell/windowing/ui/theme.lua
local beautiful = require("beautiful")
local xr = require("beautiful.xresources")

local dpi = xr.apply_dpi

local M = {}

-- =========================================================================
-- Theme
-- =========================================================================

function M.init(cfg)
	local C = (cfg and cfg.colors) or require("ui.colors").get()

	beautiful.border_width = 3
	beautiful.border_radius = 10
	beautiful.border_normal = C.blue_light
	beautiful.border_focus = C.blue_luna

	beautiful.titlebar_position = "top"
	beautiful.titlebar_height = 28
	beautiful.titlebar_bg_normal = C.blue_light
	beautiful.titlebar_bg_focus = C.blue_luna
	beautiful.titlebar_fg_normal = C.gray
	beautiful.titlebar_fg_focus = C.white
end

-- =========================================================================
-- Button Style
-- =========================================================================

function M.button_style(cfg)
	local C = (cfg and cfg.colors) or require("ui.colors").get()

	return {
		size = beautiful.titlebar_height,
		spacing = 4,
		fg = C.white,
		fg_hover = C.gray,
		close = C.red,
		close_hover = C.pink,
	}
end

return M
