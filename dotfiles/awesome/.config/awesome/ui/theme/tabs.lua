-- ~/.config/awesome/ui/theme/tabs.lua
local beautiful = require("beautiful")
local xr = require("beautiful.xresources")
local dpi = xr.apply_dpi

local T = {}

-- ============================================================================
-- Init
-- ============================================================================

function T.init(cfg)
	cfg = cfg or {}

	local C = cfg.colors or {}
	local wibar_h = tonumber(beautiful.wibar_height) or 28
	local icon_sz = math.floor(wibar_h * 0.8)

	-- =========================================================================
	-- Tabs
	-- =========================================================================

	beautiful.tabs = {
		spacing = dpi(2),
		radius = dpi(5),
		pad_h = dpi(8),
		pad_v = dpi(3),

		icon_size = icon_sz,
		title_len = 18,
		width_factor = 6,

		inactive_border_width = dpi(1),
	}

	-- =========================================================================
	-- Colors
	-- =========================================================================

	beautiful.tabs_colors = {
		accent = C.blue_dark,
		focus_bg = C.blue_dark,
		focus_fg = C.white,
		focus_border = C.blue_dark,

		normal_bg = C.transparent,
		normal_fg = C.gray,
		normal_border = C.blue_dark,

		minimize_bg = C.transparent,
		minimize_fg = C.gray,
		minimize_border = C.transparent,
	}
end

-- ============================================================================
-- Public Theme Object
-- ============================================================================

function T.get()
	local S = beautiful.tabs or {}
	local C = beautiful.tabs_colors or {}

	return {
		spacing = S.spacing,
		radius = S.radius,
		pad_h = S.pad_h,
		pad_v = S.pad_v,

		icon_size = S.icon_size,
		title_len = S.title_len,
		width_factor = S.width_factor,

		inactive_border_width = S.inactive_border_width,

		colors = {
			accent = C.accent,
			focus_bg = C.focus_bg,
			focus_fg = C.focus_fg,
			focus_border = C.focus_border,

			normal_bg = C.normal_bg,
			normal_fg = C.normal_fg,
			normal_border = C.normal_border,

			minimize_bg = C.minimize_bg,
			minimize_fg = C.minimize_fg,
			minimize_border = C.minimize_border,
		},
	}
end

return T
