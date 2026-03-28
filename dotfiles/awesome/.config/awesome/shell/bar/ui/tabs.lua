-- ~/.config/awesome/shell/bar/ui/tabs.lua
local beautiful = require("beautiful")
local xr = require("beautiful.xresources")

local dpi = xr.apply_dpi

local T = {}

local runtime = {
	ui = {},
}

-- =========================================================================
-- Helpers
-- =========================================================================

local function resolved_theme()
	local ui = runtime.ui or {}
	return ui.theme or {}
end

-- =========================================================================
-- Public API
-- =========================================================================

function T.init(opts)
	opts = opts or {}
	runtime.ui = opts.ui or runtime.ui or {}

	local theme = resolved_theme()
	local colors = theme.colors or {}

	local wibar_height = tonumber(beautiful.wibar_height) or 28
	local icon_size = math.floor(wibar_height * 0.8)

	beautiful.tabs = {
		spacing = dpi(2),
		radius = dpi(5),
		pad_h = dpi(8),
		pad_v = dpi(3),

		icon_size = icon_size,
		title_len = 18,
		title_offset_y = dpi(3),
		width_factor = 6,

		inactive_border_width = dpi(1),
	}

	beautiful.tabs_colors = {
		accent = colors.tertiary,
		focus_bg = colors.tertiary,
		focus_fg = colors.text_invert,
		focus_border = colors.tertiary,

		normal_bg = colors.transparent,
		normal_fg = colors.text_invert,
		normal_border = colors.tertiary,

		minimize_bg = colors.transparent,
		minimize_fg = colors.text_invert,
		minimize_border = colors.transparent,
	}

	return T
end

function T.get()
	local tabs_theme = beautiful.tabs or {}
	local colors = beautiful.tabs_colors or {}

	return {
		spacing = tabs_theme.spacing,
		radius = tabs_theme.radius,
		pad_h = tabs_theme.pad_h,
		pad_v = tabs_theme.pad_v,

		icon_size = tabs_theme.icon_size,
		title_len = tabs_theme.title_len,
		title_offset_y = tabs_theme.title_offset_y,
		width_factor = tabs_theme.width_factor,

		inactive_border_width = tabs_theme.inactive_border_width,

		colors = {
			accent = colors.accent,
			focus_bg = colors.focus_bg,
			focus_fg = colors.focus_fg,
			focus_border = colors.focus_border,

			normal_bg = colors.normal_bg,
			normal_fg = colors.normal_fg,
			normal_border = colors.normal_border,

			minimize_bg = colors.minimize_bg,
			minimize_fg = colors.minimize_fg,
			minimize_border = colors.minimize_border,
		},
	}
end

return T
