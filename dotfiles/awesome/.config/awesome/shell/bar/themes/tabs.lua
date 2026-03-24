-- ~/.config/awesome/shell/bar/themes/tabs.lua
local beautiful = require("beautiful")
local xr = require("beautiful.xresources")

local dpi = xr.apply_dpi

local T = {}

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

function T.init(args)
	args = args or {}

	local theme = resolved_theme(args)
	local C = theme.colors or {}

	assert(type(C.tertiary) == "string" and C.tertiary ~= "", "bar.theme.tabs: theme.colors.tertiary fehlt")
	assert(type(C.white) == "string" and C.white ~= "", "bar.theme.tabs: theme.colors.white fehlt")
	assert(type(C.gray) == "string" and C.gray ~= "", "bar.theme.tabs: theme.colors.gray fehlt")
	assert(type(C.transparent) == "string" and C.transparent ~= "", "bar.theme.tabs: theme.colors.transparent fehlt")

	local wibar_height = tonumber(beautiful.wibar_height) or 28
	local icon_size = math.floor(wibar_height * 0.8)

	beautiful.tabs = {
		spacing = dpi(2),
		radius = dpi(5),
		pad_h = dpi(8),
		pad_v = dpi(3),

		icon_size = icon_size,
		title_len = 18,
		width_factor = 6,

		inactive_border_width = dpi(1),
	}

	beautiful.tabs_colors = {
		accent = C.tertiary,
		focus_bg = C.tertiary,
		focus_fg = C.white,
		focus_border = C.tertiary,

		normal_bg = C.transparent,
		normal_fg = C.gray,
		normal_border = C.tertiary,

		minimize_bg = C.transparent,
		minimize_fg = C.gray,
		minimize_border = C.transparent,
	}
end

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
