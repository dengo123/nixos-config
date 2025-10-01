-- ~/.config/awesome/ui/theme/tabs.lua
local beautiful = require("beautiful")

local T = {}

local DEFAULTS = {
	spacing = 2,
	radius = function()
		return beautiful.border_radius or 6
	end,
	pad_h = 8,
	pad_v = 3,
	-- ICON: größer wählen (≈ 80% der Wibar-Höhe, Fallback 28px)
	icon_size = math.floor((tonumber(beautiful.wibar_height) or 28) * 0.8),
	title_len = 18,
	width_factor = 6,
	inactive_border_width = 1,
	colors = {
		accent = "#235CDB",
		focus_bg = "#1A50B8",
		focus_fg = "#FFFFFF",
		focus_border = "#00000000",
		normal_bg = "#00000000",
		normal_fg = "#DDDDDD",
		normal_border = "#235CDB",
		minimize_bg = "#00000000",
		minimize_fg = "#AAAAAA",
		minimize_border = "#00000000",
	},
}

function T.init(_) end

function T.get(overrides)
	overrides = overrides or {}
	local O, C = overrides, (overrides.colors or {})
	return {
		spacing = O.spacing or DEFAULTS.spacing,
		radius = O.radius or DEFAULTS.radius(),
		pad_h = O.pad_h or DEFAULTS.pad_h,
		pad_v = O.pad_v or DEFAULTS.pad_v,
		icon_size = O.icon_size or DEFAULTS.icon_size, -- << größer
		title_len = O.title_len or DEFAULTS.title_len,
		width_factor = O.width_factor or DEFAULTS.width_factor,
		inactive_border_width = O.inactive_border_width or DEFAULTS.inactive_border_width,
		colors = {
			accent = C.accent or DEFAULTS.colors.accent,
			focus_bg = C.focus_bg or DEFAULTS.colors.focus_bg,
			focus_fg = C.focus_fg or DEFAULTS.colors.focus_fg,
			focus_border = C.focus_border or DEFAULTS.colors.focus_border,
			normal_bg = C.normal_bg or DEFAULTS.colors.normal_bg,
			normal_fg = C.normal_fg or DEFAULTS.colors.normal_fg,
			normal_border = C.normal_border or DEFAULTS.colors.normal_border,
			minimize_bg = C.minimize_bg or DEFAULTS.colors.minimize_bg,
			minimize_fg = C.minimize_fg or DEFAULTS.colors.minimize_fg,
			minimize_border = C.minimize_border or DEFAULTS.colors.minimize_border,
		},
	}
end

return T
