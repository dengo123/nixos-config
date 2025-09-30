-- ~/.config/awesome/ui/theme/tabs.lua
local beautiful = require("beautiful")

local T = {}

-- Defaults + beautiful-Übernahmen
local DEFAULTS = {
	spacing = 6,
	radius = function()
		return beautiful.border_radius or 6
	end,
	pad_h = 8,
	pad_v = 3,
	icon_size = 16,
	title_len = 18,

	colors = {
		focus_bg = function()
			return beautiful.tasklist_bg_focus or beautiful.bg_focus or "#4C6EF5"
		end,
		focus_fg = function()
			return beautiful.tasklist_fg_focus or beautiful.fg_focus or "#FFFFFF"
		end,
		normal_bg = function()
			return beautiful.tasklist_bg_normal or "#00000000"
		end,
		normal_fg = function()
			return beautiful.tasklist_fg_normal or beautiful.fg_normal or "#DDDDDD"
		end,
		minimize_bg = function()
			return beautiful.tasklist_bg_minimize or "#00000000"
		end,
		minimize_fg = function()
			return beautiful.tasklist_fg_minimize or beautiful.fg_minimize or "#AAAAAA"
		end,
	},
}

-- optional
function T.init(_cfg) end

-- Liefert ein aufgelöstes Theme-Objekt (mit finalen Werten)
function T.get(overrides)
	overrides = overrides or {}
	local function v(x)
		return type(x) == "function" and x() or x
	end

	local theme = {
		spacing = overrides.spacing or DEFAULTS.spacing,
		radius = overrides.radius or v(DEFAULTS.radius),
		pad_h = overrides.pad_h or DEFAULTS.pad_h,
		pad_v = overrides.pad_v or DEFAULTS.pad_v,
		icon_size = overrides.icon_size or DEFAULTS.icon_size,
		title_len = overrides.title_len or DEFAULTS.title_len,
		colors = {
			focus_bg = (overrides.colors and overrides.colors.focus_bg) or v(DEFAULTS.colors.focus_bg),
			focus_fg = (overrides.colors and overrides.colors.focus_fg) or v(DEFAULTS.colors.focus_fg),
			normal_bg = (overrides.colors and overrides.colors.normal_bg) or v(DEFAULTS.colors.normal_bg),
			normal_fg = (overrides.colors and overrides.colors.normal_fg) or v(DEFAULTS.colors.normal_fg),
			minimize_bg = (overrides.colors and overrides.colors.minimize_bg) or v(DEFAULTS.colors.minimize_bg),
			minimize_fg = (overrides.colors and overrides.colors.minimize_fg) or v(DEFAULTS.colors.minimize_fg),
		},
	}
	return theme
end

return T
