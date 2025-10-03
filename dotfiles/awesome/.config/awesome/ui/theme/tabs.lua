-- ~/.config/awesome/ui/theme/tabs.lua
local beautiful = require("beautiful")
local xr = require("beautiful.xresources")
local dpi = xr.apply_dpi

local T = {}

-- Setzt die Quelle-der-Wahrheit in beautiful.* anhand der injizierten Palette
function T.init(cfg)
	cfg = cfg or {}
	local C = cfg.colors or {}

	-- Größen/Geometrie aus Umgebung ableiten
	local wibar_h = tonumber(beautiful.wibar_height) or 28
	local icon_sz = math.floor(wibar_h * 0.8)

	-- Maße/Style (fix)
	beautiful.tabs = {
		spacing = dpi(2),
		radius = dpi(5),
		pad_h = dpi(8),
		pad_v = dpi(3),

		icon_size = icon_sz,
		title_len = 18,
		width_factor = 6,

		-- Dünner Rand für inaktive Tabs; die RandFARBE kommt aus colors.focus_bg
		inactive_border_width = dpi(1),
	}

	-- Farben aus injizierter Palette (keine Hexcodes hier)
	beautiful.tabs_colors = {
		accent = C.blue_dark,
		focus_bg = C.blue_dark,
		focus_fg = C.white,
		focus_border = C.blue_dark, -- Vollständigkeit; Widget nutzt es ggf. nicht

		normal_bg = C.transparent,
		normal_fg = C.gray,
		normal_border = C.blue_dark, -- Referenz; Widget nutzt meist focus_bg als Border

		minimize_bg = C.transparent,
		minimize_fg = C.gray,
		minimize_border = C.transparent,
	}
end

-- Liefert ein konsumierbares Theme-Objekt für widgets/tabs.lua (ohne Overrides)
function T.get(_)
	local S = beautiful.tabs or {}
	local C = beautiful.tabs_colors or {}

	return {
		-- Layout / Maße
		spacing = S.spacing,
		radius = S.radius,
		pad_h = S.pad_h,
		pad_v = S.pad_v,

		-- Größen
		icon_size = S.icon_size,
		title_len = S.title_len,
		width_factor = S.width_factor,

		-- Border
		inactive_border_width = S.inactive_border_width,

		-- Farben
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
