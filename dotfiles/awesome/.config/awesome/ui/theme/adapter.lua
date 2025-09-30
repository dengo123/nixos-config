-- features/shell/menu/theme/adapter.lua
local A = {}

local function clamp(x, a, b)
	return math.max(a, math.min(b, x))
end
local function hex_to_rgb(hex)
	hex = hex or "#000000"
	local r, g, b = hex:match("#?(%x%x)(%x%x)(%x%x)")
	return tonumber(r, 16), tonumber(g, 16), tonumber(b, 16)
end
local function rgb_to_hex(r, g, b)
	return string.format(
		"#%02X%02X%02X",
		clamp(math.floor(r + 0.5), 0, 255),
		clamp(math.floor(g + 0.5), 0, 255),
		clamp(math.floor(b + 0.5), 0, 255)
	)
end
local function adjust(hex, pct)
	local r, g, b = hex_to_rgb(hex)
	local f = 1 + pct / 100
	return rgb_to_hex(r * f, g * f, b * f)
end

local DEFAULTS = {
	-- minimale Defaults, die deine Widgets brauchen
	row_bg = "#FFFFFF",
	row_fg = "#000000",
	row_h = 48,
	row_pad_l = 10,
	row_pad_r = 10,
	row_pad_t = 4,
	row_pad_b = 4,
	row_spacing = 8,
	list_spacing = 0,
	icon_ratio = 0.9,
	text_ratio = 0.25,
	header_h = 56,
	footer_h = 48,
}

local function merge(a, b)
	local o = {}
	for k, v in pairs(a or {}) do
		o[k] = v
	end
	for k, v in pairs(b or {}) do
		o[k] = v
	end
	return o
end

-- Adapter: ui_theme (+ overrides) -> widget_theme (mit Resolvern + __raw_theme)
function A.to_widgets(ui_theme, overrides)
	local P = (ui_theme and ui_theme.palette) or {}
	local M = (ui_theme and ui_theme.metrics) or {}
	local o = overrides or {}

	-- Ableitung der bekannten Menü-/Spaltenkeys aus der UI-Palette
	local base = {
		bg = P.bg,
		fg = P.fg,
		header_bg = P.primary,
		header_fg = P.primary_fg,
		footer_bg = P.primary,
		footer_fg = P.primary_fg,
		left_bg = P.left_bg or "#FFFFFF",
		left_fg = P.left_fg or "#000000",
		right_bg = P.right_bg or "#D2E5FA",
		right_fg = P.right_fg or "#000000",

		popup_radius = M.radius or 12,
		popup_border_width = M.border_width or 1,
		header_h = M.header_h or DEFAULTS.header_h,
		footer_h = M.footer_h or DEFAULTS.footer_h,
		row_h = M.row_h or DEFAULTS.row_h,
	}

	local t = merge(DEFAULTS, merge(base, o))

	-- globale Hover-Ableitungen
	t.row_bg_hover = t.row_bg_hover or adjust(t.row_bg, -8)
	t.left_row_bg_hover = t.left_row_bg_hover or adjust(t.left_bg, -8)
	t.right_row_bg_hover = t.right_row_bg_hover or adjust(t.right_bg, -8)

	-- Resolver (kompatibel zu deinen Widgets)
	function t.resolve_text_size_number(_, eff_h, kind)
		local ratio = (kind == "header" and (t.text_ratio_header or t.text_ratio))
			or (kind == "rows" and (t.text_ratio_rows or t.text_ratio))
			or (kind == "power" and (t.text_ratio_power or t.text_ratio))
			or t.text_ratio
			or DEFAULTS.text_ratio
		return math.max(1, math.floor(eff_h * ratio + 0.5))
	end

	function t.resolve_font(_, eff_h, kind)
		local family = t.font_family or "Sans"
		return string.format("%s %d", family, t.resolve_text_size_number(t, eff_h, kind))
	end

	function t.resolve_icon_size(_, eff_h, kind)
		local ratio = (kind == "header" and (t.icon_ratio_header or t.icon_ratio))
			or (kind == "rows" and (t.icon_ratio_rows or t.icon_ratio))
			or (kind == "power" and (t.icon_ratio_power or t.icon_ratio))
			or t.icon_ratio
			or DEFAULTS.icon_ratio
		return math.max(1, math.floor(eff_h * ratio + 0.5))
	end

	t.adjust = adjust
	t.__raw_theme = true
	return t
end

return A
