-- features/shell/menu/theme.lua
local Theme = {}

-- ===== Utils =====
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

-- ===== Vereinheitlichte Defaults (Container + Widgets) =====
local DEFAULTS_CONTAINER = {
	-- Grundfarben
	bg = "#222222",
	fg = "#FFFFFF",

	-- Popup / Rahmen
	popup_radius = 12,
	popup_border_width = 1,
	popup_border_color = "#235CDB",
	popup_bg = "#00000000", -- äußerer Container
	dialog_bg = "#235CDB", -- Innenfläche / Columns-Hintergrund

	-- Header
	header_bg = "#235CDB",
	header_fg = "#FFFFFF",
	header_h = 56,
	header_pad_h = 12,
	header_pad_v = 8,
	header_spacing = 10,

	-- Footer
	footer_bg = "#235CDB",
	footer_fg = "#FFFFFF",
	footer_h = 48,
	footer_pad_h = 12,
	footer_pad_v = 8,

	-- Spalten
	col_left_w = 250,
	col_right_w = 230,
	col_spacing = 1,
	cols_pad_l = 2,
	cols_pad_r = 2,
	cols_pad_t = 2,
	cols_pad_b = 2,
	left_bg = "#FFFFFF",
	left_fg = "#000000",
	right_bg = "#D2E5FA",
	right_fg = "#000000",
	border_bg = "235CDB",

	-- Höhe
	total_height = 536,
}

local DEFAULTS_WIDGETS = {
	-- Grundfarben (Widgets)
	bg = "#235CDB",
	fg = "#FFFFFF",
	bg_focus = nil,

	-- Typografie/Icon
	font_family = "Sans",
	text_ratio = 0.25,
	text_ratio_header = nil,
	text_ratio_rows = nil,
	text_ratio_power = nil,
	text_size = nil,
	text_size_header = nil,
	text_size_rows = nil,
	text_size_power = nil,
	text_min = nil,
	text_max = nil,

	icon_ratio = 0.90,
	icon_ratio_header = nil,
	icon_ratio_rows = nil,
	icon_ratio_power = nil,
	avatar_size = nil,
	icon_size = nil,
	icon_size_header = nil,
	icon_size_rows = nil,
	icon_size_power = nil,

	-- Rows
	row_bg = "#FFFFFF",
	row_fg = "#000000",
	row_bg_hover = nil, -- wird abgeleitet
	row_h = 48,
	right_row_h = 40,
	row_pad_l = 10,
	row_pad_r = 10,
	row_pad_t = 4,
	row_pad_b = 4,
	row_spacing = 8,
	list_spacing = 0,

	-- Footer/Power (für Widgets, falls genutzt)
	footer_bg = "#235CDB",
	footer_fg = "#FFFFFF",
	power_bg = nil,
	power_fg = nil,
	power_bg_hover = nil,
	power_w = 110,
	power_h = 48,
	power_pad_l = 10,
	power_pad_r = 10,
	power_pad_t = 4,
	power_pad_b = 4,
	power_spacing = 6,
	power_icon_size = nil,
	power_bar_spacing = 0,
}

-- Vereinheitlichte Defaults (Container hat Vorrang vor Widgets an den gemeinsamen Keys)
local DEFAULTS = merge(DEFAULTS_WIDGETS, DEFAULTS_CONTAINER)

local function normalize(t)
	-- Ableitungen/Kompatibilität
	t.bg_focus = t.bg_focus or adjust(t.bg, -15)

	-- Basis-Hover (global, falls irgendwo verwendet – Rows bekommen seiten-spezifisch, s.u.)
	t.row_bg_hover = t.row_bg_hover or adjust(t.row_bg, -8)

	t.power_bg = t.power_bg or t.footer_bg or t.bg
	t.power_fg = t.power_fg or t.footer_fg or t.fg
	t.power_bg_hover = t.power_bg_hover or adjust(t.power_bg, -12)

	-- Seitenspezifische Row-Basisfarben
	t.left_row_bg = t.left_row_bg or t.left_bg
	t.left_row_fg = t.left_row_fg or t.left_fg
	t.right_row_bg = t.right_row_bg or t.right_bg
	t.right_row_fg = t.right_row_fg or t.right_fg

	-- Seitenspezifische Hoverfarben (KEIN Fallback mehr auf globales row_bg_hover)
	t.left_row_bg_hover = t.left_row_bg_hover or adjust(t.left_row_bg or t.left_bg or t.row_bg, -8)
	t.right_row_bg_hover = t.right_row_bg_hover or adjust(t.right_row_bg or t.right_bg or t.row_bg, -8)

	return t
end

-- ===== Öffentliche API =====
function Theme.with_defaults(t)
	t = merge(DEFAULTS, t or {})
	return normalize(t)
end

function Theme.get(overrides)
	return Theme.with_defaults(overrides or {})
end

-- Größen-Resolver (kompatibel zu widgets/theme.lua)
function Theme.resolve_text_size_number(t, eff_h, kind)
	if kind == "header" and t.text_size_header then
		return t.text_size_header
	end
	if kind == "rows" and t.text_size_rows then
		return t.text_size_rows
	end
	if kind == "power" and t.text_size_power then
		return t.text_size_power
	end
	if t.text_size then
		return t.text_size
	end

	local ratio = (kind == "header" and t.text_ratio_header)
		or (kind == "rows" and t.text_ratio_rows)
		or (kind == "power" and t.text_ratio_power)
		or t.text_ratio
		or DEFAULTS.text_ratio

	local sz = math.max(1, math.floor(eff_h * ratio + 0.5))
	if t.text_min then
		sz = math.max(sz, t.text_min)
	end
	if t.text_max then
		sz = math.min(sz, t.text_max)
	end
	return sz
end

function Theme.resolve_font(t, eff_h, kind)
	local family = t.font_family or "Sans"
	local size = Theme.resolve_text_size_number(t, eff_h, kind)
	return family .. " " .. tostring(size)
end

function Theme.resolve_icon_size(t, eff_h, kind)
	if kind == "header" then
		if t.icon_size_header then
			return t.icon_size_header
		end
		if t.avatar_size then
			return t.avatar_size
		end
	elseif kind == "rows" and t.icon_size_rows then
		return t.icon_size_rows
	elseif kind == "power" then
		if t.icon_size_power then
			return t.icon_size_power
		end
		if t.power_icon_size then
			return t.power_icon_size
		end
	end
	if t.icon_size then
		return t.icon_size
	end

	local ratio = (kind == "header" and t.icon_ratio_header)
		or (kind == "rows" and t.icon_ratio_rows)
		or (kind == "power" and t.icon_ratio_power)
		or t.icon_ratio
		or DEFAULTS.icon_ratio

	return math.max(1, math.floor(eff_h * ratio + 0.5))
end

function Theme.row_colors(t, side)
	local left = (side ~= "right")
	local bg = left and (t.left_row_bg or t.left_bg or t.row_bg) or (t.right_row_bg or t.right_bg or t.row_bg)
	local fg = left and (t.left_row_fg or t.left_fg or t.row_fg) or (t.right_row_fg or t.right_fg or t.row_fg)
	local hover = left and (t.left_row_bg_hover or adjust(bg, -8)) or (t.right_row_bg_hover or adjust(bg, -8))
	return { bg = bg, fg = fg, hover = hover }
end

-- Utils exportieren (für alten widgets/theme-Code)
Theme.adjust = adjust

return Theme
