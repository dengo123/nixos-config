local M = {}

-- ===== Colors / Utils =====
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
function M.adjust(hex, pct)
	local r, g, b = hex_to_rgb(hex)
	local f = 1 + (pct / 100)
	return rgb_to_hex(r * f, g * f, b * f)
end

-- ===== Defaults (einheitliche Keys) =====
M.DEFAULTS = {
	-- Grundfarben
	bg = "#235CDB",
	fg = "#FFFFFF",
	bg_focus = nil,

	-- Globale Typografie/Icon Ratios (overridebar: *_header/_rows/_power)
	font_family = "Sans",
	text_ratio = 0.22,
	text_ratio_header = nil,
	text_ratio_rows = nil,
	text_ratio_power = nil,
	text_size = nil,
	text_size_header = nil,
	text_size_rows = nil,
	text_size_power = nil,
	text_min = nil,
	text_max = nil,

	icon_ratio = 0.60,
	icon_ratio_header = nil,
	icon_ratio_rows = nil,
	icon_ratio_power = nil,
	avatar_size = nil,
	icon_size = nil,
	icon_size_header = nil,
	icon_size_rows = nil,
	icon_size_power = nil,

	-- Header
	header_h = 64,
	header_bg = nil,
	header_fg = nil,
	header_pad_l = 10,
	header_pad_r = 10,
	header_pad_t = 8,
	header_pad_b = 8,
	header_spacing = 10,
	header_text_spacing = 2,
	avatar_radius = 8,

	-- Rows (Columns)
	row_bg = "#FFFFFF",
	row_fg = "#000000",
	row_bg_hover = nil,
	row_h = 48,
	row_pad_l = 10,
	row_pad_r = 10,
	row_pad_t = 4,
	row_pad_b = 4,
	row_spacing = 8,
	list_spacing = 0,

	-- Footer / Power
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

function M.with_defaults(t)
	t = t or {}
	for k, v in pairs(M.DEFAULTS) do
		if t[k] == nil then
			t[k] = v
		end
	end
	t.power_bg = t.power_bg or t.footer_bg or t.bg
	t.power_fg = t.power_fg or t.footer_fg or t.fg
	t.bg_focus = t.bg_focus or M.adjust(t.bg, -15)
	t.row_bg_hover = t.row_bg_hover or M.adjust(t.row_bg, -8)
	t.power_bg_hover = t.power_bg_hover or M.adjust(t.power_bg, -12)
	return t
end

-- ===== Größen-Resolver (Ratio > Absolute > Fallback) =====
function M.resolve_text_size_number(t, eff_h, kind)
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

	local ratio = t.text_ratio
	if kind == "header" and t.text_ratio_header then
		ratio = t.text_ratio_header
	end
	if kind == "rows" and t.text_ratio_rows then
		ratio = t.text_ratio_rows
	end
	if kind == "power" and t.text_ratio_power then
		ratio = t.text_ratio_power
	end
	ratio = ratio or M.DEFAULTS.text_ratio

	local sz = math.max(1, math.floor(eff_h * ratio + 0.5))
	if t.text_min then
		sz = math.max(sz, t.text_min)
	end
	if t.text_max then
		sz = math.min(sz, t.text_max)
	end
	return sz
end

function M.resolve_font(t, eff_h, kind)
	local family = t.font_family or "Sans"
	local size = M.resolve_text_size_number(t, eff_h, kind)
	return family .. " " .. tostring(size)
end

function M.resolve_icon_size(t, eff_h, kind)
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

	local ratio = t.icon_ratio
	if kind == "header" and t.icon_ratio_header then
		ratio = t.icon_ratio_header
	end
	if kind == "rows" and t.icon_ratio_rows then
		ratio = t.icon_ratio_rows
	end
	if kind == "power" and t.icon_ratio_power then
		ratio = t.icon_ratio_power
	end
	ratio = ratio or M.DEFAULTS.icon_ratio
	return math.max(1, math.floor(eff_h * ratio + 0.5))
end

return M
