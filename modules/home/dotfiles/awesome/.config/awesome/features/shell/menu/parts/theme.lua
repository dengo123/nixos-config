-- features/shell/menu/parts/theme.lua
local Theme = {}

local DEFAULTS = {
	-- Grundfarben
	bg = "#222222",
	fg = "#FFFFFF",

	-- Popup / Rahmen
	popup_radius = 12,
	popup_border_width = 1,
	popup_border_color = "#3A6EA5",
	popup_bg = "#00000000", -- äußerer Container (meist transparent)
	dialog_bg = "#235CDB", -- Innenfläche (Columns-Hintergrund)

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

	-- Rows / Icons
	row_h = 48,
	list_spacing = 0,
	icon_size = 18,
	row_bg = "#FFFFFF",
	row_fg = "#000000",
	hover_bg = "#FFFFFF22",
	hover_border = "#2B77FF",
	hover_bw = 2,

	-- Gesamthöhe (Columns/popup nutzen das)
	total_height = 650,
}

local function merge(a, b)
	local out = {}
	for k, v in pairs(a or {}) do
		out[k] = v
	end
	for k, v in pairs(b or {}) do
		out[k] = v
	end
	return out
end

function Theme.get(overrides)
	return merge(DEFAULTS, overrides or {})
end

-- Beispiel-Preset
function Theme.luna_xp(overrides)
	return merge(
		Theme.get({
			dialog_bg = "#235CDB",
			header_bg = "#235CDB",
			footer_bg = "#235CDB",
			left_bg = "#FFFFFF",
			right_bg = "#D2E5FA",
			popup_border_color = "#3A6EA5",
		}),
		overrides or {}
	)
end

return Theme
