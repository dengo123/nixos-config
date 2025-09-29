-- features/shell/menu/dialogs/theme.lua
-- Dialog-Theme (unabhängig von beautiful); liefert alle Keys, die rows/columns erwarten.

local Theme = {}

-- Versuche Utilities (adjust, etc.) aus dem Menü-Theme zu nutzen
local Styler_ok, Styler = pcall(require, "features.shell.menu.lib.theme")

Theme.defaults = {
	-- Geometrie
	-- dialog_w = 560,
	-- dialog_h = 360,
	dialog_radius = 0,
	dialog_border_width = 1,
	dialog_border = "#053193",

	header_ratio = 0.22,
	footer_ratio = 0.22,
	header_h = 80,
	footer_h = 80,

	-- Flächen / Farben (Dialog)
	header_bg = "#053193",
	header_fg = "#FFFFFF",
	body_bg = "#617FD9",
	body_fg = "#000000",
	footer_bg = "#053193",
	footer_fg = "#FFFFFF",

	-- Popup/Backdrop
	dialog_bg = "#053193",
	backdrop = "#00000066",

	-- Header Typo/Icon
	header_font_size = 18,
	header_icon = " XP",
	header_icon_size = 20,
	header_icon_path = "",

	-- Pads
	pad_h = 16,
	pad_v = 14,

	-- Icon/Action Cards
	icon_ratio = 0.16,
	icon_pad = 6,
	icon_cell_pad = 6,
	icon_cell_extra_w = 56,
	icon_spacing = 12,
	icon_label_size = 12,
	icon_label_leading = 1.25,
	icon_label_lines = 1,
	icon_label_color = "#FFFFFF",
	icon_shape = "rounded",
	icon_rounding = 10,
	icon_hover_bg = "#FFFFFF22",
	icon_hover_border = "#2B77FF",
	icon_hover_bw = 2,

	-- Footer / Cancel
	cancel_bg = "#F5F5EE",
	cancel_fg = "#000000",
	cancel_pad_h = 10,
	cancel_pad_v = 4,
	cancel_radius = 2,
	cancel_hover_bg = "#F5F5EE",
	cancel_hover_border = "#2B77FF",
	cancel_hover_bw = 2,
	-- cancel_width = 120,

	----------------------------------------------------------------
	-- WICHTIG: Keys, die rows.lua / columns.lua erwarten
	-- (wenn du sie nicht überschreibst, werden sie aus body_* abgeleitet)
	----------------------------------------------------------------
	row_bg = nil, -- default: body_bg
	row_fg = nil, -- default: body_fg
	left_bg = nil, -- default: row_bg
	left_fg = nil, -- default: row_fg
	right_bg = nil, -- default: row_bg
	right_fg = nil, -- default: row_fg
	row_h = 48, -- Standard-Zeilenhöhe

	-- Optional: explizite Hover-Farbe; wenn nil, wird sie in Theme.get via adjust(-8) abgeleitet
	row_bg_hover = nil,
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

function Theme.merge(a, b)
	return merge(a, b)
end

function Theme.get(overrides)
	local t = merge(Theme.defaults, overrides or {})

	-- Ableitungen: falls nicht explizit gesetzt, nutze body_* als Row-/Spaltenbasis
	t.row_bg = t.row_bg or t.body_bg
	t.row_fg = t.row_fg or t.body_fg

	t.left_bg = t.left_bg or t.row_bg
	t.left_fg = t.left_fg or t.row_fg
	t.right_bg = t.right_bg or t.row_bg
	t.right_fg = t.right_fg or t.row_fg

	-- >>> Hover-Farbe zuverlässig ableiten (bevorzugt über Styler.adjust(..., -8))
	if not t.row_bg_hover then
		local base = t.row_bg or t.body_bg or "#00000000"
		if Styler_ok and type(Styler.adjust) == "function" then
			t.row_bg_hover = Styler.adjust(base, -8)
		else
			-- Fallback: minimal dunkler via Alpha-Overlay (subtil)
			t.row_bg_hover = base
		end
	end

	-- Kennzeichne als ¿roh¿, damit widgets/rows.lua NICHT erneut normalisiert.
	t.__raw_theme = true
	return t
end

return Theme
