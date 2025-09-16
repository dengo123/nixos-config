-- features/shell/menu/dialogs/theme.lua
-- Lokales Dialog-Theme (keine beautiful-Abhängigkeit)
-- API:
--   Theme.get(overrides) -> Tabelle (Defaults ¿ overrides)
--   Theme.merge(a, b)    -> flaches Merge (Helper)

local Theme = {}

Theme.defaults = {
	--------------------------------------------------------------------------
	-- Geometrie & Layout-Ratios
	--------------------------------------------------------------------------
	dialog_w = 560,
	dialog_h = 360,
	radius = 12,

	-- Statt starrem 1/5-1/5-3/5 nutzen wir feinere Ratios (XP-Feeling)
	header_ratio = 0.22, -- ~18% der Gesamthöhe
	footer_ratio = 0.22, -- ~18% der Gesamthöhe
	-- Body ergibt sich automatisch = 1 - header_ratio - footer_ratio

	--------------------------------------------------------------------------
	-- Farben/Flächen
	--------------------------------------------------------------------------
	header_bg = "#053193",
	header_fg = "#FFFFFF",
	body_bg = "#617FD9", -- sichtbar (XP-Blau). Für transparent: "#00000000"
	body_fg = "#000000",
	footer_bg = "#053193",
	footer_fg = "#FFFFFF",

	-- Popup-Container/Backdrop
	dialog_bg = "#00000000", -- transparenter Container (um den Block herum)
	backdrop = "#00000066", -- halbtransparentes Schwarz hinter dem Dialog

	-- Header Typo/Icon
	header_font_size = 18,
	header_icon = " XP", -- Emoji
	header_icon_size = 20,
	header_icon_path = "", -- PNG/SVG Pfad, z.B. "/home/user/.config/awesome/assets/logo.png"

	--------------------------------------------------------------------------
	-- Innenabstände (Body)
	--------------------------------------------------------------------------
	pad_h = 16,
	pad_v = 14,

	--------------------------------------------------------------------------
	-- Icon/Action Cards (zentralisiert)
	--------------------------------------------------------------------------
	-- Größensteuerung
	icon_ratio = 0.16, -- Anteil von H_BODY für ICON_SIZE
	icon_pad = 6, -- Innenpad im Icon-Quadrat
	icon_cell_pad = 6, -- Außenpad der gesamten Klick-Zelle
	icon_cell_extra_w = 56, -- horizontale Zugabe für saubere Spalten
	icon_spacing = 12, -- Abstand Icon ¿ Label (vertikal)

	-- Label-Typo
	icon_label_size = 10, -- pt
	icon_label_leading = 6, -- Zeilenhöhe-Faktor
	icon_label_lines = 1, -- 1 Zeile erzwingen
	icon_label_color = "#FFFFFF",

	-- Icon-Form/Hover
	icon_shape = "rounded", -- "rect" | "rounded"
	icon_rounding = 4, -- px (nur für "rounded" relevant)
	icon_hover_bg = "#FFFFFF22",
	icon_hover_border = "#2B77FF",
	icon_hover_bw = 2,

	--------------------------------------------------------------------------
	-- Footer / Cancel-Button
	--------------------------------------------------------------------------
	cancel_bg = "#F5F5EE",
	cancel_fg = "#000000",
	cancel_pad_h = 10,
	cancel_pad_v = 4,
	cancel_radius = 2,
	cancel_hover_bg = "#F5F5EE",
	cancel_hover_border = "#2B77FF",
	cancel_hover_bw = 2,
}

function Theme.merge(a, b)
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
	return Theme.merge(Theme.defaults, overrides)
end

return Theme
