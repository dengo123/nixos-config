-- features/shell/menu/dialogs/theme.lua
-- Lokales Dialog-Theme (keine beautiful-Abhängigkeit)
-- API:
--   Theme.get(overrides) -> Tabelle (Defaults ⟵ overrides)
--   Theme.merge(a, b)    -> flaches Merge (helper, falls du’s brauchst)

local Theme = {}

Theme.defaults = {
	-- Geometrie
	dialog_w = 560,
	dialog_h = 360,
	radius = 12,

	-- Flächen/Farben
	header_bg = "#053193",
	header_fg = "#FFFFFF",
	body_bg = "#617FD9",
	body_fg = "#000000",
	footer_bg = "#053193",
	footer_fg = "#FFFFFF",

	-- Container
	dialog_bg = "#00000000", -- transparent (Popup-Container)
	backdrop = "#00000066", -- halbtransparentes Schwarz

	-- Abstände
	pad_h = 16,
	pad_v = 14,

	-- Footer/Cancel
	cancel_bg = "#F5F5EE",
	cancel_fg = "#000000",
	cancel_hover_bg = "#F5F5EE",
	cancel_hover_border = "#607CD9",

	-- Icon-Hover (falls später wieder Icons rein kommen)
	icon_hover_bg = "#FFFFFF20",
	icon_hover_border = "#2B5B88",
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
