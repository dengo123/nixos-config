-- features/shell/menu/search/theme.lua
-- Liefert alle Style-Werte für die Searchbar und erlaubt Overrides.

local Theme = {}

local DEFAULTS = {
	-- Pille/Textfeld
	bg = "#FFFFFF",
	fg = "#000000",
	cursor = "#000000",
	hit_w = 180, -- collapsed Breite
	expand_w = 180, -- expanded Breite
}

-- shared: gesamtes Menü-Theme (t aus deinem Footer)
-- overrides: spezifische Search-Overrides
function Theme.resolve(shared, overrides)
	shared = shared or {}
	overrides = overrides or {}

	-- Aus globalem Theme lesen (falls vorhanden), sonst Defaults
	local T = {
		bg = shared.search_bg or shared.left_bg or DEFAULTS.bg,
		fg = shared.search_fg or shared.left_fg or DEFAULTS.fg,
		cursor = shared.search_cursor or DEFAULTS.cursor,
		hit_w = shared.search_hit_w or DEFAULTS.hit_w,
		expand_w = shared.search_w or DEFAULTS.expand_w,
	}

	-- gezielte Overrides drüberlegen
	for k, v in pairs(overrides) do
		T[k] = v
	end
	return T
end

return Theme
