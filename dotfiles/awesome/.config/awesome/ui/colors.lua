-- ~/.config/awesome/ui/colors.lua
local M = {}

-- Zentrale Palette. Einfache Namen = "#HEX"
-- Fehlende Farben (blue_dark, green, red, pink, gray) füllen wir,
-- sobald sie in den nächsten Modulen auftauchen.
local PALETTE = {
	-- Blues
	blue_luna = "#235CDB",
	blue_light = "#0B89E7",
	blue_dark = "#1A50B8",

	-- Neutrals
	white = "#FFFFFF",
	black = "#000000",
	gray = "#DDDDDD",
	transparent = "#00000000",

	-- Accents
	red = "#FF6FA3",
	pink = "#FF8FBA",

	-- Extras
	creme = "#FFF7E6",
	creme_focus = "#F2E7CF",
}

-- Immer eine Kopie zurückgeben, damit nichts versehentlich mutiert wird.
function M.get()
	local t = {}
	for k, v in pairs(PALETTE) do
		t[k] = v
	end
	return t
end

-- Optional: Globale Exporte, falls du wirklich `beautiful.wibar_fg = white`
-- OHNE Namespace schreiben willst. Standard: aus.
function M.export_globals(enable)
	if not enable then
		return
	end
	for k, v in pairs(PALETTE) do
		_G[k] = v
	end
end

return M
