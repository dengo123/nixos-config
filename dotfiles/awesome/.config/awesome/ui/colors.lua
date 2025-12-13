-- ~/.config/awesome/ui/colors.lua
local M = {}

-- Zentrale Palette. Einfache Namen = "#HEX"
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
	overlay_40 = "#00000066",

	-- Accents
	red = "#FF6FA3",
	pink = "#FF8FBA",
	green = "#27AE60",
	green_dark = "#1F8F4A",

	-- Extras
	creme = "#E4E1D1",
	creme_focus = "#F2E7CF",
}

function M.get()
	local t = {}
	for k, v in pairs(PALETTE) do
		t[k] = v
	end
	return t
end

function M.export_globals(enable)
	if not enable then
		return
	end
	for k, v in pairs(PALETTE) do
		_G[k] = v
	end
end

return M
