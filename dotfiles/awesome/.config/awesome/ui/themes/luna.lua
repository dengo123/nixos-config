-- ~/.config/awesome/ui/themes/luna.lua
local M = {}

function M.get()
	return {
		name = "luna",

		colors = {
			primary = "#235CDB",
			secondary = "#0B89E7",
			tertiary = "#1A50B8",

			start = "#13B12D",
			start_focus = "#11A229",

			close = "#E5210B",
			close_focus = "#D21E0A",

			surface = "#F0EAD6",
			surface_focus = "#E6DCBB",

			white = "#FFFFFF",
			black = "#000000",
			gray = "#DDDDDD",
		},

		fonts = {
			ui = "Sans 10",
			ui_bold = "Sans Bold 10",
			ui_italic = "Sans Italic 10",
			ui_bold_italic = "Sans Bold Italic 10",
		},

		icons = {
			system = "ui/assets/flake.png",
		},

		wallpaper = {
			source = "ui/assets/bliss2d.png",
		},
	}
end

return M
