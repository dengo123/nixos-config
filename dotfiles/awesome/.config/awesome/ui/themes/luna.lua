-- ~/.config/awesome/ui/themes/luna.lua
local M = {}

function M.get()
	return {
		name = "luna",

		palette = {
			blue = "#235CDB",
			blue_dark = "#1A50B8",

			teal = "#0B89E7",
			teal_dark = "#086FBA",

			green = "#13B12D",
			green_dark = "#11A229",

			red = "#E5210B",
			red_dark = "#D21E0A",

			cream = "#F0EAD6",
			cream_dark = "#E6DCBB",

			orange = "#F39C12",
			orange_dark = "#D68910",

			yellow = "#E4BE26",
			yellow_dark = "#ECAD1C",

			purple = "#8E44AD",
			purple_dark = "#6C3483",

			white = "#FFFFFF",
			black = "#000000",
			gray = "#DDDDDD",
		},

		roles = {
			primary = "blue",
			secondary = "teal",
			tertiary = "blue_dark",

			start = "green",
			start_focus = "green_dark",

			close = "red",
			close_focus = "red_dark",

			surface = "cream",
			surface_focus = "cream_dark",

			foreground = "black",
			background = "white",

			text = "black",
			text_invert = "white",
		},

		fonts = {
			ui = "Sans 10",
			ui_bold = "Sans Bold 10",
			ui_italic = "Sans Italic 10",
			ui_bold_italic = "Sans Bold Italic 10",
		},

		icons = {
			system = "ui/assets/flake.png",
			run = "ui/assets/Run_2001.png",
		},

		wallpaper = {
			source = "ui/assets/bliss2d.png",
		},
	}
end

return M
