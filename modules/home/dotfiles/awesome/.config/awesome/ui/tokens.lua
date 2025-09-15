-- features/ui/tokens.lua
local gears = require("gears")

return {
	palette = {
		bg = "#ECE9D8", -- XP: Fenster-Hintergrund
		fg = "#0A0A0A",
		surface = "#F7F4EA",
		surface2 = "#DFDFDF",
		accent = "#0A64AD", -- XP Blau
		accentFg = "#FFFFFF",
		muted = "#606060",
		danger = "#B22222",
	},

	fonts = {
		ui = "Tahoma 10",
		mono = "JetBrainsMono Nerd Font 10",
	},

	radii = { sm = 4, md = 8, lg = 12, pill = 999 },

	spacing = { xs = 4, sm = 8, md = 12, lg = 16, xl = 24 },

	-- Komponentenspezifische Defaults (Layout-Entscheidungen zentral!)
	comp = {
		footer = {
			h = 40,
			pad_t = 6,
			pad_b = 6,
			pad_l = 10,
			pad_r = 10,
			power_group_spacing = 8,
			search = {
				hit_w = 200,
				w = 240,
				h_ratio = 0.5,
				bg = nil, -- nil ⇒ transparent im collapsed state
				fg = nil, -- nil ⇒ palette.fg
				cursor = "#000000",
				radius = 8,
			},
		},
	},

	shapes = {
		rounded = function(radius)
			radius = radius or 8
			return function(cr, w, h)
				gears.shape.rounded_rect(cr, w, h, radius)
			end
		end,
	},

	assets = {
		-- z.B. Pfade zu Icons, Avataren, Sounds
		-- power_icons = "/…/icons/power/",
	},
}
